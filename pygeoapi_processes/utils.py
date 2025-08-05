import logging
import subprocess
import os

LOGGER = logging.getLogger(__name__)


def call_r_script(LOGGER, r_file_name, path_rscripts, r_args):

    # Call R script:
    r_file = path_rscripts.rstrip('/')+os.sep+r_file_name
    cmd = ["/usr/bin/Rscript", "--vanilla", r_file] + r_args
    LOGGER.debug('Running command %s ... (Output will be shown once finished)' % r_file_name)
    LOGGER.info(cmd)
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.PIPE)
    stdoutdata, stderrdata = p.communicate()
    LOGGER.debug("Done running command! Exit code from bash: %s" % p.returncode)

    # Retrieve stdout and stderr
    stdouttext = stdoutdata.decode()
    stderrtext = stderrdata.decode()

    # Remove empty lines:
    stderrtext_new = ''
    for line in stderrtext.split('\n'):
        if len(line.strip())==0:
            LOGGER.debug('Empty line!')
        else:
            LOGGER.debug('Non-empty line: %s' % line)
            stderrtext_new += line+'\n'

    # Remove empty lines:
    stdouttext_new = ''
    for line in stdouttext.split('\n'):
        if len(line.strip())==0:
            LOGGER.debug('Empty line!')
        else:
            LOGGER.debug('Non-empty line: %s' % line)
            stdouttext_new += line+'\n'

    stderrtext = stderrtext_new
    stdouttext = stdouttext_new

    # Format stderr/stdout for logging:
    if len(stderrdata) > 0:
        err_and_out = 'R stdout and stderr:\n___PROCESS OUTPUT {name} ___\n___stdout___\n{stdout}\n___stderr___\n{stderr}\n___END PROCESS OUTPUT {name} ___\n______________________'.format(
            name=r_file_name, stdout=stdouttext, stderr=stderrtext)
        LOGGER.error(err_and_out)
    else:
        err_and_out = 'R stdour:\n___PROCESS OUTPUT {name} ___\n___stdout___\n{stdout}\n___stderr___\n___(Nothing written to stderr)___\n___END PROCESS OUTPUT {name} ___\n______________________'.format(
            name=r_file_name, stdout=stdouttext)
        LOGGER.info(err_and_out)

    # Extract error message from R output, if applicable:
    err_msg = None
    if not p.returncode == 0:
        err_msg = 'R script "%s" failed.' % r_file_name
        for line in stderrtext.split('\n'):
            line = line.strip().lower()
            if line.startswith('error') or line.startswith('fatal') or 'error' in line:
                LOGGER.error('FOUND R ERROR LINE: %s' % line)
                err_msg += ' '+line.strip()
                LOGGER.error('ENTIRE R ERROR MSG NOW: %s' % err_msg)

    return p.returncode, stdouttext, stderrtext, err_msg




def run_docker_container(
        docker_executable,
        image_name,
        script_name,
        download_dir,
        script_args
    ):
    LOGGER.debug('Prepare running docker container')

    # Create container name
    # Note: Only [a-zA-Z0-9][a-zA-Z0-9_.-] are allowed
    # TODO: Use job-id?
    container_name = "%s_%s" % (image_name.split(':')[0], os.urandom(5).hex())

    # Define paths inside the container
    container_in = '/in'
    container_out = '/out'
    LOGGER.debug('Mounted dirs in/out, inside container:  %s, %s' % (container_in, container_out))

    # Define paths outside the container
    host_in = os.path.join(download_dir, "in")
    host_out = os.path.join(download_dir, "out")
    LOGGER.debug('Mounted dirs in/out, outside container: %s, %s' % (host_in, host_out))

    # Ensure directories exist
    os.makedirs(host_in, exist_ok=True)
    os.makedirs(host_out, exist_ok=True)

    # Replace paths in args:
    LOGGER.debug('Script args: %s' % script_args)
    sanitized_args = []
    for arg in script_args:
        newarg = arg
        if host_in in arg:
            newarg = arg.replace(host_in, container_in)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif host_out in arg:
            newarg = arg.replace(host_out, container_out)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif arg == 'None' or arg is None:
            # R scripts may be more familiar with receiving "null" than "None"
            # But they still have to parse them to a proper NULL data type.
            newarg = 'null'
        sanitized_args.append(newarg)

    # Prepare container command
    # (mount volumes etc.)
    docker_args = [
        docker_executable, "run",
        "--rm",
        "--name", container_name,
        "-v", f"{host_in}:{container_in}:ro",
        "-v", f"{host_out}:{container_out}",
        "-e", f"R_SCRIPT={script_name}",
        image_name,
    ]
    docker_command = docker_args + sanitized_args
    LOGGER.debug('Docker command: %s' % docker_command)
    
    # Run container
    try:
        LOGGER.debug('Start running docker container')
        result = subprocess.run(docker_command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout = result.stdout.decode()
        stderr = result.stderr.decode()
        LOGGER.debug('Finished running docker container')
        return result.returncode, stdout, stderr, "no error"

    except subprocess.CalledProcessError as e:
        returncode = e.returncode
        stdout = e.stdout.decode()
        stderr = e.stderr.decode()
        LOGGER.error('Failed running docker container (exit code %s)' % returncode)
        user_err_msg = get_error_message_from_docker_stderr(stderr)
        return returncode, stdout, stderr, user_err_msg


def get_error_message_from_docker_stderr(stderr, log_all_lines = True):
    '''
    We would like to return meaningful messages to users. For example, by
    printing ALL stderr lines, we get the following:

    ERROR - Docker stderr: Error in if (zz[which.max(zz)] < minpts) stop("All species do not have enough data after removing missing values and duplicates.") : 
    ERROR - Docker stderr:   argument is of length zero
    ERROR - Docker stderr: Calls: pred_extract
    ERROR - Docker stderr: Execution halted

    ERROR - Docker stderr: Error in pred_extract(data = speciesfiltered, raster = worldclim, lat = in_colname_lat,  : 
    ERROR - Docker stderr:   All species do not have enough data after removing missing values and duplicates.
    ERROR - Docker stderr: Execution halted

    Now, how to capture the meaningful part of that, which we want to return
    to the user? Here is a first attempt:
    '''

    user_err_msg = ""
    error_on_previous_line = False
    colon_on_previous_line = False
    for line in stderr.split('\n'):

        # Skip empty lines:
        if not line:
            continue

        # Print all non-empty lines to log:
        if log_all_lines:
            LOGGER.error('Docker stderr: %s' % line)

        # R error messages may start with the word "Error"
        if line.startswith("Error"):
            #LOGGER.debug('### Found explicit error line: %s' % line.strip())
            user_err_msg += line.strip()
            error_on_previous_line = True

        # When R error messages are continued on another line, they may be
        # indented by two spaces.
        elif line.startswith("  ") and error_on_previous_line:
            #LOGGER.debug('### Found indented line following an error: %s' % line.strip())
            user_err_msg += " "+line.strip()
            error_on_previous_line = True

        # When R error messages end with a colon, they will be continued on
        # the next line, independently of their indentation I guess!
        elif colon_on_previous_line and error_on_previous_line:
            #LOGGER.debug('### Found line following a colon: %s' % line.strip())
            user_err_msg += " "+line.strip()
            error_on_previous_line = True

        else:
            #LOGGER.debug('### Do not pass back to user: %s' % line.strip())
            error_on_previous_line = False

        # Remember whether this line ended with a colon, indicating that the
        # next line will continue with the error message:
        colon_on_previous_line = False
        if line.strip().endswith(":"):
            #LOGGER.debug('### Found a colon, next line will still be error!')
            colon_on_previous_line = True

    return user_err_msg



def run_docker_container_with_readonly(
        docker_executable,
        image_name,
        script_name,
        download_dir,
        readonly_dir,
        script_args
    ):
    LOGGER.debug('Prepare running docker container')

    # Create container name
    # Note: Only [a-zA-Z0-9][a-zA-Z0-9_.-] are allowed
    # TODO: Use job-id?
    container_name = "%s_%s" % (image_name.split(':')[0], os.urandom(5).hex())

    # Define paths inside the container
    container_in = '/in'
    container_out = '/out'
    container_readonly = '/readonly'
    LOGGER.debug('Mounted dirs in/out/readonly, inside container:  %s, %s, %s' % (container_in, container_out, container_readonly))

    # Define local paths
    host_in = os.path.join(download_dir, "in")
    host_out = os.path.join(download_dir, "out")
    host_readonly = readonly_dir.rstrip('/')
    LOGGER.debug('Mounted dirs in/out/readonly, outside container: %s, %s' % (host_in, host_out, host_readonly))

    # Ensure directories exist
    os.makedirs(host_in, exist_ok=True)
    os.makedirs(host_out, exist_ok=True)

    # Replace paths in args:
    LOGGER.debug('Script args: %s' % script_args)
    sanitized_args = []
    for arg in script_args:
        newarg = arg
        if host_in in arg:
            newarg = arg.replace(host_in, container_in)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif host_out in arg:
            newarg = arg.replace(host_out, container_out)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif host_readonly in arg:
            newarg = arg.replace(host_readonly, container_readonly)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif arg == 'None' or arg is None:
            # R scripts may be more familiar with receiving "null" than "None"
            # But they still have to parse them to a proper NULL data type.
            newarg = 'null'
        sanitized_args.append(newarg)

    # Prepare container command
    # (mount volumes etc.)
    docker_args = [
        docker_executable, "run",
        "--rm",
        "--name", container_name,
        "-v", f"{host_in}:{container_in}:ro",
        "-v", f"{host_out}:{container_out}",
        "-v", f"{readonly_dir}:{container_readonly}:ro",
        "-e", f"R_SCRIPT={script_name}",
        image_name,
    ]
    docker_command = docker_args + sanitized_args
    LOGGER.debug('Docker command: %s' % docker_command)

    # Run container
    try:
        LOGGER.debug('Start running docker container')
        result = subprocess.run(docker_command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout = result.stdout.decode()
        stderr = result.stderr.decode()
        LOGGER.debug('Finished running docker container')
        return result.returncode, stdout, stderr, "no error"

    except subprocess.CalledProcessError as e:
        returncode = e.returncode
        stdout = e.stdout.decode()
        stderr = e.stderr.decode()
        LOGGER.error('Failed running docker container (exit code %s)' % returncode)
        user_err_msg = get_error_message_from_docker_stderr(stderr)
        return returncode, stdout, stderr, user_err_msg
