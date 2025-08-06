import logging
import subprocess
import json
import os

LOGGER = logging.getLogger(__name__)


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


def run_docker_container(
        docker_executable,
        image_name,
        script_name,
        input_dir_on_host,
        output_dir_on_host,
        readonly_dir_on_host,
        script_args
    ):
    LOGGER.debug('Prepare running docker container')

    # Create container name
    # Note: Only [a-zA-Z0-9][a-zA-Z0-9_.-] are allowed
    # TODO: Use job-id?
    container_name = "%s_%s" % (image_name.split(':')[0], os.urandom(5).hex())

    # Define paths inside the container
    container_out = '/out'
    container_in = '/in'
    container_readonly = '/readonly'
    LOGGER.debug('Mounted dirs /out,/in,/readonly, inside container:  %s, %s, %s' %
        (container_out, container_in, container_readonly))

    # Define paths outside the container
    host_in = input_dir_on_host
    host_out = output_dir_on_host
    host_readonly = readonly_dir_on_host
    LOGGER.debug('Mounted dirs /out,/in,/readonly, outside container: %s, %s, %s' %
        (host_out, host_in, host_readonly))

    # Replace paths in args, convert args to formats that can be passed to
    # docker and understood/parsed in R script inside docker:
    LOGGER.debug('Script args: %s' % script_args)
    sanitized_args = []
    for arg in script_args:
        newarg = arg
        if host_in is not None and host_in in arg:
            newarg = arg.replace(host_in, container_in)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif host_out is not None and host_out in arg:
            newarg = arg.replace(host_out, container_out)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif host_readonly is not None and host_readonly in arg:
            newarg = arg.replace(host_readonly, container_readonly)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif arg == 'None' or arg is None:
            # R scripts may be more familiar with receiving "null" than "None"
            # But they still have to parse them to a proper NULL data type.
            newarg = 'null'
        sanitized_args.append(newarg)

    # Prepare container command
    docker_args = [
        docker_executable, "run", "--rm",
        "--name", container_name
    ]
    # Add the mounts for three directories (-v) (ro and rw):
    if host_out is not None:
        docker_args = docker_args + ["-v", f"{host_out}:{container_out}:rw"]
    if host_in is not None:
        docker_args = docker_args + ["-v", f"{host_in}:{container_in}:ro"]
    if host_readonly is not None:
        docker_args = docker_args + ["-v", f"{host_readonly}:{container_readonly}:ro"]
    # Add the name of the script to be called (-e), and the name of the image
    docker_args = docker_args + [
        "-e", f"R_SCRIPT={script_name}",
        image_name
    ]
    # Add the arguments to be passed to the R script:
    docker_command = docker_args + sanitized_args
    LOGGER.debug('Docker command: %s' % docker_command)

    # Run container
    try:
        LOGGER.debug('Start running docker container')
        result = subprocess.run(docker_command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout = result.stdout.decode()
        stderr = result.stderr.decode()
        LOGGER.debug('Finished running docker container')

        # Print docker output:
        for line in stdout.split('\n'):
            if not line: continue
            LOGGER.debug('Docker stdout: %s' % line.strip())
            # output of print() in R-script
        for line in stderr.split('\n'):
            if not line: continue
            LOGGER.debug('Docker stderr: %s' % line.strip())
            # output of message() in R-script

        return result.returncode, stdout, stderr, "no error"

    except subprocess.CalledProcessError as e:
        returncode = e.returncode
        stdout = e.stdout.decode()
        stderr = e.stderr.decode()
        LOGGER.error('Failed running docker container (exit code %s)' % returncode)
        user_err_msg = get_error_message_from_docker_stderr(stderr)
        return returncode, stdout, stderr, user_err_msg


def download_geojson(input_url_geojson, input_dir, ending=None):
    # Not needed in getdata and pred_extract, as they now just pass on the URL.
    # But keeping it here, maybe needed at some point...

    # Download file into given dir:
    LOGGER.debug('Downloading input geojson file: %s' % input_url_geojson)
    resp = requests.get(input_url_geojson)
    if not resp.status_code == 200:
        raise ProcessorExecuteError('Could not download input geojson file (HTTP status %s): %s' % (resp.status_code, input_url_geojson))

    return store_geojson(resp.json(), input_dir, ending=ending)


def store_geojson(geojson, input_dir, ending=None):

    # Make sure the dir exists:
    if not os.path.exists(input_dir):
        os.makedirs(input_dir)

    # How should the downloaded file be named?
    # If the URL includes a name: TODO can we trust this name?
    #filename = os.path.basename(input_url_geojson)
    filename = "geojson%s" % os.urandom(5).hex()
    filename = filename if ending is None else filename+ending
    input_file_path = '%s/%s' % (input_dir, filename)
    LOGGER.debug('Storing input geojson file to: %s' % input_file_path)

    with open(input_file_path, 'w') as myfile:
        json.dump(geojson, myfile)

    return input_file_path



def call_r_script(LOGGER, r_file_name, path_rscripts, r_args):
    # Not needed anymore, as we call docker container now, not rscripts.
    # But keeping it here, maybe needed at some point...

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



### Likely not needed ###

def download_zipped_shapefile(input_url, input_dir):

    # Create a unique dir just for this download.
    # Why? We do not control what is in the unzipped zip, so we cannot
    # return the correct name if various files are mixed in the same dir!
    randomstring = os.urandom(5).hex()
    input_shp_dir = input_dir.rstrip('/')+'/zippedshp%s' % randomstring

    input_zipped_shp_path = download_any_file(input_url, input_shp_dir, '.zip')
    unzip_file(input_zipped_shp_path, input_shp_dir)

    # Find name of shapefile, which we dont control, as it is defined by whoever
    # zipped the zipfile:
    input_unzipped_shp_path = retrieve_file_name_by_ending(input_shp_dir, '.shp')
    return input_unzipped_shp_path


def download_any_file(input_url, input_dir, ending=None):

    # Make sure the dir exists:
    if not os.path.exists(input_dir):
        os.makedirs(input_dir)

    # Download file into given dir:
    LOGGER.debug('Downloading input file: %s' % input_url)
    resp = requests.get(input_url)
    if not resp.status_code == 200:
        raise ProcessorExecuteError('Could not download input file (HTTP status %s): %s' % (resp.status_code, input_url))

    # How should the downloaded file be named?
    # If the URL includes a name: TODO can we trust this name?
    #filename = os.path.basename(input_url)
    filename = "download%s" % os.urandom(5).hex()
    filename = filename if ending is None else filename+ending
    input_file_path = '%s/%s' % (input_dir, filename)
    LOGGER.debug('Storing input file to: %s' % input_file_path)

    with open(input_file_path, 'wb') as myfile:
        for chunk in resp.iter_content(chunk_size=1024):
            if chunk:
                myfile.write(chunk)

    return input_file_path


def unzip_file(input_zipped_file_path, unzip_dir):
    # TODO: See important secutiry warning here: https://docs.python.org/3/library/zipfile.html#zipfile.ZipFile.extractall

    # Note: Make sure unzip_dir is a custom dir just for this zipfile!
    # Why? We do not control what is in the unzipped zip, so we cannot
    # return the correct name if various files are mixed in the same dir!

    LOGGER.debug('Unzipping file "%s" to "%s"' % (input_zipped_file_path, unzip_dir))
    with zipfile.ZipFile(input_zipped_file_path, 'r') as zip_ref:
        zip_ref.extractall(unzip_dir)
        LOGGER.debug('Unzipping file... DONE.')


def retrieve_file_name_by_ending(input_dir, ending):

    # If user passed zipped inputs, we don't know the filename, so we can extract
    # it by its ending, if there are only one file with this ending (e.g. zipped shape).
    # DANGER: If there are several, we may return the wrong one!
    # TODO I am sure there is a better way!
    for filename in os.listdir(input_dir):
        if filename.endswith(ending):
            filepath = '%s/%s' % (input_dir, filename)
            LOGGER.debug('Name of %s file: %s' % (ending, filepath))
            return filepath

