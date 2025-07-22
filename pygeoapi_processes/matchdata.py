import logging
import subprocess
import json
import os
import requests
import zipfile
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

'''
curl --location 'http://localhost:5000/processes/match-data/execution' \
--header 'Content-Type: application/json' \
--data '{ 
    "inputs": {
        "input_datasets": ["https://localhost/download/out/biodiv-data.csv", "https://localhost/referencedata/specleanr/efidata.csv"],
        "colnames_species_names": ["speciesname", "scientificName"],
        "colnames_countries": ["JDS4_sampling_ID"],
        "colnames_lat": ["lat", "latitude"],
        "colnames_lon": ["lon", "long", "longitude"],
        "colnames_date": ["Date", "sampling_date"],
        "verbose": true
    }
}'

curl --location 'http://localhost:5000/processes/match-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_datasets": ["https://exampleserver.com/exampledata/boku/jdsdata.csv",
                           "https://exampleserver.com/exampledata/boku/efidata.csv"],
        "colnames_species_names": ["speciesname", "scientificName"],
        "colnames_countries": ["JDS4_sampling_ID"],
        "colnames_lat": ["lat", "latitude"],
        "colnames_lon": ["lon", "long", "longitude"],
        "colnames_date": ["Date", "sampling_date"],
        "verbose": true
    }
}'


'''

LOGGER = logging.getLogger(__name__)

script_title_and_path = __file__
metadata_title_and_path = script_title_and_path.replace('.py', '.json')
PROCESS_METADATA = json.load(open(metadata_title_and_path))

class DataMatchProcessor(BaseProcessor):

    def __init__(self, processor_def):
        super().__init__(processor_def, PROCESS_METADATA)
        self.supports_outputs = True
        self.job_id = 'job-id-not-set'
        self.r_script = 'matchdata.R'
        self.image_name = 'specleanr:20250722'

        # Set config:
        config_file_path = os.environ.get('AQUAINFRA_CONFIG_FILE', "./config.json")
        with open(config_file_path, 'r') as config_file:
            config = json.load(config_file)
            self.download_dir = config["download_dir"].rstrip('/')
            self.download_url = config["download_url"].rstrip('/')
            self.docker_executable = config["docker_executable"]


    def set_job_id(self, job_id: str):
        self.job_id = job_id

    def __repr__(self):
        return f'<DataMatchProcessor> {self.name}'

    def execute(self, data, outputs=None):

        #################################
        ### Get user inputs and check ###
        #################################

        in_data_paths_or_urls = data.get('input_datasets')
        in_colnames_species_names = data.get('colnames_species_names')
        in_colnames_countries = data.get('colnames_countries')
        in_colnames_lat = data.get('colnames_lat')
        in_colnames_lon = data.get('colnames_lon')
        in_colnames_date = data.get('colnames_date') #appears in JSON file

        # Checks
        if in_data_paths_or_urls is None:
            raise ProcessorExecuteError('Missing parameter "input_datasets". Please provide URL(s) to your input csv file(s).')
        if in_colnames_species_names is None:
            raise ProcessorExecuteError('Missing parameter "colnames_species_names". Please provide a list of column names.')
        if in_colnames_countries is None:
            raise ProcessorExecuteError('Missing parameter "colnames_countries". Please provide a list of column names.')
        if in_colnames_lat is None:
            raise ProcessorExecuteError('Missing parameter "colnames_lat". Please provide a list of column names.')
        if in_colnames_lon is None:
            raise ProcessorExecuteError('Missing parameter "colnames_lon". Please provide a list of column names.')
        if in_colnames_date is None:
            raise ProcessorExecuteError('Missing parameter "colnames_date". Please provide a list of column names.')


        #################################
        ### Input and output          ###
        ### storage/download location ###
        #################################

        # Where to store output data
        result_filename = 'matched-biodiv-data-%s.csv' % self.job_id
        result_filepath     = self.download_dir+'/out/'+result_filename
        result_downloadlink = self.download_url+'/out/'+result_filename


        ##################################################
        ### Convert user inputs to what R script needs ###
        ##################################################

        # Make comma-separated string from lists:
        in_data_paths_or_urls = ','.join(in_data_paths_or_urls)
        in_colnames_species_names = ','.join(in_colnames_species_names)
        in_colnames_countries = ','.join(in_colnames_countries)
        in_colnames_lat = ','.join(in_colnames_lat)
        in_colnames_lon = ','.join(in_colnames_lon)
        in_colnames_date = ','.join(in_colnames_date)


        ####################################
        ### Assemble args and run docker ###
        ####################################
        in_verbose = True

        # Assemble args for R script:
        r_args = [
            in_data_paths_or_urls,
            in_colnames_species_names,
            in_colnames_countries,
            in_colnames_lat,
            in_colnames_lon,
            in_colnames_date,
            str(in_verbose),
            result_filepath
        ]

        ## Run the docker:
        returncode, stdout, stderr, user_err_msg = run_docker_container(
            self.docker_executable,
            self.image_name,
            self.r_script,
            self.download_dir,
            r_args
        )

        if not returncode == 0:
            user_err_msg = "no message" if len(user_err_msg) == 0 else user_err_msg
            err_msg = 'Running docker container failed: %s' % user_err_msg
            raise ProcessorExecuteError(user_msg = err_msg)

        # Return link to file:
        response_object = {
            "outputs": {
                "matched_biodiversity_data": {
                    "title": self.metadata['outputs']['matched_biodiversity_data']['title'],
                    "description": self.metadata['outputs']['matched_biodiversity_data']['description'],
                    "href": result_downloadlink
                }
            }
        }

        return 'application/json', response_object


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

    # Define local paths
    local_in = os.path.join(download_dir, "in")
    local_out = os.path.join(download_dir, "out")

    # Ensure directories exist
    os.makedirs(local_in, exist_ok=True)
    os.makedirs(local_out, exist_ok=True)

    # Replace paths in args:
    sanitized_args = []
    for arg in script_args:
        newarg = arg
        if local_in in arg:
            newarg = arg.replace(local_in, container_in)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        elif local_out in arg:
            newarg = arg.replace(local_out, container_out)
            LOGGER.debug("Replaced argument %s by %s..." % (arg, newarg))
        sanitized_args.append(newarg)

    # Prepare container command
    # (mount volumes etc.)
    docker_args = [
        docker_executable, "run",
        "--rm",
        "--name", container_name,
        "-v", f"{local_in}:{container_in}",
        "-v", f"{local_out}:{container_out}",
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
        elif colon_on_previous_line:
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
