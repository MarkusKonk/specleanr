import logging
import subprocess
import json
import os
import requests
from urllib.parse import urlparse
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

'''

curl --location 'http://localhost:5000/processes/multidetect-and-clean/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://localhost/download/out/multiprecleaned.csv",
        "colname_variable": "bio6",
        "multiple_species": true,
        "colname_exclude": "x,y",
        "methods": "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel, iforest, lof, mahal",
        "silence_true_errors": true,
        "missingness": 0.1,
        "threshold": 0.7,
        "colname_species": "species"
    }
}'
'''

LOGGER = logging.getLogger(__name__)

script_title_and_path = __file__
metadata_title_and_path = script_title_and_path.replace('.py', '.json')
PROCESS_METADATA = json.load(open(metadata_title_and_path))

class MultiDetectProcessor(BaseProcessor):

    def __init__(self, processor_def):
        super().__init__(processor_def, PROCESS_METADATA)
        self.supports_outputs = True
        self.job_id = 'job-id-not-set'
        self.r_script = 'multidetect.R'
        self.image_name = 'specleanr:20250410'

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
        return f'<MultiDetectProcessor> {self.name}'

    def execute(self, data, outputs=None):

        # Get user inputs
        in_data_url = data.get('input_data')
        in_colname_var = data.get('colname_variable')
        in_bool_multiple_species = data.get('multiple_species')
        in_colname_species = data.get('colname_species', 'not_provided')
        in_colname_exclude = data.get('colname_exclude')
        in_methods = data.get('methods')
        in_silence_true_errors = data.get('silence_true_errors')
        in_missingness = data.get('missingness')
        in_threshold = data.get('threshold')

        # Checks
        if in_data_url is None:
            raise ProcessorExecuteError('Missing parameter "input_data". Please provide a URL to your input table.')
        if in_colname_var is None:
            raise ProcessorExecuteError('Missing parameter "colname_variable". Please provide a column name.')
        if in_bool_multiple_species is None:
            raise ProcessorExecuteError('Missing parameter "multiple_species". Please provide \"true\" or \"false\".')
        if in_colname_exclude is None:
            raise ProcessorExecuteError('Missing parameter "colname_exclude". Please provide a column name.')
        if in_methods is None:
            raise ProcessorExecuteError('Missing parameter "methods". Please provide a value.')
        if in_silence_true_errors is None:
            raise ProcessorExecuteError('Missing parameter "silence_true_errors". Please provide \"true\" or \"false\".')
        if in_missingness is None:
            raise ProcessorExecuteError('Missing parameter "missingness". Please provide a value.')

        # From booleans to string:
        in_bool_multiple_species = 'true' if in_bool_multiple_species else 'false'
        in_silence_true_errors = 'true' if in_silence_true_errors else 'false'

        # Set null threshold:
        if in_threshold is None:
            in_threshold = 'null'

        # Input files passed by user:
        input_dir = self.download_dir+'/in/job_%s' % self.job_id
        input_csv_path = download_any_file(in_data_url, input_dir, ".csv")

        # Where to store output data
        result_filename = 'cleaned_data-%s.csv' % self.job_id
        result_filepath     = self.download_dir+'/out/'+result_filename
        result_downloadlink = self.download_url+'/out/'+result_filename

        # Assemble args for R script:
        r_args = [
            input_csv_path,
            in_colname_var,
            in_bool_multiple_species,
            in_colname_exclude,
            in_methods,
            in_silence_true_errors,
            str(in_missingness),
            str(in_threshold),
            in_colname_species,
            result_filepath
        ]

        # Run the docker:
        returncode, stdout, stderr = run_docker_container(
            self.docker_executable,
            self.image_name,
            self.r_script,
            self.download_dir,
            r_args
        )

        if not returncode == 0:
            err_msg = 'Running docker container failed.'
            raise ProcessorExecuteError(user_msg = err_msg)

        # Return link to file:
        response_object = {
            "outputs": {
                "cleaned_data": {
                    "title": self.metadata['outputs']['cleaned_data']['title'],
                    "description": self.metadata['outputs']['cleaned_data']['description'],
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
        return result.returncode, stdout, stderr

    except subprocess.CalledProcessError as e:
        returncode = e.returncode
        stdout = e.stdout.decode()
        stderr = e.stderr.decode()
        LOGGER.error('Failed running docker container (exit code %s)' % returncode)
        for line in stderr.split('\n'):
            if line:
                LOGGER.error('Docker stderr: %s' % line)
        return returncode, stdout, stderr
