import logging
import subprocess
import json
import os
import requests
import zipfile
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

'''
curl --location 'https://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{ 
    "inputs": {
        "input_data": "https://localhost:5000/download/out/matched-biodiv-data.csv",
        "colname_species": "species",
        "species_names": "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla",
        "pct": 70,
        "bool_merge": true
    }
}'

'''

LOGGER = logging.getLogger(__name__)

script_title_and_path = __file__
metadata_title_and_path = script_title_and_path.replace('.py', '.json')
PROCESS_METADATA = json.load(open(metadata_title_and_path))

class NameCheckProcessor(BaseProcessor):

    def __init__(self, processor_def):
        super().__init__(processor_def, PROCESS_METADATA)
        self.supports_outputs = True
        self.job_id = 'job-id-not-set'
        self.r_script = 'checknames.R'
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
        return f'<NameCheckProcessor> {self.name}'

    def execute(self, data, outputs=None):

        # Get user inputs
        input_data_url = data.get('input_data')
        colname_species = data.get('colname_species')
        species_names = data.get('species_names')
        pct = data.get('pct')
        bool_merge = data.get('bool_merge')

        # Checks
        if input_data_url is None:
            raise ProcessorExecuteError('Missing parameter "input_data". Please provide a URL to your input csv.')
        if colname_species is None:
            raise ProcessorExecuteError('Missing parameter "colname_species". Please provide a column name.')
        if species_names is None:
            raise ProcessorExecuteError('Missing parameter "species_names". Please provide a list of species names.')
        if pct is None:
            raise ProcessorExecuteError('Missing parameter "pct". Please provide a number.')
        if bool_merge is None:
            raise ProcessorExecuteError('Missing parameter "bool_merge". Please provide "true" or "false".')

        # From boolean to string:
        bool_merge = 'true' if bool_merge else 'false'

        # Input files passed by user:
        input_dir = self.download_dir+'/in/job_%s' % self.job_id
        input_data_path = download_any_file(input_data_url, input_dir, '.csv')

        # Where to store output data
        result_filename1 = 'checked-biodiv-data-%s.csv' % self.job_id
        result_filepath1     = self.download_dir+'/out/'+result_filename1
        result_downloadlink1 = self.download_url+'/out/'+result_filename1
        result_filename2 = 'filtered-biodiv-data-%s.csv' % self.job_id
        result_filepath2     = self.download_dir+'/out/'+result_filename2
        result_downloadlink2 = self.download_url+'/out/'+result_filename2

        # Assemble args for R script:
        r_args = [
            input_data_path,
            colname_species,
            species_names,
            str(pct),
            bool_merge,
            result_filepath1,
            result_filepath2
        ]

        ## Run the docker:
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
                "cleannames_df": {
                    "title": self.metadata['outputs']['cleannames_df']['title'],
                    "description": self.metadata['outputs']['cleannames_df']['description'],
                    "href": result_downloadlink1
                },
                "filtered_biodiversity_data": {
                    "title": self.metadata['outputs']['filtered_biodiversity_data']['title'],
                    "description": self.metadata['outputs']['filtered_biodiversity_data']['description'],
                    "href": result_downloadlink2
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
