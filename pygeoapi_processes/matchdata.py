import logging
import subprocess
import json
import os
import requests
import zipfile
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError
from pygeoapi.process.specleanr.pygeoapi_processes.utils import run_docker_container


'''
# Works: Tested on 2025-08-05 (Merret)
curl --location 'http://localhost:5000/processes/match-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_datasets": [
            "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/efidata.csv",
            "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv"
        ],
        "colnames_species_names": ["speciesname", "scientificName"],
        "colnames_countries": ["JDS4_sampling_ID"],
        "colnames_lat": ["lat", "latitude"],
        "colnames_lon": ["lon", "long", "longitude"],
        "colnames_date": ["Date", "sampling_date"]
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
        self.image_name = 'specleanr:20250805'

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

        # In the order that the docker/R-script needs them:
        in_data_paths_or_urls = data.get('input_datasets')
        in_colnames_species_names = data.get('colnames_species_names')
        in_colnames_countries = data.get('colnames_countries')
        in_colnames_lat = data.get('colnames_lat')
        in_colnames_lon = data.get('colnames_lon')
        in_colnames_date = data.get('colnames_date') #appears in JSON file
        in_verbose = True  # (no effect on client, so not defined by client)

        # Checking for all mandatory input params:
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

        # R refuses to send lists, if the list contains only one object, and just sends
        # the object instead. So I have to check whether the list really is a list:
        if type(in_data_paths_or_urls) == type("bla"):
            in_data_paths_or_urls = [in_data_paths_or_urls]

        if type(in_colnames_species_names) == type("bla"):
            in_colnames_species_names = [in_colnames_species_names]

        if type(in_colnames_countries) == type("bla"):
            in_colnames_countries = [in_colnames_countries]

        if type(in_colnames_lat) == type("bla"):
            in_colnames_lat = [in_colnames_lat]

        if type(in_colnames_lon) == type("bla"):
            in_colnames_lon = [in_colnames_lon]

        if type(in_colnames_date) == type("bla"):
            in_colnames_date = [in_colnames_date]

        #################################
        ### Input and output          ###
        ### storage/download location ###
        #################################

        # Where to store input data
        # Here, downloaded inputs will be stored by pygeoapi.
        # It will be mounted as read-only into the docker.
        #input_dir = self.download_dir+'/in/specleanr_job_%s' % self.job_id
        # This process does not need it, so set to None, so nothing will be created/mounted:
        input_dir = None

        # Where to store output data
        output_dir = os.path.join(self.download_dir, "out")
        output_dir = self.download_dir+'/out/specleanr_job_%s' % self.job_id
        output_url = self.download_url+'/out/specleanr_job_%s' % self.job_id
        result_filename = 'matched-biodiv-data-%s.csv' % self.job_id
        result_filepath     = output_dir+'/'+result_filename
        result_downloadlink = output_url+'/'+result_filename
        os.makedirs(output_dir, exist_ok=True)


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
            input_dir,
            output_dir,
            None,
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


