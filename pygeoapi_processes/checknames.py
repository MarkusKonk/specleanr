import logging
import subprocess
import json
import os
import requests
import zipfile
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError
from pygeoapi.process.specleanr.pygeoapi_processes.utils import run_docker_container

'''
# Pass a csv containing species
# Works: Tested on 2025-08-05
curl --location 'http://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv",
        "colname_species": "species",
        "percent_correctness": 70,
        "bool_merge": true,
        "bool_synonym": true,
        "bool_ecosystem_type": true,
        "bool_rm_duplicates": true
    }
}'

# Pass a list of species
# Works: Tested on 2025-08-05
curl --location 'http://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "Alburnus alburnus, Esox lucius",
        "percent_correctness": 70,
        "bool_merge": false,
        "bool_synonym": true,
        "bool_ecosystem_type": true,
        "bool_rm_duplicates": true
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
        return f'<NameCheckProcessor> {self.name}'

    def execute(self, data, outputs=None):

        #################################
        ### Get user inputs and check ###
        #################################

        # In the order that the docker/R-script needs them:
        in_data_path           = data.get('input_data') # either one URL, or comma-separated list of strings (species names)
        in_colname_species     = data.get('colname_species', 'null') # OPTIONAL. just one string
        in_percent_correctness = data.get('percent_correctness') # number
        in_bool_merge          = data.get('bool_merge')
        in_bool_verbose = True # (no effect on client, so not defined by client)
        in_synonymn_checks     = data.get('bool_synonym')
        in_ecosystem_checks    = data.get('bool_ecosystem_type')
        in_rm_duplicates       = data.get('bool_rm_duplicates')

        # Checking for all mandatory input params:
        if in_data_path is None:
            raise ProcessorExecuteError('Missing parameter "input_data".')
        if in_percent_correctness is None:
            raise ProcessorExecuteError('Missing parameter "percent_correctness". Please provide a number.')
        if in_bool_merge is None:
            raise ProcessorExecuteError('Missing parameter "bool_merge". Please provide "true" or "false".')
        if in_synonymn_checks is None:
            raise ProcessorExecuteError('Missing parameter "bool_synonym". Please provide "true" or "false".')
        if in_ecosystem_checks is None:
            raise ProcessorExecuteError('Missing parameter "bool_ecosystem_type". Please provide "true" or "false".')
        if in_rm_duplicates is None:
            raise ProcessorExecuteError('Missing parameter "bool_rm_duplicates". Please provide "true" or "false".')

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
        result_filename = 'checked-biodiv-data-%s.csv' % self.job_id
        result_filepath     = output_dir+'/'+result_filename
        result_downloadlink = output_url+'/'+result_filename
        os.makedirs(output_dir, exist_ok=True)

        ##################################################
        ### Convert user inputs to what R script needs ###
        ##################################################

        # Nothing to do.
        # Note: If the booleans were not mandatory, then their value would be None,
        # so we would have to make sure to translate None to "False" or to "null",
        # or make sure the R script can deal with a string "None".

        ####################################
        ### Assemble args and run docker ###
        ####################################

        # Assemble args for R script:
        r_args = [
            in_data_path,
            in_colname_species,
            str(in_percent_correctness),
            str(in_bool_merge),
            str(in_bool_verbose),
            str(in_synonymn_checks),
            str(in_ecosystem_checks),
            str(in_rm_duplicates),
            result_filepath
        ]
        LOGGER.debug('r_args: %s' % r_args)

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
                "cleannames_df": {
                    "title": self.metadata['outputs']['cleannames_df']['title'],
                    "description": self.metadata['outputs']['cleannames_df']['description'],
                    "href": result_downloadlink
                }
            }
        }

        return 'application/json', response_object


