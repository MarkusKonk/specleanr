import logging
import subprocess
import json
import os
import requests
from urllib.parse import urlparse
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError
from pygeoapi.process.specleanr.pygeoapi_processes.utils import run_docker_container

'''

## Pass all possible variables
## Works. Tested 2025-08-01
curl --location 'http://localhost:5000/processes/multidetect-and-clean/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv",
        "colname_variable": "Sepal.Length",
        "select_columns": null,
        "multiple_species": true,
        "output_type": "outlier",
        "group_colname": "Species",
        "colname_exclude": null,
        "methods": "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel",
        "silence_true_errors": false,
        "boot_run": false,
        "boot_maxrecords": 30,
        "number_of_boots": 5,
        "setseed": 1125,
        "boot_threshold": 0.6,
        "exceute_pca": true,
        "number_of_pca": 2,
        "pca_silence": true,
        "pcavariable": "PC1",
        "sdm_data": true,
        "inform_na_outlier": true,
        "missingness": 1.0,
        "bool_loess": true,
        "threshold_clean": null,
        "outlierweights_mode": "abs",
        "classifymode": "med",
        "eif_bool": false,
        "classify_or_autoremove": true
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
        return f'<MultiDetectProcessor> {self.name}'

    def execute(self, data, outputs=None):

        #################################
        ### Get user inputs and check ###
        #################################

        # Get user inputs
        # In the order that the docker/R-script needs them:
        in_data_path_or_url               = data.get('input_data') # Required
        in_var_ofinterest                 = data.get('colname_variable') # Required
        in_select_var                     = data.get('select_columns', 'null') # Optional
        in_bool_multiple_species          = data.get('multiple_species') # Required
        in_output_type                    = data.get('output_type', 'outlier') # OPTIONAL clean or outlier/ Defualt outliers
        in_group_colname                  = data.get('group_colname', 'null') # OPTIONAL (I think)
        in_colnames_exclude               = data.get('colname_exclude', 'null') # OPTIONAL
        in_methods                        = data.get('methods') # Required
        in_silence_true_errors            = data.get('silence_true_errors')
        in_boot_settings_run_bool         = data.get('boot_run')
        in_boot_settings_maxrecords       = data.get('boot_maxrecords', 30)
        in_boot_settings_nb               = data.get('number_of_boots', 10)
        in_boot_settings_seed             = data.get('setseed')
        in_boot_settings_threshold        = data.get('boot_threshold', 0.6)
        in_pca_settings_exec_bool         = data.get('exceute_pca') # Boolean
        in_pca_settings_npc               = data.get('number_of_pca', 5)
        in_pca_settings_quiet             = data.get('pca_silence', True)
        in_pca_settings_pcvar             = data.get('pcavariable', "PCA1")
        in_verbose_bool = True  # (no effect on client, so not defined by client)
        in_warn_bool = True  # (no effect on client, so not defined by client)
        in_sdm_bool                       = data.get('sdm_data', True) #multivaritate data, sdm must be TRUE
        in_na_inform_bool                 = data.get('inform_na_outlier', False)
        in_missingness                    = data.get('missingness', 0.1) # OPT
        in_bool_loess                     = data.get('bool_loess')
        in_threshold_clean                = data.get('threshold_clean', 0.8)
        in_mode_clean                     = data.get('outlierweights_mode', "abs")
        in_classifymode                   = data.get('classifymode', "med") #med
        in_eif_bool                       = data.get('eif_bool', False) #empirical influence function
        in_autoextract                    = data.get('classify_or_autoremove')
        #out_result_path # to be defined by this process.

        # Checking for all mandatory input params:

        if in_data_path_or_url is None:
            raise ProcessorExecuteError('Missing parameter "input_data". Please provide a URL to your input table.')
        if in_var_ofinterest is None:
            raise ProcessorExecuteError('Missing parameter "colname_variable". Please provide a column name.')
        # OPT in_select_var
        if in_bool_multiple_species is None:
            raise ProcessorExecuteError('Missing parameter "multiple_species". Please provide "true" or "false".')
        # OPT in_output_type
        # OPT in_group_colname
        if in_methods is None:
            raise ProcessorExecuteError('Missing parameter "methods". Please provide a value.')
        if in_silence_true_errors is None:
            raise ProcessorExecuteError('Missing parameter "silence_true_errors". Please provide "true" or "false".')
        if in_boot_settings_run_bool is None:
            raise ProcessorExecuteError('Missing parameter "boot_run". Please provide "true" or "false".')
        # OPT in_boot_settings_maxrecords
        # OPT in_boot_settings_nb
        if in_boot_settings_seed is None:
            raise ProcessorExecuteError('Missing parameter "setseed". Please provide a value.')
        # OPT in_boot_settings_threshold
        if in_pca_settings_exec_bool is None:
            raise ProcessorExecuteError('Missing parameter "exceute_pca". Please provide "true" or "false".')
        # OPT in_pca_settings_npc
        # OPT in_pca_settings_quiet
        # OPT in_pca_settings_pcvar
        # OPT in_sdm_bool
        # OPT in_na_inform_bool
        if in_bool_loess is None:
            raise ProcessorExecuteError('Missing parameter "bool_loess". Please provide "true" or "false".')
        # OPT in_threshold_clean
        # OPT in_mode_clean
        # OPT in_classifymode
        # OPT in_eif_bool
        if in_autoextract is None:
            raise ProcessorExecuteError('Missing parameter "classify_or_autoremove". Please provide "true" or "false".')


        #################################
        ### Input and output          ###
        ### storage/download location ###
        #################################

        # Where to store output data
        result_filename = 'cleaned-data-%s.csv' % self.job_id
        result_filepath     = self.download_dir+'/out/'+result_filename
        result_downloadlink = self.download_url+'/out/'+result_filename


        ##################################################
        ### Convert user inputs to what R script needs ###
        ##################################################

        # Nothing to do.

        # Note: If the booleans were not mandatory, then their value would be None,
        # so we would have to make sure to translate None to "False" or to "null",
        # or make sure the R script can deal with a string "None".
        # E.g. in_bool_multiple_species, in_silence_true_errors, in_boot_settings_run_bool,
        # in_pca_settings_exec_bool, in_sdm_bool, in_eif_bool etc.


        ####################################
        ### Assemble args and run docker ###
        ####################################

        # Assemble args for R script:
        r_args = [
            in_data_path_or_url,
            str(in_var_ofinterest),
            str(in_select_var),
            str(in_bool_multiple_species),
            str(in_output_type),
            str(in_group_colname),
            str(in_colnames_exclude),
            str(in_methods),
            str(in_silence_true_errors),
            str(in_boot_settings_run_bool),
            str(in_boot_settings_maxrecords),
            str(in_boot_settings_nb),
            str(in_boot_settings_seed),
            str(in_boot_settings_threshold),
            str(in_pca_settings_exec_bool),
            str(in_pca_settings_npc),
            str(in_pca_settings_quiet),
            str(in_pca_settings_pcvar),
            str(in_verbose_bool),
            str(in_warn_bool),
            str(in_sdm_bool),
            str(in_na_inform_bool),
            str(in_missingness),
            str(in_bool_loess),
            str(in_threshold_clean),
            str(in_mode_clean),
            str(in_classifymode),
            str(in_eif_bool),
            str(in_autoextract),
            result_filepath
        ]

        # Run the docker:
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
                "cleaned_data": {
                    "title": self.metadata['outputs']['cleaned_data']['title'],
                    "description": self.metadata['outputs']['cleaned_data']['description'],
                    "href": result_downloadlink
                }
            }
        }

        return 'application/json', response_object

