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
        "input_data": "https://example.com/exampledata/boku/iris1.csv",
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
        return f'<MultiDetectProcessor> {self.name}'

    def execute(self, data, outputs=None):

        #################################
        ### Get user inputs and check ###
        #################################

        # Get user inputs
        # This below is the order in which it has to be passed to the docker!
        in_data_path_or_url               = data.get('input_data')
        in_var_ofinterest                 = data.get('colname_variable')
        in_select_var                     = data.get('select_columns')
        in_bool_multiple_species          = data.get('multiple_species')
        in_output_type                    = data.get('output_type') #clean or outlier/ Defualt outliers
        in_group_colname                  = data.get('group_colname', 'not_provided')
        in_colnames_exclude               = data.get('colname_exclude')
        in_methods                        = data.get('methods')
        in_silence_true_errors            = data.get('silence_true_errors')
        in_boot_settings_run_bool         = data.get('boot_run')
        in_boot_settings_maxrecords       = data.get('boot_maxrecords')
        in_boot_settings_nb               = data.get('number_of_boots')
        in_boot_settings_seed             = data.get('setseed')
        in_boot_settings_threshold        = data.get('boot_threshold')
        in_pca_settings_exec_bool         = data.get('exceute_pca')
        in_pca_settings_npc               = data.get('number_of_pca')
        in_pca_settings_quiet             = data.get('pca_silence')
        in_pca_settings_pcvar             = data.get('pcavariable')
        #in_verbose_bool                  = True # not to be set by user!
        #in_warn_bool                     = True # not to be set by user!
        in_sdm_bool                       = data.get('sdm_data') #multivaritate data, sdm must be TRUE
        in_na_inform_bool                 = data.get('inform_na_outlier')
        in_missingness                    = data.get('missingness')
        in_bool_loess                     = data.get('bool_loess')
        in_threshold_clean                = data.get('threshold_clean')
        in_mode_clean                     = data.get('outlierweights_mode')
        in_classifymode                   = data.get('classifymode') #med
        in_eif_bool                       = data.get('eif_bool') #empirical influence function
        in_autoextract                    = data.get('classify_or_autoremove')
        #out_result_path # to be defined by this process.

        # Checks
        if in_data_path_or_url is None:
            raise ProcessorExecuteError('Missing parameter "input_data". Please provide a URL to your input table.')
        if in_var_ofinterest is None:
            raise ProcessorExecuteError('Missing parameter "colname_variable". Please provide a column name.')
        if in_bool_multiple_species is None:
            raise ProcessorExecuteError('Missing parameter "multiple_species". Please provide \"true\" or \"false\".')
        if in_methods is None:
            raise ProcessorExecuteError('Missing parameter "methods". Please provide a value.')
        #if in_silence_true_errors is None:
            #raise ProcessorExecuteError('Missing parameter "silence_true_errors". Please provide \"true\" or \"false\".') defualt is there
        if in_missingness is None:
            raise ProcessorExecuteError('Missing parameter "missingness". Please provide a value.')

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

        # Nothing to do here...

        ####################################
        ### Assemble args and run docker ###
        ####################################

        # Assemble args for R script:
        in_verbose_bool = True
        in_warn_bool = True
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
        elif arg == 'None':
            newarg = 'null'
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
