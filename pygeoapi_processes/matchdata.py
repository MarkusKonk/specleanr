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
        "input_data_retrieved": "https://testserver.com/referencedata/specleanr/df_online.csv",
        "input_data_from_user": "https://testserver.com/referencedata/specleanr/efidata.csv",
        "colnames_species_names": "speciesname, scientificName",
        "colnames_countries": "JDS4_sampling_ID",
        "colnames_lat": "lat, latitude",
        "colnames_lon": "lon, long, longitude"
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
        self.config = None

        # Set config:
        config_file_path = os.environ.get('BOKU_CONFIG_FILE', "./config.json")
        with open(config_file_path, 'r') as config_file:
            self.config = json.load(config_file)

    def set_job_id(self, job_id: str):
        self.job_id = job_id

    def __repr__(self):
        return f'<DataMatchProcessor> {self.name}'

    def execute(self, data, outputs=None):

        # Get config
        config_file_path = os.environ.get('BOKU_CONFIG_FILE', "./config.json")
        with open(config_file_path) as configFile:
            configJSON = json.load(configFile)

        download_dir = configJSON["download_dir"]
        own_url = configJSON["own_url"]
        r_script_dir = configJSON["boku"]["r_script_dir"]

        # Get user inputs
        input_data_retrieved = data.get('input_data_retrieved')
        input_data_from_user = data.get('input_data_from_user')
        colnames_species_names = data.get('colnames_species_names')
        colnames_countries = data.get('colnames_countries')
        colnames_lat = data.get('colnames_lat')
        colnames_lon = data.get('colnames_lon')

        # Checks
        if input_data_retrieved is None:
            raise ProcessorExecuteError('Missing parameter "input_data_retrieved". Please provide a URL to your input csv.')
        if input_data_from_user is None:
            raise ProcessorExecuteError('Missing parameter "input_data_from_user". Please provide a URL to your input csv.')
        if colnames_species_names is None:
            raise ProcessorExecuteError('Missing parameter "colnames_species_names". Please provide a list of column names.')
        if colnames_countries is None:
            raise ProcessorExecuteError('Missing parameter "colnames_countries". Please provide a list of column names.')
        if colnames_lat is None:
            raise ProcessorExecuteError('Missing parameter "colnames_lat". Please provide a list of column names.')
        if colnames_lon is None:
            raise ProcessorExecuteError('Missing parameter "colnames_lon". Please provide a list of column names.')

        # User defined inputs:
        # Where will they be stored:
        #input_csvs_dir = self.config['boku']['input_temp_dir']

        # Download csv file:
        # TODO!!!

        # Where to store output data
        downloadfilename = 'matched-biodiv-data-%s.csv' % self.job_id
        downloadfilepath = download_dir.rstrip('/')+os.sep+downloadfilename

        # Run the R script:
        r_file_name = 'matchdata.R'
        r_args = [input_data_retrieved, input_data_from_user,
                  colnames_species_names, colnames_countries,
                  colnames_lat, colnames_lon,
                  downloadfilepath]
        LOGGER.info('Run R script and store result to %s!' % downloadfilepath)
        LOGGER.debug('R args: %s' % r_args)
        returncode, stdout, stderr, err_msg = call_r_script(LOGGER, r_file_name, r_script_dir, r_args)
        LOGGER.info('Running R script done: Exit code %s' % returncode)

        if not returncode == 0:
            raise ProcessorExecuteError(user_msg = err_msg)

        else:
            # Create download link:
            downloadlink = own_url.rstrip('/')+os.sep+downloadfilename

            # Return link to file:
            response_object = {
                "outputs": {
                    "matched_biodiversity_data": {
                        "title": self.metadata['outputs']['matched_biodiversity_data']['title'],
                        "description": self.metadata['outputs']['matched_biodiversity_data']['description'],
                        "href": downloadlink
                    }
                }
            }

            return 'application/json', response_object


def call_r_script(LOGGER, r_file_name, path_rscripts, r_args):
    # TODO: Move function to some module, same in all processes

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

