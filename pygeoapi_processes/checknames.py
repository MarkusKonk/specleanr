import logging
import subprocess
import json
import os
import requests
import zipfile
#from urllib.parse import urlparse
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

'''
curl --location 'http://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{ 
    "inputs": {
        "input_data": "https://testserver.com/download/matched-biodiv-data-bdb09d5c-957f-11ef-aad4-8935a9f30073.csv",
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
        self.config = None

        # Set config:
        config_file_path = os.environ.get('AQUAINFRA_CONFIG_FILE', "./config.json")
        with open(config_file_path, 'r') as config_file:
            self.config = json.load(config_file)

        # Those config items that we need:
        self.download_dir = self.config["download_dir"]
        self.own_url = self.config["own_url"]
        self.r_script_dir = self.config["boku"]["r_script_dir"]

    def set_job_id(self, job_id: str):
        self.job_id = job_id

    def __repr__(self):
        return f'<NameCheckProcessor> {self.name}'

    def execute(self, data, outputs=None):

        # Get user inputs
        input_data = data.get('input_data')
        colname_species = data.get('colname_species')
        species_names = data.get('species_names')
        pct = data.get('pct')
        bool_merge = data.get('bool_merge')

        # Checks
        if input_data is None:
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

        # Where to store output data
        downloadfilename1 = 'dunno-biodiv-data-%s.csv' % self.job_id
        downloadfilepath1 = self.download_dir.rstrip('/')+os.sep+downloadfilename1
        downloadfilename2 = 'filtered-biodiv-data-%s.csv' % self.job_id
        downloadfilepath2 = self.download_dir.rstrip('/')+os.sep+downloadfilename2

        # Run the R script:
        r_file_name = 'checknames.R'
        r_args = [input_data,
                  colname_species, species_names, str(pct), bool_merge,
                  downloadfilepath1, downloadfilepath2]
        LOGGER.info('Run R script and store result to %s and %s!' % (downloadfilepath1, downloadfilepath2))
        LOGGER.debug('R args: %s' % r_args)
        returncode, stdout, stderr, err_msg = call_r_script(LOGGER, r_file_name, self.r_script_dir, r_args)
        LOGGER.info('Running R script done: Exit code %s' % returncode)

        if not returncode == 0:
            raise ProcessorExecuteError(user_msg = err_msg)

        else:
            # Create download link:
            downloadlink1 = self.own_url.rstrip('/')+os.sep+downloadfilename1
            downloadlink2 = self.own_url.rstrip('/')+os.sep+downloadfilename2

            # Return link to file:
            response_object = {
                "outputs": {
                    "cleannames_df": {
                        "title": self.metadata['outputs']['cleannames_df']['title'],
                        "description": self.metadata['outputs']['cleannames_df']['description'],
                        "href": downloadlink1
                    },
                    "filtered_biodiversity_data": {
                        "title": self.metadata['outputs']['filtered_biodiversity_data']['title'],
                        "description": self.metadata['outputs']['filtered_biodiversity_data']['description'],
                        "href": downloadlink2
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
