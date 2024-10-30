import logging
import subprocess
import json
import os
import requests
import zipfile
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

'''
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{ 
    "inputs": {
        "study_area": "https://testserver/download/basinfinal.zip",
        "species_names": "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla",
        "gbif_limit": 50,
        "vertnet_limit": 50,
        "inaturalist_limit": 50
    }
}'
'''

LOGGER = logging.getLogger(__name__)

script_title_and_path = __file__
metadata_title_and_path = script_title_and_path.replace('.py', '.json')
PROCESS_METADATA = json.load(open(metadata_title_and_path))

class DataRetrievalProcessor(BaseProcessor):

    def __init__(self, processor_def):
        super().__init__(processor_def, PROCESS_METADATA)
        self.supports_outputs = True
        self.job_id = 'nnothing-yet'
        self.config = None

        # Set config:
        config_file_path = os.environ.get('BOKU_CONFIG_FILE', "./config.json")
        with open(config_file_path, 'r') as config_file:
            self.config = json.load(config_file)

    def set_job_id(self, job_id: str):
        self.job_id = job_id

    def __repr__(self):
        return f'<DataRetrievalProcessor> {self.name}'

    def execute(self, data, outputs=None):

        # Get config
        config_file_path = os.environ.get('BOKU_CONFIG_FILE', "./config.json")
        with open(config_file_path) as configFile:
            configJSON = json.load(configFile)

        download_dir = configJSON["download_dir"]
        download_url = configJSON["download_url"]
        r_script_dir = configJSON["boku"]["r_script_dir"]

        # Get user inputs
        study_area = data.get('study_area')
        species_names = data.get('species_names')
        gbif_limit = data.get('gbif_limit', 50)
        inaturalist_limit = data.get('inaturalist_limit', 50)
        vertnet_limit = data.get('vertnet_limit', 50)

        # Checks
        if study_area is None:
            raise ProcessorExecuteError('Missing parameter "study_area". Please provide a URL to your input shapefile.')
        if species_names is None:
            raise ProcessorExecuteError('Missing parameter "species_names". Please provide a list of species.')

        # User defined inputs:
        # Where will they be stored:
        input_polygons_dir = self.config['boku']['input_temp_dir']
        input_polygons_dir = input_polygons_dir.rstrip('/')+'/inputs_%s' % self.job_id
        if not os.path.exists(input_polygons_dir):
            os.makedirs(input_polygons_dir)

        # Download and unzip shapefile:
        input_polygons_path = download_zipped_shapefile(study_area, input_polygons_dir)

        # Where to store output data
        downloadfilename = 'biodiv-data-%s.csv' % self.job_id
        downloadfilepath = download_dir.rstrip('/')+os.sep+downloadfilename

        # Run the R script:
        r_file_name = 'getdata.R'
        r_args = [input_polygons_path, species_names,
                  str(gbif_limit), str(inaturalist_limit), str(vertnet_limit),
                  downloadfilepath]
        LOGGER.info('Run R script and store result to %s!' % downloadfilepath)
        LOGGER.debug('R args: %s' % r_args)
        returncode, stdout, stderr, err_msg = call_r_script(LOGGER, r_file_name, r_script_dir, r_args)
        LOGGER.info('Running R script done: Exit code %s' % returncode)

        if not returncode == 0:
            raise ProcessorExecuteError(user_msg = err_msg)

        else:
            # Create download link:
            downloadlink = download_url.rstrip('/')+os.sep+downloadfilename

            # Return link to file:
            response_object = {
                "outputs": {
                    "cleaned_data": {
                        "title": self.metadata['outputs']['biodiversity_data']['title'],
                        "description": self.metadata['outputs']['biodiversity_data']['description'],
                        "href": downloadlink
                    }
                }
            }

            return 'application/json', response_object

def download_zipped_shapefile(input_url_shapefile, input_polygons_dir):
    # TODO test

    # Download file:
    LOGGER.info('Downloading input data file: %s' % input_url_shapefile)
    input_zipped_shp_path = '%s/downloaded.zip' % input_polygons_dir
    resp = requests.get(input_url_shapefile)
    if resp.status_code == 200:
        LOGGER.debug('Writing input shape file to: %s' % input_zipped_shp_path)
        with open(input_zipped_shp_path, 'wb') as myfile:
            for chunk in resp.iter_content(chunk_size=1024):
                if chunk:
                    myfile.write(chunk)
        
        LOGGER.info('Unzipping file "%s" to "%s"' % (input_zipped_shp_path, input_polygons_dir))
        with zipfile.ZipFile(input_zipped_shp_path, 'r') as zip_ref:
            zip_ref.extractall(input_polygons_dir)
            print('Unzipped file to "%s"' % input_polygons_dir)
            LOGGER.info('Unzipped file to "%s"' % input_polygons_dir)
            
            # Find name of shapefile, which we dont control:
            # TODO I am sure there is a better way!
            for filename in os.listdir(input_polygons_dir):
                if filename.endswith('shp'):
                    input_polygons_path = '%s/%s' % (input_polygons_dir, filename)
                    return input_polygons_path

    else:
        raise ProcessorExecuteError('Could not download input file (HTTP status %s): %s' % (resp.status_code, input_url_shapefile))


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


