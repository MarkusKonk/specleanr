import logging
import subprocess
import json
import os
import requests
import zipfile
import warnings
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

'''
# Works: Test 2025-08-04 (Merret)
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "databases": ["gbif", "inat"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20,
        "study_area_bbox": {"bbox": [42.08333, 8.15250, 50.24500, 29.73583]},
        "percentage_correctness": 30,
        "synonym_check": true
    }
}'


# What about these?
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_bbox": {"bbox": [42.08333, 8.15250, 50.24500, 29.73583]},
        "input_data": "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 10,
        "vertnet_limit": 10,
        "inaturalist_limit": 10
    }
}'

curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_shapefile": "http://localhost/referencedata/specleanr/basinfinal.zip",
        "input_data": "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 50,
        "vertnet_limit": 50,
        "inaturalist_limit": 50
    }
}'

### Same request, but with GeoJSON study area, instead of shapefile.
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_geojson_url": "https://vm4072.kaj.pouta.csc.fi/ddas/oapif/collections/hydro90-basin/items?f=json&basin_id=1293067",
        "input_data": "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 50,
        "vertnet_limit": 50,
        "inaturalist_limit": 50
    }
}'

### Same request, but with GeoJSON study area directly posted in the HTTP POST payload.
### Note: This area is too small to yield meaningful results in the subsequent steps!
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_geojson": {
            "type": "FeatureCollection",
            "features": [{
                "type": "Feature",
                "properties": {},
                "geometry": {
                    "coordinates": [[
                        [ 15.067916439922868, 48.71725768072221],
                        [ 15.067916439922868, 48.09522635300115],
                        [ 16.295486613797266, 48.09522635300115],
                        [ 16.295486613797266, 48.71725768072221],
                        [ 15.067916439922868, 48.71725768072221]
                    ]],
                    "type": "Polygon"
                }
            }]
        },
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
        self.job_id = 'job-id-not-set'
        self.r_script = 'getdata.R'
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
        return f'<DataRetrievalProcessor> {self.name}'

    def execute(self, data, outputs=None):

        #################################
        ### Get user inputs and check ###
        #################################

        # In the order that the docker/R-script needs them:
        in_data_path = data.get("input_data")
        in_species_column = data.get("colname_species", 'null')
        in_database = data.get("databases")
        in_gbif_lim = data.get('gbif_limit', 50)
        in_inat_lim = data.get('inaturalist_limit', 50)
        in_vert_lim = data.get('vertnet_limit', 50)
        in_verbose = True # (no effect on client, so not defined by client)
        # Different ways of passing the extent/bounding box:
        study_area_shp_url = data.get('study_area_shapefile')
        study_area_geojson_url = data.get('study_area_geojson_url')
        study_area_geojson = data.get('study_area_geojson')
        study_area_bbox = data.get('study_area_bbox')
        in_percent_correct = data.get("percentage_correctness", 80)
        in_synonym_check    = data.get("synonym_check") # boolean
        in_warn_check = True # (no effect on client, so not defined by client)

        # Checks
        if in_data_path is None:
            raise ProcessorExecuteError('Missing parameter "input_data".')
        if in_database is None:
            raise ProcessorExecuteError('Missing parameter "databases".')
        if in_synonym_check is None:
            raise ProcessorExecuteError('Missing parameter "synonym_check". Please provide "true" or "false".')
        if (study_area_shp_url is None or
            study_area_bbox is None or
            study_area_geojson is None or
            study_area_geojson_url is None):
            raise ProcessorExecuteError('Missing parameter "study_area_...". Please provide the study area in some way.')

        #################################
        ### Input and output          ###
        ### storage/download location ###
        #################################

        # Input files passed by user:
        # Download and unzip shapefile:
        input_dir = self.download_dir+'/in/job_%s' % self.job_id
        # TODO: Come up with a good plan to have unique dirs for the jobs, so we separate
        # users' inputs properly! But the /in has to be there I guess to be mounted...
        # Maybe /in will not be exposed publicly, while /out will??? Do we need to separate /in and /out?

        # Where to store output data
        result_filename = 'biodiv-data-%s.csv' % self.job_id
        result_filepath     = self.download_dir+'/out/'+result_filename
        result_downloadlink = self.download_url+'/out/'+result_filename


        ##################################################
        ### Convert user inputs to what R script needs ###
        ##################################################

        # Join database strings:
        in_database = ','.join(in_database)

        # If the user provided a link to a zipped shapefile, the R package will download and unzip it...
        if study_area_shp_url is not None:
            input_polygons_path = study_area_shp_url
            in_extent = input_polygons_path

        # OR download and store GeoJSON:
        # TODO Probably storing to disk is not needed, instead read directly from HTTP response...
        elif study_area_geojson_url is not None:
            input_polygons_path = download_geojson(study_area_geojson_url, input_dir, '.json')
            in_extent = input_polygons_path

        # OR receive and store GeoJSON:
        # TODO Probably storing to disk is not needed, instead read directly from HTTP payload...
        elif study_area_geojson is not None:
            input_polygons_path = store_geojson(study_area_geojson, input_dir, '.json')
            in_extent = input_polygons_path

        elif study_area_bbox is not None:
            # R script needs: "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500"
            # OGC API spec:
            # "boundingBoxInput": {
            #   "bbox": [ 51.9, 7, 52, 7.1 ],
            #   "crs": "http://www.opengis.net/def/crs/OGC/1.3/CRS84"
            # },
            in_extent = "xmin={west}, ymin={south}, xmax={east}, ymax={north}".format(
                south = study_area_bbox["bbox"][0],
                west  = study_area_bbox["bbox"][1],
                north = study_area_bbox["bbox"][2],
                east  = study_area_bbox["bbox"][3]
            )

        else:
            in_extent = "null"


        ####################################
        ### Assemble args and run docker ###
        ####################################

        # Assemble args for R script: ###
        #THIS ARRANGMENT MUST MATCH THE SOURCE CODE NUMBERING
        r_args = [
            in_data_path,
            in_species_column,
            in_database,
            str(in_gbif_lim),
            str(in_inat_lim),
            str(in_vert_lim),
            str(in_verbose),
            in_extent,
            str(in_percent_correct),
            str(in_synonym_check),
            str(in_warn_check),
            result_filepath
        ]
        LOGGER.debug('r_args: %s' % r_args)

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
                "biodiversity_data": {
                    "title": self.metadata['outputs']['biodiversity_data']['title'],
                    "description": self.metadata['outputs']['biodiversity_data']['description'],
                    "href": result_downloadlink
                }
            }
        }

        return 'application/json', response_object


def store_geojson(geojson, input_dir, ending=None):

    # Make sure the dir exists:
    if not os.path.exists(input_dir):
        os.makedirs(input_dir)

    # How should the downloaded file be named?
    # If the URL includes a name: TODO can we trust this name?
    #filename = os.path.basename(input_url_geojson)
    filename = "geojson%s" % os.urandom(5).hex()
    filename = filename if ending is None else filename+ending
    input_file_path = '%s/%s' % (input_dir, filename)
    LOGGER.debug('Storing input geojson file to: %s' % input_file_path)

    with open(input_file_path, 'w') as myfile:
        json.dump(geojson, myfile)

    return input_file_path


def download_geojson(input_url_geojson, input_dir, ending=None):

    # Download file into given dir:
    LOGGER.debug('Downloading input geojson file: %s' % input_url_geojson)
    resp = requests.get(input_url_geojson)
    if not resp.status_code == 200:
        raise ProcessorExecuteError('Could not download input geojson file (HTTP status %s): %s' % (resp.status_code, input_url_geojson))

    return store_geojson(resp.json(), input_dir, ending=ending)


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
    LOGGER.debug('Local in/out: %s, %s' % (local_in, local_out))

    # Ensure directories exist
    os.makedirs(local_in, exist_ok=True)
    os.makedirs(local_out, exist_ok=True)

    # Replace paths in args:
    LOGGER.debug('Script args: %s' % script_args)
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
