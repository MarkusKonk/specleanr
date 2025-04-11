import logging
import subprocess
import json
import os
import requests
import zipfile
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

'''
curl --location 'https://localhost/processes/pred-extract/execution' \
--header 'Content-Type: application/json' \
--data '{ 
    "inputs": {
        "input_data": "https://localhost/download/out/filtered-biodiv-data.csv",
        "input_raster_name": "worldclim",
        "study_area": "https://localhost/referencedata/specleanr/basinfinal.zip",
        "colname_lat": "decimalLatitude",
        "colname_lon": "decimalLongitude",
        "colname_species": "speciescheck",
        "min_pts": 10,
        "bool_multiple_species": true,
        "bool_merge": false,
        "bool_list": false
    }
}'

### Same request, but with GeoJSON study area, instead of shapefile.
curl --location 'https://localhost/processes/pred-extract/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://localhost/download/out/filtered-biodiv-data.csv",
        "input_raster_name": "worldclim",
        "study_area_geojson_url": "https://vm4072.kaj.pouta.csc.fi/ddas/oapif/collections/hydro90-basin/items?f=json&basin_id=1293067",
        "colname_lat": "decimalLatitude",
        "colname_lon": "decimalLongitude",
        "colname_species": "speciescheck",
        "min_pts": 10,
        "bool_multiple_species": true,
        "bool_merge": false,
        "bool_list": false
    }
}'


### Same request, but with GeoJSON study area directly posted in the HTTP POST payload.
### Note: This area is too small to yield meaningful results!
curl --location 'https://localhost/processes/pred-extract/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://localhost/download/out/filtered-biodiv-data.csv",
        "input_raster_name": "worldclim",
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
        "colname_lat": "decimalLatitude",
        "colname_lon": "decimalLongitude",
        "colname_species": "speciescheck",
        "min_pts": 10,
        "bool_multiple_species": true,
        "bool_merge": false,
        "bool_list": false
    }
}'

'''

LOGGER = logging.getLogger(__name__)

script_title_and_path = __file__
metadata_title_and_path = script_title_and_path.replace('.py', '.json')
PROCESS_METADATA = json.load(open(metadata_title_and_path))

class PredExtractProcessor(BaseProcessor):

    def __init__(self, processor_def):
        super().__init__(processor_def, PROCESS_METADATA)
        self.supports_outputs = True
        self.job_id = 'job-id-not-set'
        self.r_script = 'pred_extract.R'
        self.image_name = 'specleanr:20250410'

        # Set config:
        config_file_path = os.environ.get('AQUAINFRA_CONFIG_FILE', "./config.json")
        with open(config_file_path, 'r') as config_file:
            config = json.load(config_file)
            self.download_dir = config["download_dir"].rstrip('/')
            self.download_url = config["download_url"].rstrip('/')
            self.docker_executable = config["docker_executable"]
            self.input_raster_dir = config['specleanr']['inputs_static_rasters_path'].rstrip('/')


    def set_job_id(self, job_id: str):
        self.job_id = job_id

    def __repr__(self):
        return f'<PredExtractProcessor> {self.name}'

    def execute(self, data, outputs=None):

        # Get user inputs
        input_data_url = data.get('input_data')
        input_raster_name = data.get('input_raster_name') # TODO: Get from data lake?
        study_area_shp_url = data.get('study_area')
        study_area_geojson_url = data.get('study_area_geojson_url')
        study_area_geojson = data.get('study_area_geojson')
        colname_lat = data.get('colname_lat')
        colname_lon = data.get('colname_lon')
        colname_species = data.get('colname_species')
        min_pts = data.get('min_pts')
        bool_multiple_species = data.get('bool_multiple_species')
        bool_merge = data.get('bool_merge')
        bool_list = data.get('bool_list')


        # Checks
        if input_data_url is None:
            raise ProcessorExecuteError('Missing parameter "input_data". Please provide a URL to your input csv.')
        if study_area_shp_url is None and study_area_geojson_url is None and study_area_geojson is None:
            raise ProcessorExecuteError('Missing parameter "study_area". Please provide a URL to your input study area as zipped shapefile, as geojson (or just post geojson)...')
        if input_raster_name is None:
            raise ProcessorExecuteError('Missing parameter "input_raster_name". Please provide a name of your input raster.')
        if colname_lat is None:
            raise ProcessorExecuteError('Missing parameter "colname_lat". Please provide a column name.')
        if colname_lon is None:
            raise ProcessorExecuteError('Missing parameter "colname_lon". Please provide a column name.')
        if colname_species is None:
            raise ProcessorExecuteError('Missing parameter "colname_species". Please provide a column name.')
        if min_pts is None:
            raise ProcessorExecuteError('Missing parameter "min_pts". Please provide a number.')
        if bool_multiple_species is None:
            raise ProcessorExecuteError('Missing parameter "bool_multiple_species". Please provide "true" or "false".')
        if bool_merge is None:
            raise ProcessorExecuteError('Missing parameter "bool_merge". Please provide "true" or "false".')
        if bool_list is None:
            raise ProcessorExecuteError('Missing parameter "bool_list". Please provide "true" or "false".')

        # From booleans to string:
        bool_multiple_species = 'true' if bool_multiple_species else 'false'
        bool_merge = 'true' if bool_merge else 'false'
        bool_list = 'true' if bool_list else 'false'

        # Input csv file passed by user:
        input_dir = self.download_dir+'/in/job_%s' % self.job_id
        input_csv_path = download_any_file(input_data_url, input_dir, ".csv")

        # Input study area passed by user:
        # Download and unzip shapefile:
        if study_area_shp_url is not None:
            input_polygons_path = download_zipped_shapefile(study_area_shp_url, input_dir)

        # OR download and store GeoJSON:
        # TODO Probably storing to disk is not needed, instead read directly from HTTP response...
        elif study_area_geojson_url is not None:
            input_polygons_path = download_geojson(study_area_geojson_url, input_dir, '.json')

        # OR receive and store GeoJSON:
        # TODO Probably storing to disk is not needed, instead read directly from HTTP payload...
        elif study_area_geojson is not None:
            input_polygons_path = store_geojson(study_area_geojson, input_dir, '.json')

        # Input raster: TODO Data lake!
        if input_raster_name == 'worldclim':
            input_raster_path = '%s/worldclim.tiff' % self.input_raster_dir
            LOGGER.debug('Using static raster: %s' % input_raster_path)
        else:
            err_msg = "Other rasters are currently not allowed! Try with worldclim."
            LOGGER.error(err_msg)
            raise NotImplementedError(err_msg) # TODO: Not implemented!

        # Where to store output data
        result_filename = 'multiprecleaned-%s.csv' % self.job_id
        result_filepath     = self.download_dir+'/out/'+result_filename
        result_downloadlink = self.download_url+'/out/'+result_filename

        # Assemble args for R script:
        r_args = [
            input_csv_path,
            input_raster_path,
            input_polygons_path,
            colname_lat,
            colname_lon,
            colname_species,
            str(min_pts),
            bool_merge,
            bool_list,
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
                "multiprecleaned": {
                    "title": self.metadata['outputs']['multiprecleaned']['title'],
                    "description": self.metadata['outputs']['multiprecleaned']['description'],
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


def download_zipped_shapefile(input_url, input_dir):

    # Create a unique dir just for this download.
    # Why? We do not control what is in the unzipped zip, so we cannot
    # return the correct name if various files are mixed in the same dir!
    randomstring = os.urandom(5).hex()
    input_shp_dir = input_dir.rstrip('/')+'/zippedshp%s' % randomstring

    input_zipped_shp_path = download_any_file(input_url, input_shp_dir, '.zip')
    unzip_file(input_zipped_shp_path, input_shp_dir)

    # Find name of shapefile, which we dont control, as it is defined by whoever
    # zipped the zipfile:
    input_unzipped_shp_path = retrieve_file_name_by_ending(input_shp_dir, '.shp')
    return input_unzipped_shp_path


def retrieve_file_name_by_ending(input_dir, ending):

    # If user passed zipped inputs, we don't know the filename, so we can extract
    # it by its ending, if there are only one file with this ending (e.g. zipped shape).
    # DANGER: If there are several, we may return the wrong one!
    # TODO I am sure there is a better way!
    for filename in os.listdir(input_dir):
        if filename.endswith(ending):
            filepath = '%s/%s' % (input_dir, filename)
            LOGGER.debug('Name of %s file: %s' % (ending, filepath))
            return filepath


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


def unzip_file(input_zipped_file_path, unzip_dir):
    # TODO: See important secutiry warning here: https://docs.python.org/3/library/zipfile.html#zipfile.ZipFile.extractall

    # Note: Make sure unzip_dir is a custom dir just for this zipfile!
    # Why? We do not control what is in the unzipped zip, so we cannot
    # return the correct name if various files are mixed in the same dir!

    LOGGER.debug('Unzipping file "%s" to "%s"' % (input_zipped_file_path, unzip_dir))
    with zipfile.ZipFile(input_zipped_file_path, 'r') as zip_ref:
        zip_ref.extractall(unzip_dir)
        LOGGER.debug('Unzipping file... DONE.')


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
