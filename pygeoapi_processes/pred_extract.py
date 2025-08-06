import logging
import subprocess
import json
import os
import requests
import zipfile
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError
from pygeoapi.process.specleanr.pygeoapi_processes.utils import run_docker_container_with_readonly
from pygeoapi.process.specleanr.pygeoapi_processes.utils import store_geojson


'''
# Example using CSV and GeoJSON and GeoTIFF input
# Works: Tested on 2025-07-31 (Merret)

curl --location 'http://localhost:5000/processes/pred-extract/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_for_pred_extract.csv",
        "input_raster_url_or_name": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/worldclim.tiff",
        "study_area_geojson_url": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson",
        "colname_lat": "decimalLatitude",
        "colname_lon": "decimalLongitude",
        "colname_species": "species",
        "mininmum_sprecords": 10,
        "bool_merge": true,
        "bool_list": false,
        "bool_coords": true,
        "bool_remove_nas": true,
        "bool_remove_duplicates": false,
        "minimum_sprecordsallow": false
    }
}'

# Example using CSV and GeoJSON (passed directly) and GeoTIFF (static, on the server) input
# Works: Tested on 2025-08-05 (Merret)

curl --location 'http://localhost:5000/processes/pred-extract/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_for_pred_extract.csv",
        "input_raster_url_or_name": "worldclim",
        "study_area_geojson": {
            "type": "FeatureCollection",
            "features": [{
                "type": "Feature",
                "properties": {},
                "geometry": {
                    "type": "Polygon",
                    "coordinates": [[
                        [ 9.3593826329501, 50.15015768185512],
                        [ 7.9191189516121, 47.12117448155652],
                        [ 9.3089855398883, 45.44042789489441],
                        [11.3163007857614, 46.07905231554338],
                        [19.8679225896241, 41.37556228149228],
                        [28.1073844852147, 42.03408100365081],
                        [30.8463783248204, 45.85409918874026],
                        [28.2557360912777, 49.13596193751451],
                        [20.1314141836580, 50.32119604024012],
                        [ 9.3593826329502, 50.15015768185512]
                    ]]
                }
            }]
        },
        "colname_lat": "decimalLatitude",
        "colname_lon": "decimalLongitude",
        "colname_species": "species",
        "mininmum_sprecords": 10,
        "bool_merge": true,
        "bool_list": false,
        "bool_coords": true,
        "bool_remove_nas": true,
        "bool_remove_duplicates": false,
        "minimum_sprecordsallow": false
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
        self.image_name = 'specleanr:20250805'

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

        #################################
        ### Get user inputs and check ###
        #################################

        # Get user inputs
        # In the order that the docker/R-script needs them:
        in_data_path_or_url = data.get('input_data')
        in_raster_path = data.get('input_raster_url_or_name') # TODO: Get raster from data lake?
        study_area_shp_url = data.get('study_area_shapefile')
        study_area_geojson_url = data.get('study_area_geojson_url')
        study_area_geojson = data.get('study_area_geojson')
        study_area_bbox = data.get('study_area_bbox')
        in_colname_lat = data.get('colname_lat') # string
        in_colname_lon = data.get('colname_lon') # string
        in_colname_species = data.get('colname_species') # string
        in_min_pts = data.get('mininmum_sprecords') # number
        in_bool_merge = data.get('bool_merge') # boolean
        in_bool_list = data.get('bool_list') # boolean
        in_bool_verbose = True # (no effect on client, so not defined by client)
        in_bool_warn = True # (no effect on client, so not defined by client)
        in_bool_coords = data.get('bool_coords') # boolean
        in_na_inform = True # (no effect on client, so not defined by client)
        in_na_rm = data.get('bool_remove_nas') # boolean
        in_rm_duplicates = data.get("bool_remove_duplicates") # boolean
        in_minimumpts_rm = data.get("minimum_sprecordsallow") # boolean

        # Checking for all mandatory input params:
        if in_data_path_or_url is None:
            raise ProcessorExecuteError('Missing parameter "input_data". Please provide a URL to your input csv.')
        if in_raster_path is None:
            raise ProcessorExecuteError('Missing parameter "input_raster_url_or_name". Please provide a name or URL of your input raster.')
        if (study_area_shp_url is None and
            study_area_bbox is None and
            study_area_geojson is None and
            study_area_geojson_url is None):
            err_msg = 'Missing parameter "study_area_...". Please provide the study area as (zipped) shapefile or geojson or as a bounding box.'
            raise ProcessorExecuteError(err_msg)
        if in_colname_lat is None:
            raise ProcessorExecuteError('Missing parameter "colname_lat". Please provide a column name.')
        if in_colname_lon is None:
            raise ProcessorExecuteError('Missing parameter "colname_lon". Please provide a column name.')
        if in_colname_species is None:
            raise ProcessorExecuteError('Missing parameter "colname_species". Please provide a column name.')
        if in_min_pts is None:
            raise ProcessorExecuteError('Missing parameter "mininmum_sprecords". Please provide a number.')
        if in_bool_merge is None:
            raise ProcessorExecuteError('Missing parameter "bool_merge". Please provide "true" or "false".')
        if in_bool_list is None:
            raise ProcessorExecuteError('Missing parameter "bool_list". Please provide "true" or "false".')
        if in_bool_coords is None:
            raise ProcessorExecuteError('Missing parameter "bool_coords". Please provide "true" or "false".')
        if in_na_rm is None:
            raise ProcessorExecuteError('Missing parameter "bool_remove_nas". Please provide "true" or "false".')
        if in_rm_duplicates is None:
            raise ProcessorExecuteError('Missing parameter "bool_remove_duplicates". Please provide "true" or "false".')
        if in_minimumpts_rm is None:
            raise ProcessorExecuteError('Missing parameter "minimum_sprecordsallow". Please provide "true" or "false".')

        #################################
        ### Input and output          ###
        ### storage/download location ###
        #################################

        # Where to store input data
        # Here, downloaded inputs will be stored by pygeoapi.
        # It will be mounted as read-only into the docker.
        input_dir = self.download_dir+'/in/job_%s' % self.job_id

        # Where to store output data
        output_dir = self.download_dir+'/out/job_%s' % self.job_id
        output_url = self.download_url+'/out/job_%s' % self.job_id
        result_filename = 'multiprecleaned-%s.csv' % self.job_id
        result_filepath     = output_dir+'/'+result_filename
        result_downloadlink = output_url+'/'+result_filename
        os.makedirs(output_dir, exist_ok=True)


        ##################################################
        ### Convert user inputs to what R script needs ###
        ##################################################

        # Input study area passed by user:
        # If user provided link to a shapefile, pass it to the package
        # (it can deal with remote shapefiles, as long as they are zipped):
        if study_area_shp_url is not None:
            in_bbox_path = input_polygons_path
            input_dir = None # No need to mount, so set to None

        # OR if user provided link to a GeoJSON file, pass it to the package
        # (it can deal with remote GeoJSON file):
        elif study_area_geojson_url is not None:
            in_bbox_path = study_area_geojson_url
            input_dir = None # No need to mount, so set to None

        # OR extract GeoJSON from HTTP POST payload and store it:
        # TODO Probably storing to disk is not needed, instead read directly from HTTP payload...
        elif study_area_geojson is not None:
            os.makedirs(input_dir, exist_ok=True) # create the job-specific dir
            input_polygons_path = store_geojson(study_area_geojson, input_dir, '.json')
            in_bbox_path = input_polygons_path

        # OR extract a JSON bounding box and convert to R format:
        elif study_area_bbox is not None:
            # R script needs: "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500"
            # OGC API spec:
            # "boundingBoxInput": {
            #   "bbox": [ 51.9, 7, 52, 7.1 ],
            #   "crs": "http://www.opengis.net/def/crs/OGC/1.3/CRS84"
            # },
            in_bbox_path = "xmin={west}, ymin={south}, xmax={east}, ymax={north}".format(
                south = study_area_bbox["bbox"][0],
                west  = study_area_bbox["bbox"][1],
                north = study_area_bbox["bbox"][2],
                east  = study_area_bbox["bbox"][3]
            )
            input_dir = None # No need to mount, so set to None

        else:
            in_bbox_path = "null"
            input_dir = None # No need to mount, so set to None

        # Input raster:
        if in_raster_path.startswith('http'):
            LOGGER.debug('Using the raster provided by the user. It will not be downloaded, but accessed as-is.')
        else:
            LOGGER.debug('Using static raster on the server...')
            # TODO: Maybe remove this option at some point?
            if in_raster_path == 'worldclim':
                in_raster_path = '%s/worldclim.tiff' % self.input_raster_dir
            else:
                err_msg = "Providing other rasters (than worldclim) by name is currently not possible! Try with worldclim, or provide the URL to a cloud-optimized raster."
                LOGGER.error(err_msg)
                raise NotImplementedError(err_msg)
        LOGGER.debug('Using raster: %s' % in_raster_path)

        ####################################
        ### Assemble args and run docker ###
        ####################################

        # Assemble args for R script:
        r_args = [
            in_data_path_or_url,
            in_raster_path,
            in_bbox_path,
            in_colname_lat,
            in_colname_lon,
            in_colname_species,
            str(in_min_pts),
            str(in_bool_merge),
            str(in_bool_list),
            str(in_bool_verbose),
            str(in_bool_warn),
            str(in_bool_coords),
            str(in_na_inform),
            str(in_na_rm),
            str(in_rm_duplicates),
            str(in_minimumpts_rm),
            result_filepath
        ]

        # Run the docker:
        returncode, stdout, stderr, user_err_msg = run_docker_container_with_readonly(
            self.docker_executable,
            self.image_name,
            self.r_script,
            input_dir,
            output_dir,
            self.input_raster_dir,
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

