import logging
import subprocess
import json
import os
import requests
import zipfile
import warnings
from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError
from pygeoapi.process.specleanr.pygeoapi_processes.utils import run_docker_container
from pygeoapi.process.specleanr.pygeoapi_processes.utils import store_geojson


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
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "databases": ["gbif", "inat"],
        "gbif_limit": 20,
        "inaturalist_limit": 20,
        "percentage_correctness": 30,
        "synonym_check": true,
        "study_area_geojson": {
            "type": "FeatureCollection",
            "features": [{
                "type": "Feature",
                "properties": {},
                "geometry": {
                    "type": "Polygon",
                    "coordinates": [[
                        [15.067916439922868,48.71725768072221],
                        [15.067916439922868,48.09522635300115],
                        [16.295486613797266,48.09522635300115],
                        [16.295486613797266,48.71725768072221],
                        [15.067916439922868,48.71725768072221]
                    ]]
                }
            }]
        }
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

        # Checking for all mandatory input params:
        if in_data_path is None:
            raise ProcessorExecuteError('Missing parameter "input_data".')
        # OPT in_species_column
        if in_database is None:
            raise ProcessorExecuteError('Missing parameter "databases". Please provide list of databases to query.')
        # OPT in_gbif_lim in_inat_lim in_vert_lim
        if (study_area_shp_url is None and
            study_area_bbox is None and
            study_area_geojson is None and
            study_area_geojson_url is None):
            err_msg = 'Missing parameter "study_area_...". Please provide the study area as (zipped) shapefile or geojson or as a bounding box.'
            raise ProcessorExecuteError(err_msg)
        # OPT in_percent_correct#
        if in_synonym_check is None:
            raise ProcessorExecuteError('Missing parameter "synonym_check". Please provide "true" or "false".')

        #################################
        ### Input and output          ###
        ### storage/download location ###
        #################################

        # Where to store input data
        # Here, downloaded inputs will be stored by pygeoapi.
        # It will be mounted as read-only into the docker.
        # This process may store GeoJSON data there that it received in HTTP POST!
        input_dir = self.download_dir+'/in/specleanr_job_%s' % self.job_id

        # Where to store output data
        output_dir = os.path.join(self.download_dir, "out")
        output_dir = self.download_dir+'/out/specleanr_job_%s' % self.job_id
        output_url = self.download_url+'/out/specleanr_job_%s' % self.job_id
        result_filename = 'biodiv-data-%s.csv' % self.job_id
        result_filepath     = output_dir+'/'+result_filename
        result_downloadlink = output_url+'/'+result_filename
        os.makedirs(output_dir, exist_ok=True)

        ##################################################
        ### Convert user inputs to what R script needs ###
        ##################################################

        # Join database strings:
        in_database = ','.join(in_database)

        # Input study area passed by user:
        # If user provided link to a shapefile, pass it to the package
        # (it can deal with remote shapefiles, as long as they are zipped):
        if study_area_shp_url is not None:
            in_extent = study_area_shp_url
            input_dir = None # No need to mount, so set to None

        # OR if user provided link to a GeoJSON file, pass it to the package
        # (it can deal with remote GeoJSON file):
        elif study_area_geojson_url is not None:
            in_extent = study_area_geojson_url
            input_dir = None # No need to mount, so set to None

        # OR extract GeoJSON from HTTP POST payload and store it:
        # TODO Probably storing to disk is not needed, instead read directly from HTTP payload...
        elif study_area_geojson is not None:
            os.makedirs(input_dir, exist_ok=True) # create the job-specific dir
            input_polygons_path = store_geojson(study_area_geojson, input_dir, '.json')
            in_extent = input_polygons_path

        # OR extract a JSON bounding box and convert to R format:
        elif study_area_bbox is not None:
            input_dir = None # No need to mount, so set to None
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
            input_dir = None # No need to mount, so set to None


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
                "biodiversity_data": {
                    "title": self.metadata['outputs']['biodiversity_data']['title'],
                    "description": self.metadata['outputs']['biodiversity_data']['description'],
                    "href": result_downloadlink
                }
            }
        }

        return 'application/json', response_object

