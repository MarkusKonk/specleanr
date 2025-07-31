# Testing and running the scripts

Merret Buurman, IGB Berlin, 2025-07-14

The R scripts in this directory (`src/`) are not part of the library. Instead, they use the functions defined in the library, and they are scripts that can be called from a commandline (e.g. bash in Linux). These scripts are included in the specleanr Dockerfile, and they are called as part of the OGC processes (AquaINFRA project, pygeoapi platform).

This README shows how to call them in all three ways:

* Via command line
* Using the Docker image
* Via the HTTP API (OGC API)

Below you find examples how the scripts in `src/bla.R` can be executed from the **Linux command line.** On other operating systems, there will be similar ways of calling this.

For running them in a **Docker container**, Docker has to be installed and the specleanr Docker image has to be built using the Dockerfile contained in this repository.

For running them via OGC HTTP API, they have to be deployed on an instance of the platform [pygeoapi](https://pygeoapi.io). The examples in this README use `curl`, but the requests will be the same for any other HTTP client.



## (1) getdata.R

* Note: All our pygeoapi tests seem to use default `percentage_correctness` and default `synonym_check`

### Required arguments

These arguments are required, in this order:

```
Rscript getdata.R \
  in_data_path in_species_column \
  in_database in_gbif_lim in_inat_lim in_vert_lim in_verbose \
  in_extent \
  in_percent_correct in_synonym_check in_warn_check \
  out_result_path
```

The result is stored as a CSV in `out_result_path` !

There are two ways to pass the input species:

* You can either provide a **list of species** as a string. In this case, set the column name to `null`.
* Or you can provide a **CSV file** (path or URL) with species occurrences and provide the column name where the species names are found.

There are these ways to pass the geographical extent:

* You can either provide it as **string**: `xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500`
* Or you can provide a **GeoJSON file**: `danube.geojson` (path to file on local filesystem or URL to remote GeoJSON file)
* Or you can provide a **shapefile**: `danube.shp` (path to file on local filesystem)
* If the shapefile is remote, i.e. if you provide a URL, it has to be **zipped**: `danube.zip`

### Example input data

To run it **remotely,** using URLs for the input data, you have to store it somewhere where it can be accessed via URL. In our case, we have example species data:

* https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_occurrences_short.csv (relevant column: `species`)
* https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv (relevant column: `speciesname`)

We also have a zipped shape as example extent:

* https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip

### Case 1: Test with species list (and string extent)

Works (all three tested on 2025-07-29, I only checked whether a result was returned - not whether the result was correct and inside the extent).

**From command line:**

Pass a list of species (and then set the second parameter, which is the column name, to "null"):

```
Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat" "20" "20" "20" "true" \
  "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

```
docker run -v './in:/in" "-v" "./out:/out' -e 'R_SCRIPT=getdata.R" "specleanr:latest" "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" "gbif,inat" "20" "20" "20" "True" "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" "80" "null" "True" "/out/biodiv-data-test.csv'
```

**Via HTTP API:**

```
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_bbox": {"bbox": [42.08333, 8.15250, 50.24500, 29.73583]},
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "databases": ["gbif", "inat"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20
    }
}'
```

### Case 2a: Test with species list (and local shp file as extent) 

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

Pass a list of species (and then set the second parameter, which is the column name, to "null"):

```
Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "true" \
  "./danube/danube.shp" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested from command line on 2025-07-29 (Merret)


```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" specleanr:20250722 "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" "gbif,inat,vertnet" "20" "20" "20" "True" "/in/danube.shp" "80" "null" "True" "/out/biodiv-data-test.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 2b: Test with species list (and remote shp file as extent) 

**From command line:**

Works: Tested from command line on 2025-07-30 (Merret)

Pass a list of species (and then set the second parameter, which is the column name, to "null"):

```
Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "true" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/download/in:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" specleanr:20250722 "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" "gbif,inat,vertnet" "20" "20" "20" "True" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" "80" "null" "True" "/out/biodiv-data-4d4e23dd-6d20-11f0-8f3f-fa163e42fba0.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)

```
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_shapefile": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip",
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20
    }
}'
```

### Case 3a: Test with species list (and local geojson file as extent) 

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "inat" "20" "20" "20" "true" \
  "./danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" "inat" "20" "20" "20" "True" "/in/danube_from_boku.geojson" "80" "null" "True" "/out/biodiv-data-test.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 3b: Test with species list (and remote geojson file as extent) 

**From command line:**

Works: Tested via command line on 2025-07-30 (Merret)

```
Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "inat" "20" "20" "20" "true" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" "gbif,inat,vertnet" "20" "20" "20" "True" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" "80" "null" "True" "/out/biodiv-data-test.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)

```
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_geojson_url": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson",
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20
    }
}'
```

### Case 4a: Test with local species csv file (and string extent)

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript getdata.R \
  "./jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "/in/jdsdata.csv" "speciesname" "gbif,inat,vertnet" "20" "20" "20" "True" "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" "80" "null" "True" "/out/biodiv-data-272468f6-6d26-11f0-83ba-fa163e42fba0.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 4b: Test with remote species csv file (and string extent)

**From command line:**

Works: Tested from command line on 2025-07-30 (Merret)

```
Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" "gbif,inat,vertnet" "20" "20" "20" "True" "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" "80" "null" "True" "/out/biodiv-data-test.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)

```
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_bbox": {"bbox": [42.08333, 8.15250, 50.24500, 29.73583]},
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv",
        "colname_species": "speciesname",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20
    }
}'
```

### Case 5a: Test with local species csv file (and local shp file as extent)

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript getdata.R \
  "./jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "./danube/danube.shp" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```
**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "/in/jdsdata.csv" "speciesname" "gbif,inat,vertnet" "20" "20" "20" "True" "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" "80" "null" "True" "/out/biodiv-data-272468f6-6d26-11f0-83ba-fa163e42fba0.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 5b: Test with remote species csv file (and remote zipped shp file as extent)

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/download/in:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" "gbif,inat,vertnet" "20" "20" "20" "True" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" "80" "null" "True" "/out/biodiv-data-272468f6-6d26-11f0-83ba-fa163e42fba0.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)


```
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_shapefile": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip",
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv",
        "colname_species": "speciesname",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20
    }
}'
```

### Case 6a: Test with remote species csv file (and local geojson file as extent)

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "./danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" "gbif,inat,vertnet" "20" "20" "20" "True" "/in/danube_from_boku.geojson" "80" "null" "True" "/out/biodiv-data-test.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 6b: Test with remote species csv file (and remote geojson file as extent)

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" "gbif,inat,vertnet" "20" "20" "20" "True" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" "80" "null" "True" "/out/biodiv-data-test.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)

```
curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_geojson_url": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson",
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv",
        "colname_species": "speciesname",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20
    }
}'
```



## (2) matchdata.R

### Required arguments

These arguments are required, in this order:

```
Rscript matchdata.R \
  in_data_paths_or_urls \
  in_colnames_species_names in_colnames_countries in_colnames_lat in_colnames_lon in_colnames_date \
  in_verbose_bool out_result_path
```

The result is stored as a CSV in `out_result_path` !

### Example input data

How to get input data for testing locally: To test with some local CSV input data, you can grab data from the specleanr package, and store it locally:

```
# Prepare the input data in R:
library(specleanr)
data(efidata) # Data extract from EFIPLUS data
data(jdsdata) # Data extract from JDS4 data
data.table::fwrite(efidata , file = './efidata.csv')
data.table::fwrite(jdsdata , file = './jdsdata.csv')
```

To run it using URLs for the input data, you have to store the input data somewhere where it can be accessed via URL. In our case, we have:

* https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv
* https://aquainfra.ogc.igb-berlin.de/exampledata/boku/efidata.csv

### Case 1a: Local CSV input

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript matchdata.R \
  "efidata.csv,jdsdata.csv" \
  "speciesname, scientificName" "JDS4_sampling_ID, country" "lat, lati" "lon, long" "sampling_date,Date" \
  "TRUE" "./result_matchdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku/:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=matchdata.R" "specleanr:latest" "/in/efidata.csv,/in/jdsdata.csv" "speciesname,scientificName"  "JDS4_sampling_ID, country" "lat, lati" "lon, long" "sampling_date,Date" "TRUE" "/out/result_matchdata-test.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 1b: Remote CSV input

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

```
Rscript matchdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv,https://aquainfra.ogc.igb-berlin.de/exampledata/boku/efidata.csv" \
  "speciesname, scientificName" "JDS4_sampling_ID" "lat, lati" "lon, long" "sampling_date,Date" \
  "TRUE" "./result_matchdata.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=matchdata.R" "specleanr:latest" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/efidata.csv,https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname,scientificName" "JDS4_sampling_ID, country" "lat, lati" "lon, long" "sampling_date,Date" "TRUE" "/out/result_matchdata-test.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)

```
curl --location 'http://localhost:5000/processes/match-data/execution' \
--header 'Content-Type: application/json' \
--data '{ 
    "inputs": {
        "input_datasets": ["https://aquainfra.ogc.igb-berlin.de/exampledata/boku/efidata.csv", "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv"],
        "colnames_species_names": ["speciesname", "scientificName"],
        "colnames_countries": ["JDS4_sampling_ID"],
        "colnames_lat": ["lat", "latitude"],
        "colnames_lon": ["lon", "long", "longitude"],
        "colnames_date": ["Date", "sampling_date"]
    }
}'
```



## (3) checknames.R

### Required arguments

These arguments are required, in this order:

```
Rscript checknames.R \
  in_data_path in_colname_species \
  in_percent_correctness \
  in_bool_merge in_bool_verbose \
  in_synonymn_checks in_ecosystem_checks in_rm_duplicates \
  out_result_path_names
```

The result is stored as a CSV in `out_result_path_names` !

Just like in `getdata.R`, there are two ways to pass the input species:

* You can either provide a **list of species** as a string. In this case, set the column name to `null`.
* Or you can provide a **CSV file** (path or URL) with species occurrences and provide the column name where the species names are found.

### Example input data

You don't necessarily need input data - you can simply use a string list as input. If you want to test with a CSV file, just take the result of `matchdata`.

### ALL OK: Case 1: String input, merge=False

Also: percent correctness 70, synonym check=True, ecosystem check=True, rm duplicates True

**From command line:**

Works: Tested from command line on 2025-07-29 (Merret)

Pass a list of species (and then set the second parameter to `"null"`):

```
Rscript checknames.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "70" \
  "false" "true" "true" "true" "true" \
  "./result_checknames.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=checknames.R" "specleanr:latest" "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" "70" "false" "true" "true" "true" "true" "/out/result_matchdata-test.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)

```
curl --location 'http://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "percent_correctness": 70,
        "bool_merge": false,
        "bool_synonymn": true,
        "bool_ecosystem_type": true,
        "bool_rm_duplicates": true
    }
}'
```

### ALL OK: Case 2a: File input (local), merge=True

As input, I use 

**From command line:**

Pass the result CSV of `matchdata`, for example:

Works: Tested via command line on 2025-07-30 (Merret)

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
Rscript checknames.R \
  "matched-biodiv-data-example.csv" "species" \
  "70" \
  "true" "true" \
  "true" "true" "true" \
  "./result_checknames.csv"
```

**Run the Docker container:**

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

Works: Tested via docker on 2025-07-30 (Merret)

```
docker run -v "/var/www/nginx/exampledata/boku:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=checknames.R" "specleanr:20250722" "/in/matched-biodiv-data-example.csv" "species" "70" "True" "True" "True" "True" "True" "/out/checked-biodiv-data-test.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### ALL OK: Case 2b: File input (remote), merge=True

**From command line:**

Pass the result CSV of `matchdata`, for example:

Works: Tested via command line on 2025-07-30 (Merret)

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
Rscript checknames.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" \
  "70" \
  "true" "true" \
  "true" "true" "true" \
  "./result_checknames.csv"
```

Also works if we set synonymn_checks, ecosystem_checks, rm_duplicates to `false`:

```
Rscript checknames.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" \
  "70" \
  "true" "true" \
  "false" "false" "false" \
  "./result_checknames.csv"
```

**Run the Docker container:**

Works: Tested via docker on 2025-07-30 (Merret)

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
docker run -v "/var/www/nginx/download/in:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=checknames.R" "specleanr:20250722" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" "70" "True" "True" "True" "True" "True" "/out/checked-biodiv-data-test.csv"
```

Also works if we set synonymn_checks, ecosystem_checks, rm_duplicates to `false`:

```
docker run -v "/var/www/nginx/download/in:/in" -v "/var/www/nginx/download/out:/out" -e "R_SCRIPT=checknames.R" "specleanr:20250722" "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" "70" "True" "True" "False" "False" "False" "/out/checked-biodiv-data-test.csv"
```

**Via HTTP API:**

Works: Tested via pygeoapi on 2025-07-30 (Merret)

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
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
```

Also works if we set synonymn_checks, ecosystem_checks, rm_duplicates to `false`:

```
curl --location 'http://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv",
        "colname_species": "species",
        "percent_correctness": 70,
        "bool_merge": true,
        "bool_synonymn": true
    }
}'
```

