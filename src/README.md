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
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 1"; date; Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat" "20" "20" "20" "true" \
  "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500" \
  "30" "TRUE" "TRUE" "./result_getdata_test1.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 1"; date; docker run "-v" "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat" "20" "20" "20" "True" \
  "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" \
  "30" "True" "True" "/out/biodiv-data-test1.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

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
```

### Case 2a: Test with species list (and local shp file as extent)

**From command line:**

Pass a list of species (and then set the second parameter, which is the column name, to "null"):

```
# Works: Tested on 2025-08-01 (Merret)

echo "getdata test 2a"; date; Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "true" \
  "./danube/danube.shp" \
  "30" "TRUE" "TRUE" "./result_getdata_test2a.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 2a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" "-v" "./out:/out" -e "R_SCRIPT=getdata.R" specleanr:latest \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "/in/danube.shp" \
  "30" "True" "True" "/out/biodiv-data-test2a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 2b: Test with species list (and remote shp file as extent)

**From command line:**

Pass a list of species (and then set the second parameter, which is the column name, to "null"):

```
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 2b"; date; Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "true" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" \
  "30" "TRUE" "TRUE" "./result_getdata_test2b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 2b"; date; docker run -v "./out:/out" -e "R_SCRIPT=getdata.R" specleanr:latest \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" \
  "30" "True" "True" "/out/biodiv-data-test2b.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20,
        "study_area_shapefile": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip",
        "percentage_correctness": 30,
        "synonym_check": true
    }
}'
echo "this was getdata, test case 2b"; date
```

### Case 3a: Test with species list (and local geojson file as extent)

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 3a"; date; Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "inat" "20" "20" "20" "true" \
  "./danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata_test3a.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 3a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "inat" "20" "20" "20" "True" \
  "/in/danube_from_boku.geojson" \
  "30" "True" "True" "/out/biodiv-data-test3a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 3b: Test with species list (and remote geojson file as extent)

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 3b"; date; Rscript getdata.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "true" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata3b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 3b"; date; docker run -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "True" "True" "/out/biodiv-data-test3b.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "databases": ["gbif", "inat", "vertnet"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20,
        "study_area_geojson_url": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson",
        "percentage_correctness": 30,
        "synonym_check": true
    }
}'
echo "this was getdata, test case 3b"; date
```

### Case 4a: Test with local species csv file (and string extent)

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 4a"; date; Rscript getdata.R \
  "./jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500" \
  "30" "TRUE" "TRUE" "./result_getdata_test4a.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 4a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "/in/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" \
  "30" "True" "True" "/out/biodiv-data-test4a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 4b: Test with remote species csv file (and string extent)

**From command line:**

```
# Works: Tested on 2025-07-30 (Merret)

echo "getdata test 4b"; date; Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500" \
  "30" "TRUE" "TRUE" "./result_getdata_test4b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 4b"; date; docker run -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "True" }
  "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" \
  "30" "True" "True" "/out/biodiv-data-test4b.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

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
        "inaturalist_limit": 20,
        "percentage_correctness": 30,
        "synonym_check": true
    }
}'
echo "this was getdata, test case 4b"; date
```

### Case 5a: Test with local species csv file (and local shp file as extent)

**From command line:**

```
# Works: Tested on 2025-07-29 (Merret)

echo "getdata test 5a"; date; Rscript getdata.R \
  "./jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "./danube/danube.shp" \
  "30" "TRUE" "TRUE" "./result_getdata_test5a.csv"
```
**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 5a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "/in/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "xmin=8.1525, ymin=42.08333, xmax=29.73583, ymax=50.245" \
  "30" "True" "True" "/out/biodiv-data-test5a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 5b: Test with remote species csv file (and remote zipped shp file as extent)

**From command line:**

```
# Works: Tested on 2025-07-29 (Merret)

echo "getdata test 5b"; date; Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" \
  "30" "True" "True" "./result_getdata_test5b.csv"
```

Same command with a different input table:

```
# Works: Tested on 2025-08-01 (Merret)

echo "getdata test 5b"; date; Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_occurrences_short.csv" "species" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" \
  "30" "True" "True" "./result_getdata_test5b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 5b"; date; docker run -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/basinfinal.zip" \
  "30" "True" "True" "/out/biodiv-data-test5b.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-04 (Merret)

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
        "inaturalist_limit": 20,
        "percentage_correctness": 30,
        "synonym_check": true
    }
}'
echo "this was getdata, test case 5b"; date
```

### Case 6a: Test with remote species csv file (and local geojson file as extent)

**From command line:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 6a"; date; Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "./danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata6a.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 6a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "/in/danube_from_boku.geojson" \
  "30" "True" "True" "/out/biodiv-data-test6a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 6b: Test with remote species csv file (and remote geojson file as extent)

**From command line:**

```
# Works: Tested on 2025-07-29 (Merret)

echo "getdata test 6b"; date; Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "TRUE" "TRUE" "./result_getdata_test6b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 6b"; date; docker run -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "20" "20" "20" "True" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "True" "True" "/out/biodiv-data-test6b.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-04 (Merret)

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
        "inaturalist_limit": 20,
        "percentage_correctness": 30,
        "synonym_check": true
    }
}'
echo "this was getdata, test case 6b"; date
```

### Case 7: Test with synonym check false...

(TODO: Ask Anthony whether this case makes sense...)

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 7"; date; Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "5" "5" "5" "TRUE" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "FALSE" "TRUE" "./result_getdata_test7.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 7"; date; docker run -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat,vertnet" "5" "5" "5" "TRUE" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "30" "False" "True" "/out/biodiv-data-test7.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-04 (Merret)

curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_geojson_url": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson",
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv",
        "colname_species": "speciesname",
        "databases": ["gbif", "inat"],
        "gbif_limit": 5,
        "vertnet_limit": 5,
        "inaturalist_limit": 5,
        "percentage_correctness": 30,
        "synonym_check": false
    }
}'
echo "this was getdata, test case 7"; date
```

### Case 8: Test with different percentage correctness...

(TODO: Ask Anthony whether this case makes sense...)

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "getdata test 8"; date; Rscript getdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat" "20" "20" "20" "TRUE" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "90" "TRUE" "TRUE" "./result_getdata_test8.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-04 (Merret)

echo "getdata test 8"; date; docker run -v "./out:/out" -e "R_SCRIPT=getdata.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" "speciesname" \
  "gbif,inat" "20" "20" "20" "True" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "90" "True" "True" "/out/biodiv-data-test8.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-04 (Merret)

curl --location 'http://localhost:5000/processes/retrieve-biodiversity-data/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "study_area_geojson_url": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson",
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv",
        "colname_species": "speciesname",
        "databases": ["gbif", "inat"],
        "gbif_limit": 20,
        "vertnet_limit": 20,
        "inaturalist_limit": 20,
        "percentage_correctness": 90,
        "synonym_check": true
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
  in_bool_verbose out_result_path
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

```
# Works: Tested on 2025-08-05 (Merret)

echo "matchdata test 1a"; date; Rscript matchdata.R \
  "efidata.csv,jdsdata.csv" \
  "speciesname, scientificName" "JDS4_sampling_ID, country" "lat, lati" "lon, long" "sampling_date,Date" \
  "TRUE" "./result_matchdata_test2a.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "matchdata test 1a"; date; docker run -v "/var/www/nginx/exampledata/boku/:/in" -v "./out:/out" -e "R_SCRIPT=matchdata.R" "specleanr:latest" \
  "/in/efidata.csv,/in/jdsdata.csv" \
  "speciesname,scientificName"  "JDS4_sampling_ID, country" "lat, lati" "lon, long" "sampling_date,Date" \
  "TRUE" "/out/result_matchdata-test2a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 1b: Remote CSV input

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "matchdata test 1b"; date; Rscript matchdata.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv,https://aquainfra.ogc.igb-berlin.de/exampledata/boku/efidata.csv" \
  "speciesname, scientificName" "JDS4_sampling_ID" "lat, lati" "lon, long" "sampling_date,Date" \
  "TRUE" "./result_matchdata_test1b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "matchdata test 1b"; date; docker run -v "./out:/out" -e "R_SCRIPT=matchdata.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/efidata.csv,https://aquainfra.ogc.igb-berlin.de/exampledata/boku/jdsdata.csv" \
  "speciesname,scientificName" "JDS4_sampling_ID, country" "lat, lati" "lon, long" "sampling_date,Date" \
  "TRUE" "/out/result_matchdata-test1b.csv"
```

**Via HTTP API:**

```
Works: Tested on 2025-08-05 (Merret)

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
echo "This was matchdata, test 1b"; date
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

You don't necessarily need input data - you can simply use a string list as input. If you want to test with a CSV file, just take the result of `matchdata`. One example is stored here:

* https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv

### Case 1: String input, merge=False

* Pass a list of species (and then set the second parameter to `"null"`):
* Also: percent correctness 70, synonym check=True, ecosystem check=True, rm duplicates=True

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "checknames test 1"; date; Rscript checknames.R \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "70" \
  "False" "True" \
  "True" "True" "True" \
  "./result_checknames_test1.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "checknames test 1"; date; docker run -v "./out:/out" -e "R_SCRIPT=checknames.R" "specleanr:latest" \
  "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
  "70" \
  "false" "true" \
  "true" "true" "true" \
  "/out/result_matchdata-test1.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

curl --location 'http://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius",
        "percent_correctness": 70,
        "bool_merge": false,
        "bool_synonym": true,
        "bool_ecosystem_type": true,
        "bool_rm_duplicates": true
    }
}'
echo "This was checknames, test 1", date
```

### Case 2a: File input (local), merge=True

* Pass the result CSV of `matchdata`, for example:
* Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "checknames test 2a"; date; Rscript checknames.R \
  "matched-biodiv-data-example.csv" "species" \
  "70" \
  "True" "True" \
  "True" "True" "True" \
  "./result_checknames_test2a.csv"
```

**Run the Docker container:**

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
# Works: Tested via docker on 2025-08-05 (Merret)

echo "checknames test 2a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=checknames.R" "specleanr:latest" \
  "/in/matched-biodiv-data-example.csv" "species" \
  "70" \
  "True" "True" \
  "True" "True" "True" \
  "/out/checked-biodiv-data-test2a.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 2b: File input (remote), merge=True

**From command line:**

* Pass the result CSV of `matchdata`, for example:
* Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
# Works: Tested on 2025-08-05 (Merret)

echo "checknames test 2b"; date; Rscript checknames.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" \
  "70" \
  "True" "True" \
  "True" "True" "True" \
  "./result_checknames_test2b.csv"
```

Also works if we set synonymn_checks, ecosystem_checks, rm_duplicates to `false`:

```
# Works: Tested on 2025-08-05 (Merret)

echo "checknames test 2b"; date; Rscript checknames.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" \
  "70" \
  "True" "True" \
  "False" "False" "False" \
  "./result_checknames_test2b.csv"
```

**Run the Docker container:**

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
# Works: Tested on 2025-08-05 (Merret)

echo "checknames test 2b"; date; docker run -v "./out:/out" -e "R_SCRIPT=checknames.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" \
  "70" \
  "True" "True" \
  "True" "True" "True" \
  "/out/checked-biodiv-data-test2b.csv"
```

Also works if we set synonymn_checks, ecosystem_checks, rm_duplicates to `false`:

```
# Works: Tested on 2025-08-05 (Merret)

echo "checknames test 2b"; date; docker run -v "./out:/out" -e "R_SCRIPT=checknames.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv" "species" \
  "70" \
  "True" "True" \
  "False" "False" "False" \
  "/out/checked-biodiv-data-test2b.csv"
```

**Via HTTP API:**

Here we set synonymn_checks, ecosystem_checks, rm_duplicates to `true`:

```
# Works: Tested on 2025-08-05 (Merret)

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
# Works: Tested on 2025-08-05 (Merret)

curl --location 'http://localhost:5000/processes/check-names/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/matched-biodiv-data-example.csv",
        "colname_species": "species",
        "percent_correctness": 70,
        "bool_merge": true,
        "bool_synonym": false,
        "bool_ecosystem_type": false,
        "bool_rm_duplicates": false
    }
}'
```



## (4) pred_extract.R

### Required arguments

These arguments are required, in this order:

```
Rscript pred_extract.R \
  in_data_path_or_url \
  in_raster_path \
  in_bbox_path \
  in_colname_lat in_colname_lon in_colname_species \
  in_min_pts \
  in_bool_merge in_bool_list in_bool_verbose \
  in_bool_warn in_bool_coords \
  in_na_inform in_na_rm \
  in_rm_duplicates in_minimumpts_rm \
  out_result_path
```

Then, the result is stored as a CSV in `out_result_path` !

### Example input data

Get the path to raster file from the _specleanr_ package, by installing the package and then running:

```
system.file('extdata/worldclim.tiff', package='specleanr')
```

Download these files:

* Use as input GeoJSON: https://services-eu1.arcgis.com/bzJgidEyiimiAx34/arcgis/rest/services/danube_shp/FeatureServer/0/query?where=1%3D1&outFields=*&f=geojson
* Use as input species data: https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_for_pred_extract.csv
* Use as input species data: https://drive.boku.ac.at/seafhttp/files/62946130-51b1-4d00-98cf-8d79ff46d5db/jdshttptest.csv (link expired)

### Case 1a: All local input files

**From command line:**

```
# Works: Tested on 2025-08-01 (Merret)

echo "pred_extract test 1a"; date; Rscript pred_extract.R \
  "./species_for_pred_extract.csv" \
  "./worldclim.tiff" \
  "./danube_from_boku.geojson" \
  "decimalLatitude" "decimalLongitude" "species" \
  "10" \
  "True" "False" "True" \
  "True" "True" \
  "True" "True" \
  "False" "False" \
  "./result_pred_extract_test1a.csv"
```

**Run the Docker container:**

```
Works: Tested on 2025-07-31 (Merret)

echo "pred_extract test 1a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=pred_extract.R" "specleanr:latest" \
  "/in/species_for_pred_extract.csv" \
  "/in/worldclim.tiff" \
  "/in/danube_from_boku.geojson" \
  "decimalLatitude" "decimalLongitude" "species" \
  "10" \
  "True" "False" "True" \
  "True" "True" \
  "True" "True" \
  "False" "False" \
  "/out/result_pred_extract_test1a.csv"
```

**Via HTTP API:**

* Cannot run via HTTP API using local input file

### Case 1b: All remote input files

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "pred_extract test 1b"; date; Rscript pred_extract.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_for_pred_extract.csv" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/worldclim.tiff" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "decimalLatitude" "decimalLongitude" "species" \
  "10" \
  "True" "False" "True" \
  "True" "True" \
  "True" "True" \
  "False" "False" \
  "./result_pred_extract_test1b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "pred_extract test 1b"; date; docker run -v "./out:/out" -e "R_SCRIPT=pred_extract.R" "specleanr:latest" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_for_pred_extract.csv" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/worldclim.tiff" \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/danube_from_boku.geojson" \
  "decimalLatitude" "decimalLongitude" "species" \
  "10" \
  "True" "False" "True" \
  "True" "True" \
  "True" "True" \
  "False" "False" \
  "/out/result_pred_extract_test1b.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

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
echo "this was pred_extract test 1b"; date;
```

### Case 2: Static raster on server

**From command line:** Same as test 2a, because it just reads the raster from the local file system.

**Run the Docker container:** Same as test 2a, because it just reads the raster from the local file system.

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

curl --location 'http://localhost:5000/processes/pred-extract/execution' \
--header 'Content-Type: application/json' \
--data '{
    "inputs": {
        "input_data": "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/species_for_pred_extract.csv",
        "input_raster_url_or_name": "worldclim",
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
echo "This was pred_extract case 2"; date
```


## (5) multidetect.R

### Required arguments

These arguments are required, in this order:

```
Rscript multidetect.R \
  in_data_path_or_url in_var_ofinterest \
  in_select_var in_bool_multiple_species in_output_type \
  in_group_colname in_colnames_exclude \
  in_methods \
  in_silence_true_errors in_boot_settings_run_bool \
  in_boot_settings_maxrecords in_boot_settings_nb in_boot_settings_seed in_boot_settings_threshold in_pca_settings_exec_bool \
  in_pca_settings_npc in_pca_settings_quiet in_pca_settings_pcvar \
  in_bool_verbose in_warn_bool \
  in_sdm_bool in_na_inform_bool in_missingness in_bool_loess in_threshold_clean \
  in_mode_clean in_classifymode in_eif_bool in_autoextract \
  out_result_path
```

The result is stored as a CSV in `out_result_path` !

For convenience, here's the same but with indication how many arguments are on which line...

```
Rscript multidetect.R \
  2 in_data_path_or_url in_var_ofinterest \
  3 in_select_var in_bool_multiple_species in_output_type \
  2 in_group_colname in_colnames_exclude \
  1 in_methods \
  2 in_silence_true_errors in_boot_settings_run_bool \
  5 in_boot_settings_maxrecords in_boot_settings_nb in_boot_settings_seed in_boot_settings_threshold in_pca_settings_exec_bool \
  3 in_pca_settings_npc in_pca_settings_quiet in_pca_settings_pcvar \
  2 in_bool_verbose in_warn_bool \
  5 in_sdm_bool in_na_inform_bool in_missingness in_bool_loess in_threshold_clean \
  4 in_mode_clean in_classifymode in_eif_bool in_autoextract \
  out_result_path
```

### Example data

How to get input data for testing locally:

```
# download test data from BOKU, and store as "iris1.csv":
wget https://drive.boku.ac.at/f/2a0ede9e03fe4817a9db/?dl=1
# variable column: Sepal.Length
# group column: Species
```

### Comparison of test cases

The three test cases have the same params, except for:

* in_pca_settings_exec_bool (`True` or `False`)
* in_bool_loess (`True` or `False`)
* in_threshold_clean (`0.8` or `null`)
* in_autoextract (`True` or `False`)

```
Rscript multidetect.R \
  "iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" <in_pca_settings_exec_bool> \
                        "True"                      \
                        "False"                     \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" <in_bool_loess> <in_threshold_clean> \
                      "True"          "null"               \
                      "False"         "0.8"                \
                      "False"         "null"               \
  "abs" "med" "False" <in_autoextract> \
                      "True"           \
                      "False"          \
  "./result_multidetect_test.csv"
```

### Case 1a: with loess "True" (this provides optimal threshold)

* This runs the functions `multidetect()`, `extract_clean_data()`

**From command line:**

```
# Not run (will fail due to comments):

echo "multidetect test1"; date; Rscript multidetect.R \
  "iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \ # The last param is True, so pca_settings_exec_bool is True!
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "True" "null" \ # The last params are True and null, so loess is True, and no threshold!
  "abs" "med" "False" "True" \ # The last param is True, so autoextract is True!
  "./result_multidetect_test1a.csv"
```

Without comments, good for copy-pasting:

```
# Works: Tested on 2025-08-05 (Merret)

echo "multidetect test1"; date; Rscript multidetect.R \
  "iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "True" "null" \
  "abs" "med" "False" "True" \
  "./result_multidetect_test1a.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "test multidetect 1a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=multidetect.R" specleanr:latest \
  "/in/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "True" "null" \
  "abs" "med" "False" "True" \
 "/out/cleaned-data-test1a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 1b: with loess "True" (this provides optimal threshold)

* This runs the functions `multidetect()`, `extract_clean_data()`

**From command line:**

```
# Not run (will fail due to comments):

echo "multidetect test 1b"; date; Rscript multidetect.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \ # The last param is True, so pca_settings_exec_bool is True!
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "True" "null" \ # The last params are True and null, so loess is True, and no threshold!
  "abs" "med" "False" "True" \ # The last param is True, so autoextract is True!
  "./result_multidetect_test1b.csv"
```

Without comments, good for copy-pasting:

```
# Works: Tested on 2025-08-05 (Merret)

echo "multidetect test 1b"; date; Rscript multidetect.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "True" "null" \
  "abs" "med" "False" "True" \
  "./result_multidetect_test1b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "test multidetect 1b"; date; docker run -v "./out:/out" -e "R_SCRIPT=multidetect.R" specleanr:latest \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "True" "null" \
  "abs" "med" "False" "True" \
 "/out/cleaned-data-test1b.csv"
```

**Via HTTP API:**

```
# Works: Tested on 2025-08-05 (Merret)

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
echo "test multidetect 1b"; date
```

### Case 2a: With loess "False" and threshold 0.8 (local input file)

* This runs the functions `multidetect()`, `extract_clean_data()`

**From command line:**

```
# Not run (will fail due to comments):

echo "multidetect test 2a:"; date; Rscript multidetect.R \
  "iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  # In the previous line, the last param is True, so pca_settings_exec_bool is True! \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "0.8" \
  # In the previous line, the last params are False and 0.8, so no loess, and threshold is 0.8! \
  "abs" "med" "False" "True" \
  # In the previous line, the last param is True, so autoextract is True!
  "./result_multidetect_test2a.csv"
```

Without comments, good for copy-pasting:

```
# Works: Tested on 2025-08-05 (Merret)

echo "multidetect test 2a:"; date; Rscript multidetect.R \
  "iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "0.8" \
  "abs" "med" "False" "True" \
  "./result_multidetect_test2a.csv"
```

Warning: `In FUN(X[[i]], ...) : The 3 rows for Setosa are less than variables and some methods may not function properly.`


**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "test multidetect 2a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=multidetect.R" specleanr:latest \
  "/in/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "0.8" \
  "abs" "med" "False" "True" \
 "/out/cleaned-data-test2a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 2b: With loess "False" and threshold 0.8 (remote input files)

**From command line:**

This gives out a warning: `Warning message: In FUN(X[[i]], ...) : The 3 rows for Setosa are less than variables and some methods may not function properly.`

```
# Works: Tested on 2025-08-05 (Merret)

echo "multidetect test 2b:"; date; Rscript multidetect.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "0.8" \
  "abs" "med" "False" "True" \
  "./result_multidetect_test2b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "test multidetect 2b"; date; docker run -v "./out:/out" -e "R_SCRIPT=multidetect.R" specleanr:latest \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "0.8" \
  "abs" "med" "False" "True" \
 "/out/cleaned-data-test2b.csv"
```

**Via HTTP API:**

TODO Anthony: The warning is not sent to the user - ok?

```
# Works: Tested on 2025-08-05 (Merret)

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
        "bool_loess": false,
        "threshold_clean": 0.8,
        "outlierweights_mode": "abs",
        "classifymode": "med",
        "eif_bool": false,
        "classify_or_autoremove": true
    }
}'
echo "multidetect test 2b"; date
```

### Case 3a: With loess "False" and threshold 0.8 (local input file)

* This runs the functions `multidetect()`, `classify_data()`

**From command line:**

```
# Not run (will fail due to comments):

echo "multidetect test 3a:"; date; Rscript multidetect.R \
  "iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "False" \
  # In the previous line, the last param is False, so no pca_settings_exec_bool! \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "null" \
  # In the previous line, the last params are False and null, so no loess and no threshold! \
  "abs" "med" "False" "False" \
  # In the previous line, the last param is False, so no autoextract!
  "./result_multidetect_test3a.csv"
```

Without comments, good for copy-pasting:

```
# Works: Tested on 2025-08-05 (Merret)

echo "multidetect test 3a:"; date; Rscript multidetect.R \
  "iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "False" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "null" \
  "abs" "med" "False" "False" \
  "./result_multidetect_test3a.csv"
```

Warnings:

```
Warning messages:
1: In ocindex(x = outliers, sp = sp, props = TRUE, absolute = TRUE,  :
  The absolute outliers found are suspicious since they are shared by less than 50% of the methods used.
2: In ocindex(x = outliers, sp = sp, props = TRUE, absolute = TRUE,  :
  The absolute outliers found are suspicious since they are shared by less than 50% of the methods used.
3: In ocindex(x = outliers, sp = sp, props = TRUE, absolute = TRUE,  :
  The absolute outliers found are suspicious since they are shared by less than 50% of the methods used.
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "test multidetect 3a"; date; docker run -v "/var/www/nginx/exampledata/boku:/in" -v "./out:/out" -e "R_SCRIPT=multidetect.R" specleanr:latest \
  "/in/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "null" \
  "abs" "med" "False" "False" \
 "/out/cleaned-data-test3a.csv"
```

**Via HTTP API:** Cannot run via HTTP API using local input file

### Case 3b: With loess "False" and threshold 0.8 (remote input file)

* This runs the functions `multidetect()`, `classify_data()`

This causes warning, which is not visible to the user when they use the API:

```
Warning messages:
1: In ocindex(x = outliers, sp = sp, props = TRUE, absolute = TRUE,  :
  The absolute outliers found are suspicious since they are shared by less than 50% of the methods used.
2: In ocindex(x = outliers, sp = sp, props = TRUE, absolute = TRUE,  :
  The absolute outliers found are suspicious since they are shared by less than 50% of the methods used.
3: In ocindex(x = outliers, sp = sp, props = TRUE, absolute = TRUE,  :
  The absolute outliers found are suspicious since they are shared by less than 50% of the methods used.
```

**From command line:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "multidetect test 3b:"; date; Rscript multidetect.R \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "False" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "null" \
  "abs" "med" "False" "False" \
  "./result_multidetect_test3b.csv"
```

**Run the Docker container:**

```
# Works: Tested on 2025-08-05 (Merret)

echo "test multidetect 3b"; date; docker run -v "./out:/out" -e "R_SCRIPT=multidetect.R" specleanr:latest \
  "https://aquainfra.ogc.igb-berlin.de/exampledata/boku/iris1.csv" "Sepal.Length" \
  "null" "True" "outlier" \
  "Species" "null" \
  "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel" \
  "False" "False" \
  "30" "5" "1125" "0.6" "True" \
  "2" "True" "PC1" \
  "True" "True" \
  "True" "True" "1.0" "False" "null" \
  "abs" "med" "False" "False" \
 "/out/cleaned-data-test3b.csv"
```
**Via HTTP API:** 

```
# Works: Tested on 2025-08-05 (Merret)

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
        "bool_loess": false,
        "threshold_clean": null,
        "outlierweights_mode": "abs",
        "classifymode": "med",
        "eif_bool": false,
        "classify_or_autoremove": false
    }
}'
echo "multidetect test 3b"; date
```

