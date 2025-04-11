# Pygeoapi Processes Howto


These are some how-tos mainly as reference to ourselves... Frequently needed commands.

## Quickly update process

On the server, go to the directory containing the repo and pull the newest changes from GitHub:

```
# Go to the directory:
cd /opt/.../pygeoapi/pygeoapi/process/specleanr

# Make sure you are on the correct branch:
git status # possibly git stash...
git checkout main

# Pull changes
git pull
```

If something inside the Docker image changed, re-build the docker image:

```
# image name must correspond to the name hard-coded in the process file:
docker build -t specleanr .

# for debugging build issues:
#docker build --no-cache --progress=plain -t specleanr .

# add another tag to the image that keeps the current date:
today=$(date '+%Y%m%d')
docker build -t specleanr:${today} .

```

If something outside the Docker image also changed, re-install the changes on pygeoapi:

```
# activate virtual environment:
cd /opt/.../pygeoapi
source ../venv3/bin/activate

# install changes:
pip install .

# restart pygeoapi:
sudo systemctl restart pygeoapi
```



## Quickly add process

To add a new process, of course it needs to be in the repo and the Dockerfile needs to be built etc.

Plus:

Make sure the config.json contains everything we need:

```
vi /opt/.../pygeoapi/config.json
```

Add the new process(es) to `pygeoapi-config.yml` and `plugin.py`:

```
vi /opt/.../pygeoapi/pygeoapi-config.yml
vi /opt/.../pygeoapi/pygeoapi/plugin.py"

```

Install added process, so that pygeoapi can import them:

```
# activate virtual environment:
cd /opt/.../pygeoapi
source ../venv3/bin/activate

# install changes:
pip install .
```

Generate a new `pygeoapi-openapi.yml` (after pip installing! virtual env must be active!)

```
export PYGEOAPI_CONFIG=pygeoapi-config.yml
export PYGEOAPI_OPENAPI=pygeoapi-openapi.yml
date; pygeoapi openapi generate $PYGEOAPI_CONFIG --output-file $PYGEOAPI_OPENAPI

```


Then restart pygeoapi:

```
# restart pygeoapi:
sudo systemctl restart pygeoapi
```


## Test the image

getdata script:

docker rm bokutest; /usr/bin/docker run --name bokutest -v "/var/www/nginx/download/in:/in" -v "/var/www/nginx/download/out:/out" -e R_SCRIPT=getdata.R specleanr "/in/basinfinal.zip" "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla" "50" "50" "50" "/out/blaresult.csv"



# How to build and run using Docker...?

## How to build the docker image

For this you have to be inside the directory that contains the file "Dockerfile"!

```
docker build -t specleanr:today ./
```

## How to run the docker containers


For this, you need the following input data inside the directory `/path/to/data/directory/`, which will be bind-mounted into the docker container:

* `basinfinal.shp`
* `efidata.csv`
* `worldclim.tiff`

The docker container will store outputs in the same directory. In a slightly more sophisticated setting, you can use `/in` for static or external inputs, and `out` for data written by these dockerized scripts (which may be inputs into the next script then).

(1/5) Getdata args:

```
docker run --name boku1 -v /path/to/data/directory:/data -e R_SCRIPT=getdata.R specleanr:today "/data/basinfinal.shp" "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla" "50" "50" "50" "/data/1_getdataresult.csv"
```

(2/5) Matchdata:

```
docker run --name boku2 -v /path/to/data/directory:/data -e R_SCRIPT=matchdata.R specleanr:today "/data/1_getdataresult.csv" "/data/efidata.csv" "speciesname, scientificName" "JDS4_sampling_ID" "lat, latitude" "lon, long, longitude" "/data/2_matchdataresult.csv"
```

(3/5) Checknames:

```
docker run --name boku3 -v /path/to/data/directory:/data -e R_SCRIPT=checknames.R specleanr:today "/data/2_matchdataresult.csv" "species" "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla" "70" "true" "/data/3_checknamesresult_names.csv" "/data/3_checknamesresult_filtered.csv"
```

(4/5) Pred-extract:

```
docker run --name boku4 -v /path/to/data/directory:/data -e R_SCRIPT=pred_extract.R specleanr:today "/data/3_checknamesresult_filtered.csv" "/data/worldclim.tiff" "/data/basinfinal.shp" "decimalLatitude" "decimalLongitude" "speciescheck" "10" "false" "false" "/data/4_predextractresult.csv"
```

(5/5) Multidetect:

```
docker run --name boku5 -v /path/to/data/directory:/data -e R_SCRIPT=multidetect.R specleanr:today "/data/4_predextractresult.csv" "bio6" "false" "" "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel, iforest, lof, mahal" "false" "0.1" "0.7" "species" "/data/5_multidetectresult.csv"
```


