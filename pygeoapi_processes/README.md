
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

The docker container will store outputs in the same directory.

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


