# How to call the scripts in this directory

Merret Buurman, IGB Berlin, 2025-07-14


The R scripts in this directory are not part of the library - they are examples on how to use the functions defined in the library, and they are scripts that can be called from a commandline (e.g. bash in Linux). These scripts are included in the specleanr Dockerfile, and they are called as part of the OGC processes (AquaINFRA project, pygeoapi platform).

## How to call (Linux command line)

These are examples how the scripts in `src/bla.R` can be executed from the **Linux command line.** On other operating systems, there will be similar ways of calling this.

### (1) getdata.R

```
Rscript getdata.R https://aqua.igb-berlin.de/referencedata/aqua90m/species_occurrences_cuba_maxine.csv species gbif,inat,vertnet 20 20 20 TRUE 50,6,51,7 30 TRUE TRUE ./result_getdata.csv
```

### (2) matchdata.R

```
Rscript matchdata.R in_data_paths_or_urls in_colnames_species_names in_colnames_countries in_colnames_lat in_colnames_lon in_colnames_date in_verbose_bool out_result_path
```

To test with some CSV input data, you can grab data from the package, and store it locally:

```
# Prepare the input data, in R:

library(specleanr)
data(efidata) #Data extract from EFIPLUS data
data(jdsdata) #Data extract from JDS4 data
data.table::fwrite(efidata , file = './efidata.csv')
data.table::fwrite(jdsdata , file = './jdsdata.csv')
```

Then, from command line:

```
Rscript matchdata.R "efidata.csv,jdsdata.csv" "speciesname, scientificName" "JDS4_sampling_ID, country" "lat, lati" "lon, long" "sampling_date,Date" TRUE ./result_matchdata.csv
```

To run it remotely, using URLs for the input data, you have to store it somewhere where it can be accessed via URL. In our case, we have:

* https://ourserver.de/exampledata/boku/jdsdata.csv
* https://ourserver.de/exampledata/boku/efidata.csv


Run it from command line:

```
Rscript matchdata.R "https://ourserver.de/exampledata/boku/jdsdata.csv,https://ourserver.de/exampledata/boku/efidata.csv" "speciesname, scientificName" "JDS4_sampling_ID" "lat, lati" "lon, long" "sampling_date,Date" TRUE ./result_matchdata.csv
```

Now, the result should be visible in `result_matchdata.csv` !


### (3) checknames.R

### (4) pred_extract.R

### (5) multidetect.R

