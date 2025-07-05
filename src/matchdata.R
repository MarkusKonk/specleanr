## R script running the function "match_datasets" from
## the specleanr package, to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin
##
## AquaINFRA Project, October 2024
##
## TODO: Currently this only allowes one user-defined dataset!


# To test, run this script in bash with:
# Rscript matchdata.R "some_inputs.csv" "some_more_inputs.csv" "speciesname, scientificName" "JDS4_sampling_ID" "lat, lati" "lon, long" "data_matched.csv"
print('Starting wrapper script: matchdata.R')
library(specleanr)


# I created test data with:
#data(efidata) #Data extract from EFIPLUS data
#data(jdsdata) #Data extract from JDS4 data
#data.table::fwrite(efidata , file = './efidata.csv')
#data.table::fwrite(jdsdata , file = './jdsdata.csv')
#danube <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package = 'specleanr'), quiet=TRUE)
#df_online <- specleanr::getdata(data = c("Squalius cephalus", "Salmo trutta", "Thymallus thymallus", "Anguilla anguilla"),
#  bbox = danube, gbiflim = 50, inatlim = 50, vertlim = 50, verbose = F)
#data.table::fwrite(df_online , file = './df_online.csv')


args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path_or_url_online   = args[1]
in_data_path_or_url_user     = args[2]
in_colnames_species_names    = args[3] # e.g. "speciesname, scientificName"
in_colnames_countries        = args[4] # e.g. "JDS4_sampling_ID"
in_colnames_lat              = args[5] # e.g. "lat, lati"
in_colnames_lon              = args[6] # e.g. "lon, long"
out_result_path              = args[7]
in_verbose_bool              = args[8]

in_verbose_bool      =  "true"



in_verbose_bool   = tolower(in_verbose_bool) == 'true'


# (1) Remove spaces and split:
print(paste('Splitting input arg species names...'))
in_colnames_species_names = gsub(", ", ",", in_colnames_species_names, fixed = TRUE)
in_colnames_species_names = gsub(" ,", ",", in_colnames_species_names, fixed = TRUE)
in_colnames_species_names = strsplit(in_colnames_species_names, ",")[[1]]
in_colnames_countries = gsub(", ", ",", in_colnames_countries, fixed = TRUE)
in_colnames_countries = gsub(" ,", ",", in_colnames_countries, fixed = TRUE)
in_colnames_countries = strsplit(in_colnames_countries, ",")[[1]]
in_colnames_lat = gsub(", ", ",", in_colnames_lat, fixed = TRUE)
in_colnames_lat = gsub(" ,", ",", in_colnames_lat, fixed = TRUE)
in_colnames_lat = strsplit(in_colnames_lat, ",")[[1]]
in_colnames_lon = gsub(", ", ",", in_colnames_lon, fixed = TRUE)
in_colnames_lon = gsub(" ,", ",", in_colnames_lon, fixed = TRUE)
in_colnames_lon = strsplit(in_colnames_lon, ",")[[1]]


# (2) Read data from CSV or from URL
print(paste('Reading input data from CSV...'))
df_online <- data.table::fread(in_data_path_or_url_online)
data_from_user <- data.table::fread(in_data_path_or_url_user)


# (3) Run match_datasets:
print('Running specleanr:match_datasets...')
mergealldfs <- match_datasets(
  datasets = list(
    user = data_from_user,
    onlinedata = df_online),
  country = in_colnames_countries,
  lats = in_colnames_lat,
  lons = in_colnames_lon,
  species = in_colnames_species_names)

  datasets,
  country = NULL,
  lats = NULL,
  lons = NULL,
  species = NULL,
  date = NULL,
  verbose = FALSE
print('Running specleanr:match_datasets... DONE.')

# mergealldfs <- match_datasets(
#  datasets = list(
#    efi= efidata,
#    jds = jdsdata,
#    onlinedata = df_online),
#  country = c('JDS4_sampling_ID', 'country', 'Land'),
#  lats = c('lat', 'lati'),
#  lons = c('lon', 'long', 'lo'),
#  species = c('speciesname', 'scientificName'))


# (4) Write the result to csv file:
print(paste0('Write result to csv file: ', out_result_path))
data.table::fwrite(mergealldfs , file = out_result_path)
