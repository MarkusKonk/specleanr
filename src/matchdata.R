## R script running the function "match_datasets" from
## the specleanr package, to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin
##
## AquaINFRA Project, October 2024
##

# To test, run this script in bash with:
# Rscript matchdata.R "some_inputs.csv" "some_more_inputs.csv" "speciesname, scientificName" "JDS4_sampling_ID" "lat, lati" "lon, long" "data_matched.csv"
message('Starting wrapper script: matchdata.R')
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


##################################
### Get command line arguments ###
### as strings                 ###
##################################

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_paths_or_urls        = args[1]
in_colnames_species_names    = args[2] # e.g. "speciesname, scientificName"
in_colnames_countries        = args[3] # e.g. "JDS4_sampling_ID, country"
in_colnames_lat              = args[4] # e.g. "lat, lati"
in_colnames_lon              = args[5] # e.g. "lon, long"
in_colnames_date             = args[6] # e.g. "sampling_date,Date"
in_verbose_bool              = args[7] # e.g. "true"
out_result_path              = args[8] # path where output CSV will be written


################################
### Convert string arguments ###
### to R data types          ###
################################

# Make boolean from string:
in_verbose_bool = tolower(in_verbose_bool) == 'true'

# Remove spaces and split:
if (in_verbose_bool) message('DEBUG: Splitting input args that are lists...')
in_data_paths_or_urls = gsub(", ", ",", in_data_paths_or_urls, fixed = TRUE)
in_data_paths_or_urls = gsub(" ,", ",", in_data_paths_or_urls, fixed = TRUE)
in_data_paths_or_urls = strsplit(in_data_paths_or_urls, ",")[[1]]
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
in_colnames_date = gsub(", ", ",", in_colnames_date, fixed = TRUE)
in_colnames_date = gsub(" ,", ",", in_colnames_date, fixed = TRUE)
in_colnames_date = strsplit(in_colnames_date, ",")[[1]]


########################
### Read input data: ###
########################

# Read data from CSV or from URL
if (in_verbose_bool) message('DEBUG: Reading input data from CSV...')
if (in_verbose_bool) message('DEBUG: Inputs data URLs (or paths): ', paste(in_data_paths_or_urls, collapse=' + '))
all_input_datasets <- lapply(in_data_paths_or_urls, data.table::fread)
names(all_input_datasets) <- basename(in_data_paths_or_urls)
if (in_verbose_bool) message('DEBUG: Names of input datasets (from URLs): ', paste(names(all_input_datasets), collapse=' + '))


##############################
### Run specleanr function ###
##############################

message('DEBUG: Verbosity? ', in_verbose_bool)

if (in_verbose_bool) {
  message('DEBUG: Logging all input args to match_datasets():')
  message('DEBUG:   datasets = list of datasets...')
  message('DEBUG:   datasets = of type: "', typeof(all_input_datasets), '": ...'))
  message('DEBUG:   country  = of type: "', typeof(in_colnames_countries), '": ', paste(in_colnames_countries, collapse=', '))
  message('DEBUG:   lats     = of type: "', typeof(in_colnames_lat), '": ', paste(in_colnames_lat, collapse=', '))
  message('DEBUG:   lons     = of type: "', typeof(in_colnames_lon), '": ', paste(in_colnames_lon, collapse=', '))
  message('DEBUG:   species  = of type: "', typeof(in_colnames_species_names), '": ', paste(in_colnames_species_names, collapse=', '))
  message('DEBUG:   date     = of type: "', typeof(in_colnames_date), '": ', paste(in_colnames_date, collapse=', '))
  message('DEBUG:   verbose  = of type: "', typeof(in_verbose_bool), '": ', in_verbose_bool)
}

if (in_verbose_bool) message('DEBUG: Running specleanr::match_datasets...')
mergealldfs <- match_datasets(
  datasets = all_input_datasets,
  country = in_colnames_countries,
  lats = in_colnames_lat,
  lons = in_colnames_lon,
  species = in_colnames_species_names,
  date = in_colnames_date,
  verbose = in_verbose_bool)
if (in_verbose_bool) message('DEBUG: Running specleanr::match_datasets... DONE.')


# Write the result to csv file:
if (in_verbose_bool) message(paste0('DEBUG: Write result to csv file: ', out_result_path))
data.table::fwrite(mergealldfs , file = out_result_path)
if (in_verbose_bool) message('DEBUG: Write result to csv file... DONE.')
if (in_verbose_bool) message('DEBUG: Finished wrapper script matchdata')
