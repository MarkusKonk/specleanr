## R script combining the functions "multidetect" and "extract_clean_data" from
## the specleanr package, to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin
##
## AquaINFRA Project, October 2024
##



# To test, run this script in bash with:
# Rscript pred_extract.R "bla.csv" "bla.tiff" "decimalLatitude" "decimalLongitude" "speciescheck" "10" "true" "false" "true" "bladata.csv"

library(specleanr)

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path_or_url = args[1]
in_raster_path = args[2]
in_shape_path = args[3]
in_colname_lat = args[4] # e.g. "decimalLatitude"
in_colname_lon = args[5] # e.g. "decimalLongitude"
in_colname_species = args[6] # e.g. "speciescheck"
in_min_pts = args[7] # e.g. "10"
in_bool_merge = args[8] # e.g. "FALSE"
in_bool_list = args[9] # e.g. "TRUE"
out_result_path = args[10]



# (1) Read data from CSV or from URL
print(paste('Reading input data from CSV...'))
speciesfiltered <- data.table::fread(in_data_path_or_url)


# (2) Read raster from ...
worldclim <- terra::rast(in_raster_path)
#worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package = 'specleanr'))


# (3) Read bbox from shapefile
# TODO: Whole shapefile just for bbox? Better?
print(paste('Reading input data from shapefile...'))
study_area <- sf::st_read(in_shape_path, quiet=TRUE)
#danube <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package = 'specleanr'), quiet=TRUE)


# (4) Make string booleans boolean
in_bool_merge = tolower(in_bool_merge) == 'true'
in_bool_list = tolower(in_bool_list) == 'true'


# (5) Run pred_extract
print(paste('Run pred_extract...'))
print(paste('in_colname_lat:', in_colname_lat))
print(paste('in_colname_lon:', in_colname_lon))
print(paste('in_colname_species:', in_colname_species))
print(paste('in_min_pts:', in_min_pts))
print(paste('in_bool_list:', in_bool_list))
print(paste('in_bool_merge:', in_bool_merge))
multiprecleaned <- pred_extract(
  data = speciesfiltered, 
  raster = worldclim, 
  lat = in_colname_lat,
  lon = in_colname_lon,
  colsp = in_colname_species,
  bbox  = study_area,  
  list = in_bool_list, 
  minpts = as.numeric(in_min_pts),
  merge = in_bool_merge)

#multipreclened <- pred_extract(
#  data= speciesfiltered, 
#  raster= worldclim, 
#  lat = 'decimalLatitude',
#  lon = 'decimalLongitude',
#  colsp = 'speciescheck',
#  bbox  = danube,  
#  multiple = TRUE, 
#  list= TRUE, 
#  minpts = 10,
#  merge = FALSE)
#names(multipreclened)
#[1] "Salmo trutta"        "Anguilla anguilla"   "Squalius cephalus"  "Thymallus thymallus"


# (6) Write the result to csv file:
print(paste0('Write result to csv file: ', out_result_path))
data.table::fwrite(multiprecleaned , file = out_result_path)

