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

print('Starting wrapper script: pred_extract.R.')
library(specleanr)


##################################
### Get command line arguments ###
### as strings                 ###
##################################

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path_or_url  = args[1]
in_raster_path       = args[2]
in_bbox_path         = args[3]
in_colname_lat       = args[4] # e.g. "decimalLatitude"
in_colname_lon       = args[5] # e.g. "decimalLongitude"
in_colname_species   = args[6] # e.g. "speciescheck"
in_min_pts           = args[7] # e.g. "10"
in_bool_merge        = args[8] # e.g. "FALSE"
in_bool_list         = args[9] # e.g. "TRUE"
in_bool_verbose      = args[10] #logical, FALSE
in_bool_warn         = args[11] #logical, FALSE
in_bool_coords       = args[12] #logical, FALSE
in_na_inform         = args[13] #logical, FALSE
in_na_rm             = args[14] #logical, FALSE
in_rm_duplicates     = args[15] #logical, FALSE
in_minimumpts_rm     = args[16] #logical, FALSE
out_result_path      = args[17]


################################
### Convert string arguments ###
### to R data types          ###
################################

# Make numeric from string:
in_min_pts = as.numeric(in_min_pts)

# Make boolean from string:
in_bool_merge = tolower(in_bool_merge) == 'true'
in_bool_list = tolower(in_bool_list) == 'true'
in_bool_verbose = tolower(in_bool_verbose) == 'true'
in_bool_warn = tolower(in_bool_warn) == 'true'
in_bool_coords = tolower(in_bool_coords) == 'true'
in_na_inform = tolower(in_na_inform) == 'true'
in_na_rm = tolower(in_na_rm) == 'true'
in_rm_duplicates = tolower(in_rm_duplicates) == 'true'
in_minimumpts_rm = tolower(in_minimumpts_rm) == 'true'


########################
### Read input data: ###
########################

# (1) Read data from CSV or from URL
if (in_bool_verbose) message('DEBUG: Reading input data from CSV, from:', in_data_path_or_url)
speciesfiltered <- data.table::fread(in_data_path_or_url)
if (in_bool_verbose) message('DEBUG: Reading input data from CSV... DONE.')


# (2) Read raster from ...
if (in_bool_verbose) message('DEBUG: Reading input raster, from:', in_raster_path)
worldclim <- terra::rast(in_raster_path)
#worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package = 'specleanr'))
if (in_bool_verbose) message('DEBUG: Reading input raster... DONE.')


# (3) Read bbox from shapefile
# If the URL points to a zipped shape, download and unzip before we can read it:
if (startsWith(in_bbox_path, 'http') & endsWith(in_bbox_path, 'zip')) {
  if (in_bool_verbose) message("DEBUG: Downloading zipped shapefile: ", in_bbox_path)
  temp_zip <- tempfile(fileext = ".zip")
  download.file(in_bbox_path, temp_zip, mode = "wb")
  unzip(temp_zip, exdir = tempdir())
  in_bbox_path <- list.files(tempdir(), pattern = "\\.shp$", full.names = TRUE)
} else if (startsWith(in_bbox_path, 'http') & endsWith(in_bbox_path, 'shp')) {
  stop('If you specify a remote shapefile as input, please zip it...')
}

if (in_bool_verbose) message('DEBUG: Reading input data from shapefile or GeoJSON:', in_bbox_path)
study_area <- sf::st_read(in_bbox_path, quiet=TRUE)
if (in_bool_verbose) message('DEBUG: Reading input data from shapefile... DONE.')


##############################
### Run specleanr function ###
##############################

if (in_bool_verbose) {
  message("DEBUG: Logging all input args to pred_extract():")
  #message("DEBUG: data   = ", speciesfiltered)
  message('DEBUG: raster = ', worldclim)
  message('DEBUG: lat    = ', in_colname_lat)
  message('DEBUG: lon    = ', in_colname_lon)
  message('DEBUG: colsp  = ', in_colname_species)
  message('DEBUG: bbox   = ', study_area)
  message('DEBUG: list   = ', in_bool_list)
  message('DEBUG: minpts = ', in_min_pts)
  message('DEBUG: mp     = ', in_minimumpts_rm)
  message('DEBUG: rm_duplicates = ', in_rm_duplicates)
  message('DEBUG: merge     = ', in_bool_merge)
  message('DEBUG: warn      = ', in_bool_warn)
  message('DEBUG: verbose   = ', in_bool_verbose)
  message('DEBUG: coords    = ', in_bool_coords)
  message('DEBUG: na.inform = ', in_na_inform)
  message('DEBUG: na.rm     = ', in_na_rm)
}

if (in_bool_verbose) message('DEBUG: Running specleanr::pred_extract...')
multiprecleaned <- pred_extract(
  data = speciesfiltered, 
  raster = worldclim, 
  lat = in_colname_lat,
  lon = in_colname_lon,
  colsp = in_colname_species,
  bbox  = study_area,  
  list = in_bool_list, 
  minpts = in_min_pts,
  mp     = in_minimumpts_rm,
  rm_duplicates = in_rm_duplicates,
  merge = in_bool_merge,
  warn = in_bool_warn,
  verbose= in_bool_verbose,
  coords = in_bool_coords,
  na.inform = in_na.inform,
  na.rm = in_na_rm)
if (in_bool_verbose) message('DEBUG: Running specleanr::pred_extract... DONE.')


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


# Write the result to csv file:
if (in_bool_verbose) message('DEBUG: Write result to csv file: ', out_result_path)
data.table::fwrite(multiprecleaned , file = out_result_path)
if (in_bool_verbose) message('DEBUG: Write result to csv file... DONE.')
if (in_bool_verbose) message('DEBUG: Finished wrapper script pred_extract')
