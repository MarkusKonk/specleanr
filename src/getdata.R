## R script running the function "getdata" from
## the specleanr package, to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin and Anthony Basooma, BOKU Vienna.
##
## AquaINFRA Project, October 2025
##


# To test, run this script in bash with:
#
# Rscript getdata.R "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" \
#  "gbif,inat,vertnet" "20" "20" "20" "TRUE" \
#  "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500" \
#  "30" "TRUE" "TRUE" "./result_getdata.csv"


message('Starting wrapper script: getdata.R.')
#print(paste('DEBUG: R package sources: libPaths()', .libPaths()))
library(specleanr)
#print('DEBUG: Successfully imported library specleanr')

##################################
### Get command line arguments ###
### as strings                 ###
##################################

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path        = args[1] # can be a dataset or species list. If species list then the colsp is empty
in_species_column   = args[2] # if the data is a dataframe then the colsp should be provided. Otherwise "null"
in_database         = args[3] # list of databases to consider (gbif, inat, and vertnet)
in_gbif_lim         = args[4] # e.g. "50"
in_inat_lim         = args[5] # e.g. "50"
in_vert_lim         = args[6] # e.g. "50"
in_verbose          = args[7] # logical: verbose or not
in_extent           = args[8] # provide either "null" or shapefile or geojson (to act as a polygon bounding box) or a bounding box (xmin, ymin, xmax, ymax)
in_percent_correct  = args[9] # allowed percentage correctness of species names. Used for checknames fn
in_synonym_check    = args[10] # logical: allow synoymns or not from FishBase
in_warn_check       = args[11] # logical: ??
out_result_path     = args[12] # path where output CSV will be written

########################
### Read input data: ###
### study_area       ###
########################

# Check if the bounding box is NULL,
# or provided in a form of named vector (xmin, ymin, xmax, ymax).
# Otherwise load from shapefile

# study area is NULL:
if(tolower(in_extent)=='null'){

  study_area = NULL
  
# study area is a file:
}else if(startsWith(in_extent, 'http') | file.exists(in_extent)){

  # If the URL points to a zipped shapefile, download and unzip before we can read it:
  if (startsWith(in_extent, 'http') & endsWith(in_extent, 'zip')) {
    message("DEBUG: Downloading and unzipping zipped shapefile: ", in_extent)
    temp_zip <- tempfile(fileext = ".zip")
    download.file(in_extent, temp_zip, mode = "wb")
    unzip(temp_zip, exdir = tempdir())
    in_extent <- list.files(tempdir(), pattern = "\\.shp$", full.names = TRUE)
    message("DEBUG: Downloading and unzipping zipped shapefile... DONE.")

  # If the URL points to a non-zipped shapefile, throw error:
  } else if (startsWith(in_extent, 'http') & endsWith(in_extent, 'shp')) {
    stop('If you specify a remote shapefile as input, please zip it...')
  }

  # Read data from shapefile
  message('DEBUG: Reading input data from shapefile or GeoJSON:',in_extent)
  #message('DEBUG: Content of directory ', dirname(in_data_path), ':', paste(list.files(dirname(in_data_path)), collapse=", "))
  study_area <- sf::st_read(in_extent, quiet=TRUE)
  message('DEBUG: Reading input data from shapefile or GeoJSON... DONE.')

# study area is a bounding box:
}else{
  message("DEBUG: Splitting the string to form a vector of bounding box... (Must be provided as named list, in this format: 'xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500')")
  study_area = eval(parse(text = paste0("list(", in_extent, ")")))
}

########################
### Read input data: ###
### speciesdata      ###
########################

# if species data is a file, read it:
if(startsWith(in_data_path, 'http') | file.exists(in_data_path)) {
  message("DEBUG: Reading speciesdata from: ", in_data_path)
  speciesdata <- data.table::fread(in_data_path)
  message("DEBUG: Reading speciesdata... DONE.")

# if species are given as a comma-separated list, remove spaces and split:
}else{
  message('DEBUG: Splitting input arg species names...')
  in_data_path = gsub(", ", ",", in_data_path, fixed = TRUE)
  in_data_path = gsub(" ,", ",", in_data_path, fixed = TRUE)
  speciesdata = strsplit(in_data_path, ",")[[1]]
  message('DEBUG: Splitted input arg species names: ', paste(speciesdata, collapse="+"))
  in_species_column <- NULL
}

################################
### Convert string arguments ###
### to R data types          ###
################################

# List of database names (remove spaces and split)
in_database = gsub(", ", ",", in_database, fixed = TRUE)
in_database = gsub(" ,", ",", in_database, fixed = TRUE)
in_database = strsplit(in_database, ",")[[1]]
message('DEBUG: Splitted input arg database names: ', paste(in_database, collapse="+"))

# Make numerics from string:
in_gbif_lim = as.numeric(in_gbif_lim)
in_inat_lim = as.numeric(in_inat_lim)
in_vert_lim = as.numeric(in_vert_lim)
in_percent_correct = as.numeric(in_percent_correct)

# Make booleans from string:
in_verbose       = tolower(in_verbose) == 'true'
in_synonym_check = tolower(in_synonym_check) == 'true'
in_warn_check    = tolower(in_warn_check) == 'true'


##############################
### Run specleanr function ###
##############################

if (in_verbose) {
  message("DEBUG: Logging all input args to getdata():")
  message("DEBUG:   data = ", speciesdata)
  message("DEBUG:   colsp = ", in_species_column)
  message("DEBUG:   extent = ", study_area)
  message("DEBUG:   db = ", in_database)
  message("DEBUG:   lims = ", paste(in_gbif_lim, in_vert_lim, in_inat_lim, collapse=", "))
  message("DEBUG:   verbose = ", in_verbose)
  message("DEBUG:   in_synonym_check = ", in_synonym_check)
  message("DEBUG:   in_warn_check = ", in_warn_check)
  message("DEBUG:   in_percent_correct = ", in_percent_correct)
  message('DEBUG:   Running specleanr::getdata...')
}

message('DEBUG: Running specleanr::getdata...')
df_online <- getdata(
  data     = speciesdata, 
  colsp    = in_species_column,
  extent   = study_area,
  db       = in_database,
  gbiflim  = in_gbif_lim,
  inatlim  = in_inat_lim,
  vertlim  = in_vert_lim,
  verbose  = in_verbose,
  sn   = in_synonym_check,
  warn = in_warn_check,
  pct  = in_percent_correct
  )
message('DEBUG: Running specleanr::getdata... DONE.')


# Write the result to csv file:
message('DEBUG: Write result to csv file: ', out_result_path)
data.table::fwrite(df_online , file = out_result_path)
message('DEBUG: Write result to csv file... DONE.')
message('DEBUG: Finished wrapper script getdata.R.')
