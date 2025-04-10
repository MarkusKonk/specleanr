## R script running the function "getdata" from
## the specleanr package, to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin
##
## AquaINFRA Project, October 2024
##



# To test, run this script in bash with:
# Rscript getdata.R "studyarea.shp" "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla" "50" "50" "50" "data_gbif_vert_inat.csv"
print('Starting wrapper script: getdata.R.')
#print(paste('DEBUG: R package sources: libPaths()', .libPaths()))
library(specleanr)
#print('DEBUG: Successfully imported library specleanr')

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path = args[1]
in_species_names = args[2] # e.g. "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla"
in_gbif_lim = args[3] # e.g. "50"
in_inat_lim = args[4] # e.g. "50"
in_vert_lim = args[5] # e.g. "50"
out_result_path = args[6]


# (1) Read data from shapefile
# TODO Test, can st_read also read GeoJSON? It should?
print(paste('Reading input data from shapefile or GeoJSON:', in_data_path))
#print(paste('DEBUG: Content of directory ', dirname(in_data_path), ':', paste(list.files(dirname(in_data_path)), collapse=", ")))
study_area <- sf::st_read(in_data_path, quiet=TRUE)
#study_area <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package = 'specleanr'), quiet=TRUE)


# (2) Remove spaces and split:
print(paste('Splitting input arg species names...'))
in_species_names = gsub(", ", ",", in_species_names, fixed = TRUE)
in_species_names = gsub(" ,", ",", in_species_names, fixed = TRUE)
in_species_names = strsplit(in_species_names, ",")[[1]]


# (3) Make limits numeric
in_gbif_lim = as.numeric(in_gbif_lim)
in_inat_lim = as.numeric(in_inat_lim)
in_vert_lim = as.numeric(in_vert_lim)


# (3) Run getdata:
print('Running specleanr::getdata...')
df_online <- getdata(
  data = in_species_names, 
  extent = study_area,
  gbiflim = in_gbif_lim,
  inatlim = in_inat_lim,
  vertlim = in_vert_lim,
  verbose = F)
print('Running specleanr::getdata... DONE.')


# (4) Write the result to csv file:
print(paste0('Write result to csv file: ', out_result_path))
data.table::fwrite(df_online , file = out_result_path)
