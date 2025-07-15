## R script running the function "getdata" from
## the specleanr package, to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin and Anthony Basooma, BOKU Vienna.
##
## AquaINFRA Project, October 2025
##



# To test, run this script in bash with:
# Rscript getdata.R "studyarea.shp" "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla" "50" "50" "50" "data_gbif_vert_inat.csv"
print('Starting wrapper script: getdata.R.')
#print(paste('DEBUG: R package sources: libPaths()', .libPaths()))
library(specleanr)
#print('DEBUG: Successfully imported library specleanr')

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path        = args[1] #can be a dataset or species list. If species list then the colsp is empty
in_species_column   = args[2] # if the data is a dataframe then the colsp should be provided. Otherwise NULL by default
in_database         = args[3] #databases to consider (gbif, inat, and vertnet)
in_gbif_lim         = args[4] # e.g. "50"
in_inat_lim         = args[5] # e.g. "50"
in_vert_lim         = args[6] # e.g. "50"
in_verbose          = args[7] # logical TRUE or FALSE: EXECUTION messages
in_extent           = args[8] #provide either a shaepfile to act as a polygon bounding box or use a bounding box (xmin, ymin, xmax, ymax)
in_percent_correct  = as.numeric(args[9]) #allowed percentage correctness of species names. Used for checknames fn
in_synonym_check    = args[10] #allow synoymns or not from FishBase
in_warn_check       = args[11] #logical
out_result_path     = args[12]


#check if the bounding box is provided in a form of vector (xmin, ymin, xmax, ymax). otherwise load from shapefile

if(tolower(in_extent)=='null'){ #

  study_area = NULL
  
}else if(startsWith(in_extent, 'http') | file.exists(in_extent)){

  # (1) Read data from shapefile
# TODO Test, can st_read also read GeoJSON? It should?
print(paste('Reading input data from shapefile or GeoJSON:',in_extent))
#print(paste('DEBUG: Content of directory ', dirname(in_data_path), ':', paste(list.files(dirname(in_data_path)), collapse=", ")))
study_area <- sf::st_read(in_extent, quiet=TRUE)


  }else{
    #study_area <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package = 'specleanr'), quiet=TRUE)
  x = in_extent
  print('Spliting the string to form a vector of bounding box. Must provide a string of named e.g. "xmin=8.15250, ymin=42.08333, xmax=29.73583, ymax=50.24500"')
  study_area = eval(parse(text = paste0("list(", x, ")")))
  
}

if(!is(in_data_path, 'vector')){
speciesdata <- sf::st_read(in_data_path, quiet=TRUE)
}else{
  # (2) Remove spaces and split:
print(paste('Splitting input arg species names...'))
in_data_path = gsub(", ", ",", in_data_path, fixed = TRUE)
in_data_path = gsub(" ,", ",", in_data_path, fixed = TRUE)
speciesdata = strsplit(in_data_path, ",")[[1]]
}


#database list
in_database = gsub(", ", ",", in_database, fixed = TRUE)
in_database = gsub(" ,", ",", in_database, fixed = TRUE)
in_database = strsplit(in_database, ",")[[1]]




# (3) Make limits numeric
in_gbif_lim = as.numeric(in_gbif_lim)
in_inat_lim = as.numeric(in_inat_lim)
in_vert_lim = as.numeric(in_vert_lim)


# (3) Run getdata:
print('Running specleanr::getdata...')
df_online <- getdata(
  data     = speciesdata, 
  colsp    = in_species_column,
  extent   = study_area,
  db       = in_database,
  gbiflim  = in_gbif_lim,
  inatlim  = in_inat_lim,
  vertlim  = in_vert_lim,
  verbose  = in_verbose,
  sn = in_synonym_check,
  warn = in_warn_check,
  pct =  in_percent_correct
  )
print('Running specleanr::getdata... DONE.')


# (4) Write the result to csv file:
print(paste0('Write result to csv file: ', out_result_path))
data.table::fwrite(df_online , file = out_result_path)
