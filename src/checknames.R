## R script running the function "check_names" from
## the specleanr package, and filter the results,
## to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin
##
## AquaINFRA Project, October 2024
##

# To test, run this script in bash with:
# Rscript checknames.R "Alburnus alburnus, Abramis brama, Cyprinus carpio, Esox lucius" "null" "70" "false" "true" "true" "true" "true" "./result_checknames.csv"

message('Starting wrapper script: checknames.R.')
library(specleanr)


##################################
### Get command line arguments ###
### as strings                 ###
##################################

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path              = args[1] # a URL or a list of species
in_colname_species        = args[2] # column name, e.g. "species", or "null"
in_percent_correctness    = args[3] # number, e.g. "70"
in_bool_merge             = args[4] # logical, e.g. "true"
in_bool_verbose           = args[5] # logical, e.g. "true"
in_synonymn_checks        = args[6] # logical, e.g. "true"
in_ecosystem_checks       = args[7] # logical, e.g. "true"
in_rm_duplicates          = args[8] # logical, e.g. "true"
out_result_path_names     = args[9] # path where output CSV will be written


################################
### Convert string arguments ###
### to R data types          ###
################################

# Make booleans from string:
in_bool_merge = tolower(in_bool_merge) == 'true'
in_bool_verbose = tolower(in_bool_verbose) == 'true'
in_synonymn_checks = tolower(in_synonymn_checks) == 'true'
in_ecosystem_checks = tolower(in_ecosystem_checks) == 'true'
in_rm_duplicates = tolower(in_rm_duplicates) == 'true'

# Make numeric from string:
in_percent_correctness = as.numeric(in_percent_correctness)


########################
### Read input data: ###
### speciesdata      ###
########################

# if species data is a file, read it:
if(startsWith(in_data_path, 'http') | file.exists(in_data_path)) {
  message("DEBUG: Reading speciesdata from CSV: ", in_data_path)
  species_names_or_df <- data.table::fread(in_data_path)
  message("DEBUG: Reading speciesdata from CSV... DONE.")

# if species are given as a comma-separated list, remove spaces and split:
} else {
  in_colname_species <- NULL
  message('DEBUG: Splitting input arg species names...')
  in_data_path = gsub(", ", ",", in_data_path, fixed = TRUE)
  in_data_path = gsub(" ,", ",", in_data_path, fixed = TRUE)
  species_names_or_df = strsplit(in_data_path, ",")[[1]]
  message('DEBUG: Splitted input arg species names: ', paste(species_names_or_df, collapse="+"))

  if(length(species_names_or_df)<2 ){
    stop('The number of species should be greater than 1 and nodataframe can be exported')
  }
}


##############################
### Run specleanr function ###
##############################

message('DEBUG: Verbosity? ', in_bool_verbose)

if (in_bool_verbose) {
  message("DEBUG: Logging all input args to check_names():")
  # Log a data table, or a list of strings:
  message('DEBUG:   data    = of type "', typeof(species_names_or_df), '"')
  if (data.table::is.data.table(species_names_or_df)) {
    message('DEBUG:   data    = of class "data.table"')
    message('DEBUG:   data    = columns: ', paste(names(species_names_or_df), collapse=','))
    message('DEBUG:   data    = first line: ', paste(species_names_or_df[1], collapse=','))
  } else if (typeof(species_names_or_df) == typeof(c('bla'))) {
    message('DEBUG:   data    = ', paste(species_names_or_df, collapse=', '))
  }
  # Log all other, simpler, objects:
  message('DEBUG:   colsp   = of type "', typeof(in_colname_species), '": ', in_colname_species)
  message('DEBUG:   pct     = of type "', typeof(in_percent_correctness), '": ', in_percent_correctness)
  message('DEBUG:   merge   = of type "', typeof(in_bool_merge), '": ', in_bool_merge)
  message('DEBUG:   verbose = of type "', typeof(in_bool_verbose), '": ', in_bool_verbose)
  message('DEBUG:   sn      = of type "', typeof(in_synonymn_checks), '": ', in_synonymn_checks)
  message('DEBUG:   ecosystem = of type "', typeof(in_ecosystem_checks), '": ', in_ecosystem_checks)
  message('DEBUG:   rm_duplicates = of type "', typeof(in_rm_duplicates), '": ', in_rm_duplicates)
}

if (in_bool_verbose) message('DEBUG: Running specleanr::check_names...')
cleannames_df <- check_names(
  data = species_names_or_df,
  colsp = in_colname_species,
  pct = in_percent_correctness,
  merge = in_bool_merge,
  verbose = in_bool_verbose,
  sn = in_synonymn_checks,
  ecosystem = in_ecosystem_checks,
  rm_duplicates = in_rm_duplicates
)
if (in_bool_verbose) message('DEBUG: Running specleanr::check_names... DONE.')


# Write the results to csv file:
if (in_bool_verbose) message('DEBUG: Write result to csv file: ', out_result_path_names)
data.table::fwrite(cleannames_df , file = out_result_path_names)
if (in_bool_verbose) message('DEBUG: Write result to csv file... DONE.')
if (in_bool_verbose) message('DEBUG: Finished wrapper script checknames.')
