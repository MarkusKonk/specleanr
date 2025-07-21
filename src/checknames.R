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
# Rscript checknames.R "./result_matchdata.csv" "species" "70" "true" "true" "true" "true" "true" "./result_checknames1.csv" "./result_checknames2.csv"
# Rscript checknames.R "species1,species2,species3" "NA"  "70" "true" "true" "true" "true" "true" "./result_checknames1.csv" "./result_checknames2.csv"

message('Starting wrapper script: checknames.R.')
library(specleanr)


##################################
### Get command line arguments ###
### as strings                 ###
##################################

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path              = args[1] # a URL or a list of species
in_colname_species        = args[2] # e.g. "species", or "NA" or "na"
in_percent_correctness    = args[3] # e.g. "70"
in_bool_merge             = args[4] #logical, e.g. "true"
in_bool_verbose           = args[5] #logical, e.g. "true"
in_synonymn_checks        = args[6] #logical, e.g. "true"
in_ecosystem_checks       = args[7] #logical, e.g. "true"
in_rm_duplicates          = args[8] #logical, e.g. "true"
out_result_path_names     = args[9] 


################################
### Convert string arguments ###
### to R data types          ###
################################

# Make booleans from string:
in_bool_merge = tolower(in_bool_merge) == 'true'
in_bool_verbose = tolower(in_bool_verbose) == 'true'
in_synonymn_checks = tolower(in_synonymn_checks) == 'true'
in_ecosystem_checks = tolower(in_synonymn_checks) == 'true'
in_rm_duplicates = tolower(in_rm_duplocates) == 'true'

# Make numeric from string:
in_percent_correctness = as.numeric(in_percent_correctness)

########################
### Read input data: ###
### speciesdata      ###
########################

ä
########################
### Read input data: ###
### speciesdata      ###
########################

# if species data is a file, read it:
if(startsWith(in_data_path, 'http') | file.exists(in_data_path)) {
  message("DEBUG: Reading speciesdata from CSV: ", in_data_path)
  mergealldfs <- data.table::fread(in_data_path)
  message("DEBUG: Reading speciesdata from CSV... DONE.")

# if species are given as a comma-separated list, remove spaces and split:
} else {
  in_colname_species <- NULL
  message('DEBUG: Splitting input arg species names...')
  in_data_path = gsub(", ", ",", in_data_path, fixed = TRUE)
  in_data_path = gsub(" ,", ",", in_data_path, fixed = TRUE)
  mergealldfs = strsplit(in_data_path, ",")[[1]]
  message('DEBUG: Splitted input arg species names: ', paste(speciesdata, collapse="+"))

  if(length(in_species_names)<2 ){
    stop('The number of species should be greater than 1 and nodataframe can be exported')
  }
}


##############################
### Run specleanr function ###
##############################

if (in_bool_verbose) {
  message("DEBUG: Logging all input args to check_names():")
  #message("DEBUG:   data    = ", mergealldfs)
  message("DEBUG:   colsp   = ", in_colname_species)
  message("DEBUG:   pct     = ", in_percent_correctness)
  message("DEBUG:   merge   = ", in_bool_merge)
  message("DEBUG:   verbose = ", in_bool_verbose)
  message("DEBUG:   sn            = ", in_synonymn_checks)
  message("DEBUG:   ecosystem     = ", in_ecosystem_checks)
  message("DEBUG:   rm_duplicates = ", in_rm_duplicates)
}

if (in_bool_verbose) message('DEBUG: Running specleanr::check_names...')
cleannames_df <- check_names(
  data = mergealldfs,
  colsp = in_species_names,
  pct = in_percent_correctness,
  merge = in_bool_merge,
  verbose = in_bool_verbose,
  sn = in_synonymn_checks,
  ecosystem = in_ecosystem_checks,
  rm_duplicates = in_rm_duplicates
)
if (in_bool_verbose) message('DEBUG: Running specleanr::check_names... DONE.')


# Write the results to csv file:
if (in_bool_verbose) message(paste0('Write result (cleannames_df) to csv file: ', out_result_path_names))
data.table::fwrite(cleannames_df , file = out_result_path_names)
if (in_bool_verbose) message('DEBUG: Write result to csv file... DONE.')
if (in_bool_verbose) message('DEBUG: Finished wrapper script getdata.R.')
