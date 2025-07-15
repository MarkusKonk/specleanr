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
out_result_path_filtered  = args[10]


# Make string booleans boolean
in_bool_merge = tolower(in_bool_merge) == 'true'
in_bool_verbose = tolower(in_bool_verbose) == 'true'


# If there is NO column name, we assume that a list of species is given.
# Otherwise, we assume an input CSV file is given.
if (tolower(in_colname_species) == 'na') {
  in_colname_species <- NULL
  if (in_bool_verbose) message('DEBUG: No column name given, so we assume the user passed a list of species...')
  if (in_bool_verbose) message('DEBUG: Splitting input arg species names...')
  in_species_names <- in_data_path
  in_species_names <- gsub(", ", ",", in_species_names, fixed = TRUE)
  in_species_names <- gsub(" ,", ",", in_species_names, fixed = TRUE)
  in_species_names <- strsplit(in_species_names, ",")[[1]]
  if (in_bool_verbose) message('DEBUG: Found species names: ', paste(in_species_names, collapse=' + '))
  mergealldfs <- in_species_names
} else {
  if (in_bool_verbose) message('DEBUG: A column name was given, so we assume the user passed a file...')
  if (in_bool_verbose) message(paste0('DEBUG: Reading input data from CSV: ', in_data_path))
  mergealldfs <- data.table::fread(in_data_path)
  in_species_names <- in_colname_species
}

print(in_colname_species)

# Run check_names:
if (in_bool_verbose) message('DEBUG: Running specleanr::check_names...')
cleannames_df <- check_names(
  data = mergealldfs,
  colsp = in_species_names,
  pct = as.numeric(in_pct),
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

