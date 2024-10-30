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
# Rscript checknames.R "matched-biodiv-data.csv" "species" "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla" "70" "true"  "data_checked1.csv" "data_checked2.csv"

library(specleanr)

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path = args[1]
in_colname_species = args[2] # e.g. "species"
in_species_names = args[3] # e.g. "Squalius cephalus, Salmo trutta, Thymallus thymallus, Anguilla anguilla"
in_pct = args[4] # e.g. "70"
in_bool_merge = args[5] # e.g. "true"
out_result_path_names = args[6]
out_result_path_filtered = args[7]


# (1) Read data from CSV or from URL
print(paste('Reading input data from CSV...'))
mergealldfs <- data.table::fread(in_data_path)


# (2) Make string booleans boolean
in_bool_merge = tolower(in_bool_merge) == 'true'


# (3) Remove spaces and split:
print(paste('Splitting input arg species names...'))
in_species_names = gsub(", ", ",", in_species_names, fixed = TRUE)
in_species_names = gsub(" ,", ",", in_species_names, fixed = TRUE)
in_species_names = strsplit(in_species_names, ",")[[1]]


# (4) Run check_names:
cleannames_df <- check_names(data = mergealldfs, colsp = in_colname_species, pct = as.numeric(in_pct), merge = in_bool_merge)
#cleannames_df <- check_names(data = mergealldfs, colsp = 'species', pct = 70, merge = TRUE)


# (5) Filter result:
speciesfiltered <- cleannames_df[cleannames_df$speciescheck %in% in_species_names,]
# speciesfiltered <- cleannames_df[cleannames_df$speciescheck %in% c("Squalius cephalus", 'Salmo trutta', "Thymallus thymallus","Anguilla anguilla"),]


# (6) Write the results to csv file:
print(paste0('Write result (cleannames_df) to csv file: ', out_result_path_names))
data.table::fwrite(cleannames_df , file = out_result_path_names)
print(paste0('Write result (speciesfiltered) to csv file: ', out_result_path_filtered))
data.table::fwrite(speciesfiltered , file = out_result_path_filtered)
