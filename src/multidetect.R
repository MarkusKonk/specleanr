## R script combining the functions "multidetect" and "extract_clean_data" from
## the specleanr package, to be run from the pygeoapi platform.
##
## Package by Anthony Basooma, BOKU Vienna.
## R script by Merret Buurman, IGB Berlin
##
## AquaINFRA Project, October 2024
##



# To test, run this script in bash with:
# Rscript multidetect.R "test_inputs_multidetect.csv" "Sepal.Length" "Species" "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel, iforest, lof, mahal" 0.1 "data_out_cleandata.csv"
print('Starting wrapper script: multidetect.R.')
library(specleanr)

#req: required field
#opt: optional and defualt settings are appropiate
#req_set: required but the defualt is appropiate
#req_adj: required and data based--user can chnage

#data used to give example: iris data


##################################
### Get command line arguments ###
### as strings                 ###
##################################

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path_or_url                  = args[1] # req
in_var_ofinterest                    = args[2] # req     e.g. "Sepal.Length
in_select_var                        = args[3] # opt     if only particular columns are needed form the datasest
in_bool_multiple_species             = args[4] # opt     e.g. "TRUE"
in_output_type                       = args[5] # req_set e.g., clean or outlier#defualt outlier
in_group_colname                     = args[6] # opt     e.g. if multiple = TRUE, then its neeed. in iris data like "Species
in_colnames_exclude                  = args[7] # opt     e.g.exclude irrelevant columns mostly important for multidimensional data e.g. ID, SN, x, y etc
in_methods                           = args[8] # req     e.g. "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel, iforest, lof, mahal"
in_silence_true_errors               = args[9] # req_set e.g. "TRUE"
in_boot_settings_run_bool            = args[10] #bool
in_boot_settings_maxrecords          = as.numeric(args[11]) #numb or max to initate bootstraping
in_boot_settings_nb                  = as.numeric(args[12]) #numeric e.g 30 defualt
in_boot_settings_seed                = as.numeric(args[13]) #rnadomisation e.g 1123..
in_boot_settings_threshold           = as.numeric(args[14]) #decimal e.g. 0.6
in_pca_settings_exec_bool            = args[15] #bool
in_pca_settings_npc                  = as.numeric(args[16]) #integer..number of pcs retained
in_pca_settings_quiet                = args[17] #bool to print the cummulative variance
in_pca_settings_pcvar                = args[18] #pc used for outlier detectiion defualt is PC1
in_verbose_bool                      = args[19] #implementation messages
in_warn_bool                         = args[20] #return warning msgs
in_sdm_bool                          = args[21] # if multidimensional data then its TRUE. univariate data sdm is F
in_na_inform_bool                    = args[22] # msgs on handling NAs defualt FALSE
in_missingness                       = as.numeric(args[23]) #req_adj# e.g. 0.1

###=========
#Extrain clean data
#===========
in_bool_loess                        = args[24] #e.g.g TRUE or FALSE
in_threshold_clean                   = args[25] #defualt is 0.8
in_mode_clean                        = args[26]#e.g. 'abs', "best"

#classifying data
in_classifymode                      = args[27] #default med
in_eif_bool                          = args[28] #e.g. F/T 
in_autoextract                       = args[29] #TRUE to classfy data

#output
out_result_path = args[30]
##out_summary_path = args[35]

# in_threshold = args[8] # can be "0.7", then loess is set to FALSE. Can be "NULL", then loess is set to TRUE
# in_colname_species = args[9]


########################
### Read input data: ###
########################

# (1) Read data from CSV or from URL
message('DEBUG: Reading input data from CSV file: ', in_data_path_or_url)
dfinal <- data.table::fread(in_data_path_or_url)
message('DEBUG: Reading input data from CSV file... DONE.')


################################
### Convert string arguments ###
### to R data types          ###
################################

# Remove spaces and split:
message('DEBUG: Splitting input args that are lists...')
in_methods = gsub(", ", ",", in_methods, fixed = TRUE)
in_methods = gsub(" ,", ",", in_methods, fixed = TRUE)
in_methods = strsplit(in_methods, ",")[[1]]

if(!in_colnames_exclude==tolower('null')){
in_colnames_exclude = gsub(", ", ",", in_colnames_exclude, fixed = TRUE)
in_colnames_exclude = gsub(" ,", ",", in_colnames_exclude, fixed = TRUE)
in_colnames_exclude = strsplit(in_colnames_exclude, ",")[[1]]
}else{
  in_colnames_exclude <- NULL
}

if(!in_select_var==tolower('null')){
in_select_var = gsub(", ", ",", in_select_var, fixed = TRUE)
in_select_var = gsub(" ,", ",", in_select_var, fixed = TRUE)
in_select_var = strsplit(in_select_var, ",")[[1]]
}else{
  in_select_var<- NULL
}

# Make boolean from string:
in_bool_multiple_species   = tolower(in_bool_multiple_species) == 'true'
in_silence_true_errors     = tolower(in_silence_true_errors) == 'true'
in_warn_bool               = tolower(in_warn_bool) == 'true'
in_na_inform_bool          = tolower(in_na_inform_bool) == 'true'
in_sdm_bool                = tolower(in_sdm_bool) == 'true'
in_pca_settings_exec_bool  = tolower(in_pca_settings_exec_bool) == 'true'
in_boot_settings_run_bool  = tolower(in_boot_settings_run_bool) == 'true'
in_verbose_bool            = tolower(in_verbose_bool) == 'true'
in_pca_settings_quiet      = tolower(in_pca_settings_quiet) == 'true'
in_bool_loess              = tolower(in_bool_loess ) == 'true'
in_eif_bool                = tolower(in_eif_bool) == 'true'

# Assemble required lists:
in_bootSettings = list(run= in_boot_settings_run_bool, nb= in_boot_settings_nb,
                    maxrecords = in_boot_settings_maxrecords, seed= in_boot_settings_seed,
                    th = in_boot_settings_threshold)

in_pc = list(exec = in_pca_settings_exec_bool, npc= in_pca_settings_npc,
          q = in_pca_settings_quiet, pcvar = in_pca_settings_pcvar)


# Only provide column name for group if multiple = FALSE

if (!in_bool_multiple_species) {
  message('DEBUG: Setting group column name to NULL (only needed in case of multiple groups).')
  in_group_colname <- NULL
}


##############################
### Run specleanr function ###
##############################

# Run multidetect
# print(paste('Running specleanr::multidetect...'))
# print(paste('Var:', in_var_ofinterest))
# print(paste('Multiple:', in_bool_multiple_species))
# print(paste('Colname Species:', in_colname_species))
# print(paste('Exclude columns:', paste0(in_colnames_exclude, collapse=' + ')))
# print(paste('Missingness:', in_missingness))
# print(paste('SilenceErrors:', in_silence_true_errors))
# print(paste('Methods:', paste0(in_methods, collapse=' + ')))

message('DEBUG: Running specleanr::multidetect...')
outlieriris_mult <- multidetect(
  data            = dfinal,
  var             = in_var_ofinterest,
  select          = in_select_var,
  output          = in_output_type,
  exclude         = in_colnames_exclude,
  multiple        = in_bool_multiple_species,
  var_col         = in_group_colname,
  methods         = in_methods,
  bootSettings    = in_bootSettings,
  pc              = in_pc,
  verbose         = in_verbose_bool,
  spname          = in_spname_auto,
  warn            = in_warn_bool,
  missingness     = in_missingness,
  silence_true_errors = in_silence_true_errors,
  sdm             = in_sdm_bool,
  na.inform       = in_na.inform_bool
  )
message('DEBUG: Running specleanr::multidetect... DONE.')

if(tolower(in_autoextract)=='false'){

  message('DEBUG: Running specleanr::classify_data...')
  cleandata2 <- classify_data(
  refdata     = dfinal,
  outliers    = outlieriris_mult,
  var_col     = in_group_colname,
  warn        = in_warn_bool,
  verbose     = in_verbose_bool,
  classify    = in_classifymode,
  EIF         = in_eif_bool
)
  message('DEBUG: Running specleanr::classify_data... DONE.')

}else{
  # Run extract_clean_data
if (tolower(in_threshold_clean) == 'null') {
  # if loess=TRUE, then no threshold!
  message('DEBUG: Threshold is null, using loess=TRUE...')
  in_threshold_clean <- NULL
  in_bool_loess <- TRUE
} else if (!(is.na(as.numeric(in_threshold_clean)))) {
  # if loess=FALSE, then set threshold!
  in_threshold_clean <- as.numeric(in_threshold_clean)
  message(paste0('DEBUG: Threshold is a number (', in_threshold_clean, '), using loess=FALSE...'))
  in_bool_loess <- FALSE
}

message('DEBUG: Running specleanr::extract_clean_data...')
cleandata2 <- extract_clean_data(
  refdata             = dfinal,
  outliers            = outlieriris_mult,
  mode                = in_mode_clean,
  var_col             = in_group_colname,
  threshold           = in_threshold_clean,
  warn                = in_warn_bool,
  verbose             = in_verbose_bool,
  loess               = in_bool_loess
)
message('DEBUG: Running specleanr::extract_clean_data... DONE.')
}

# Write summary to txt file
# Note: I don't know how to get the string summary. The object is an S4 object, and I cannot access the string.
#print(paste0('Write result to txt file: ', out_summary_path))
#fileConn<-file(out_summary_path)
#writeLines(outlieriris_mult, fileConn)
#close(fileConn)


# Write the result to csv file:
#print(cleandata2)
if (in_bool_verbose) message('DEBUG: Write result to csv file: ', out_result_path)
data.table::fwrite(cleandata2 , file = out_result_path)
if (in_bool_verbose) message('DEBUG: Write result to csv file... DONE.')
if (in_bool_verbose) message('DEBUG: Finished wrapper script pred_extract')
