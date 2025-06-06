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

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path_or_url = args[1]
in_colname_var = args[2] # e.g. "Sepal.Length
in_bool_multiple_species = args[3] # e.g. "TRUE"
in_colnames_exclude = args[4] # e.g. "Species"
in_methods = args[5] # e.g. "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel, iforest, lof, mahal"
in_silence_true_errors = args[6] # e.g. "TRUE"
in_missingness = as.numeric(args[7]) # e.g. 0.1
in_threshold = args[8] # can be "0.7", then loess is set to FALSE. Can be "NULL", then loess is set to TRUE
in_colname_species = args[9]
out_result_path = args[10]
#out_summary_path = args[10]


# Note: I created the csv for testing (with artificial outliers) with this code:
#irisdata1 <- iris
#> #add outlier data and NAs
#rowsOutNA1 <- data.frame(x= c(344, NA,NA, NA), 
#                         x2 = c(34, 45, 544, NA), 
#                         x3= c(584, 5, 554, NA),
#                        x4 = c(575, 4554,474, NA), 
#                        x5 =c('Setosa', 'Setosa', 'Setosa', "Setosa"))
# colnames(rowsOutNA1) <- colnames(irisdata1)
# dfinal <- rbind(irisdata1, rowsOutNA1)
# data.table::fwrite(dfinal , file = './test_inputs_multidetect.csv')
#
# Example taken from vignette:
# https://github.com/AnthonyBasooma/specleanr/blob/master/vignettes/generaloutlier.Rmd


# (1) Read data from CSV or from URL
print(paste('Reading input data from CSV...'))
dfinal <- data.table::fread(in_data_path_or_url)


# (2) Remove spaces and split:
print(paste('Splitting input arg methods...'))
in_methods = gsub(", ", ",", in_methods, fixed = TRUE)
in_methods = gsub(" ,", ",", in_methods, fixed = TRUE)
in_methods = strsplit(in_methods, ",")[[1]]
in_colnames_exclude = gsub(", ", ",", in_colnames_exclude, fixed = TRUE)
in_colnames_exclude = gsub(" ,", ",", in_colnames_exclude, fixed = TRUE)
in_colnames_exclude = strsplit(in_colnames_exclude, ",")[[1]]

# (3) Make string booleans boolean
in_bool_multiple_species = tolower(in_bool_multiple_species) == 'true'
in_silence_true_errors = tolower(in_silence_true_errors) == 'true'


# Only provide column name for species if multiple = FALSE
if (!in_bool_multiple_species) {
  print('Setting species column name to NULL (only needed in case of multiple species).')
  in_colname_species <- NULL
}


# (4) Run multidetect
print(paste('Running specleanr::multidetect...'))
print(paste('Var:', in_colname_var))
print(paste('Multiple:', in_bool_multiple_species))
print(paste('Colname Species:', in_colname_species))
print(paste('Exclude columns:', paste0(in_colnames_exclude, collapse=' + ')))
print(paste('Missingness:', in_missingness))
print(paste('SilenceErrors:', in_silence_true_errors))
print(paste('Methods:', paste0(in_methods, collapse=' + ')))
outlieriris_mult <- multidetect(
  data = dfinal,
  var = in_colname_var,
  multiple = in_bool_multiple_species, # are we checking for one or several species!
  var_col = in_colname_species, # if multiple is true!
  exclude = in_colnames_exclude, # removes columns that are not numeric.
  methods = in_methods,
  silence_true_errors = in_silence_true_errors, # show execution errors and therefore for multiple species the code will break if one of the methods fails to execute.
  missingness = in_missingness) # threshold for how many NAs...
print(paste('Running specleanr::multidetect... DONE.'))


# (5) Run extract_clean_data
if (tolower(in_threshold) == 'null') {
  # if loess=TRUE, then no threshold!
  print('Threshold is null, using loess=TRUE...')
  in_threshold <- NULL
  in_bool_loess <- TRUE
} else if (!(is.na(as.numeric(in_threshold)))) {
  # if loess=FALSE, then set threshold!
  in_threshold <- as.numeric(in_threshold)
  print(paste0('Threshold is a number (', in_threshold, '), using loess=FALSE...'))
  in_bool_loess <- FALSE
}
print(paste('Running specleanr::extract_clean_data...'))
cleandata2 <- extract_clean_data(
  refdata = dfinal,
  outliers = outlieriris_mult,
  loess = in_bool_loess,
  threshold = in_threshold,
  var = in_colname_species)
print(paste('Running specleanr::extract_clean_data... DONE.'))


# (6) Write summary to txt file
# Note: I don't know how to get the string summary. The object is an S4 object, and I cannot access the string.
#print(paste0('Write result to txt file: ', out_summary_path))
#fileConn<-file(out_summary_path)
#writeLines(outlieriris_mult, fileConn)
#close(fileConn)


# (7) Write the result to csv file:
print(paste0('Write result to csv file: ', out_result_path))
data.table::fwrite(cleandata2 , file = out_result_path)



#> outlieriris_mult
#====================================== 
#  Data cleaning summary 
#======================================
# Number of species        : 1 
# Number of methods        : 10 
# Methods used             : mixediqr,logboxplot,iqr,distboxplot,jknife,semiqr,hampel,iforest,lof,mahal
# Multiple                 : FALSE
# Variable                 : Sepal.Length
# Output                   : outlier
# Dataset Name             : dfinal
# Excluded columns         : Species
# ======================================> 

#testing how to fork


