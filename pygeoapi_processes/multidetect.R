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

library(jsonlite)
library(specleanr)

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path_or_url = args[1]
in_colname_var = args[2] # e.g. "Sepal.Length
in_colname_exclude = args[3] # e.g. "Species"
in_methods = args[4] # e.g. "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel, iforest, lof, mahal"
in_missingness = as.numeric(args[5]) # eg. 0.1
out_result_path = args[6]
#out_summary_path = args[7]


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
in_methods = gsub(" ", "", in_methods, fixed = TRUE)
in_methods = strsplit(in_methods, ",")[[1]]


# (3) Run multidetect
print(paste('Run multidetect...'))
print(paste('Var:', in_colname_var))
print(paste('Exclude:', in_colname_exclude))
print(paste('Missingness:', in_missingness))
print(paste0('Methods: ', paste0(in_methods, collapse=', ')))
outlieriris_mult <- multidetect(
  data = dfinal,
  var = in_colname_var,
  multiple = FALSE, # TODO Ask: Should we keep this fixed? -> Are we checking for one or several species! If true -> check vignette
  exclude = in_colname_exclude, # remove columns that are not numeric.
  methods = in_methods, # keep!
  showErrors = FALSE, # some methods dont work with certain formats of datasets! ignore the ones that dont work. up to the user.
  missingness = in_missingness) # threshold for how many NAs...


# (4) Run extract_clean_data
print(paste('Run extract_clean_data'))
cleandata2 <- extract_clean_data(
  refdata = dfinal,
  outliers = outlieriris_mult,
  loess = TRUE)


# (5) Write summary to txt file
# Note: I don't know how to get the string summary. The object is an S4 object, and I cannot access the string.
#print(paste0('Write result to txt file: ', out_summary_path))
#fileConn<-file(out_summary_path)
#writeLines(outlieriris_mult, fileConn)
#close(fileConn)


# (6) Write the result to csv file:
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

