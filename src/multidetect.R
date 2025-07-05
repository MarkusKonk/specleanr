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

args <- commandArgs(trailingOnly = TRUE)
print(paste0('R Command line args: ', args))
in_data_path_or_url                  = args[1] # req
in_colname_var                       = args[2] # req     e.g. "Sepal.Length
in_select_var                        = args[3] # opt     if only particular columns are needed form the datasest
in_bool_multiple_species             = args[4] # opt     e.g. "TRUE"
in_output_type                       = args[5] # req_set e.g., clean or outlier#defualt outlier
in_group_columnname                  = args[6] # opt     e.g. if multiple = TRUE, then its neeed. in iris data like "Species
in_colnames_exclude                  = args[7] # opt     e.g.exclude irrelevant columns mostly important for multidimensional data e.g. ID, SN, x, y etc
in_methods                           = args[8] # req     e.g. "mixediqr, logboxplot, iqr, distboxplot, jknife, semiqr, hampel, iforest, lof, mahal"
in_silence_true_errors               = args[9] # req_set e.g. "TRUE"
in_ecoranges_settings_df             = args[10]  #opt dataframe with ranges: optional unless ecoranges used
in_ecoranges_settings_ecoparam       = args[11]  #opt ecorange param used e.g temp opt unless ecoranges used
in_ecoranges_settings_optspcol       = args[12]  #opt unless ecoranges used
in_ecoranges_settings_direction      = args[13]  #opt unless ecoranges used
in_ecoranges_settings_maxcol         = args[14]  #opt unless ecoranges used
in_ecoranges_settings_mincol         = args[15]  #opt unless ecoranges used
in_ecoranges_settings_minval         = args[16]  #opt unless ecoranges used
in_ecoranges_settings_maxval         = args[50]  #opt unless ecoranges used
in_ecoranges_settings_checkFB_bool   = args[17]  #opt unless ecoranges used
in_ecoranges_settings_mode           = args[18]  #opt unless ecoranges used
in_ecoranges_settings_lat            = args[19]  #opt unless ecoranges used
in_ecoranges_settings_lon            = args[17]  #opt unless ecoranges used
in_ecoranges_settings_pct            = args[18]  #opt unless ecoranges used
in_ecoranges_settings_warn           = args[19]  #opt unless ecoranges used
in_kmeans_settings_k          = args[20] #number of clusters
in_kmeans_settings_method     = args[21] #e.g 'silhouette' #optimising
in_kmeans_settings_mode       = args[22]
in_isoforest_settings_cutoff  = args[22] #e.g 0.5
in_isoforest_settings_size    = args[23] #e.g 0.7
in_mahal_settings_mode        = args[24] #e.g soft
in_reversejk_settings         = args[25] #e.g soft
in_zscore_settings_type       = args[26] #e.g mild
in_zscore_settings_mode       = args[27] #e.g mild
in_glosh_settings_k           = as.numeric(args[28]) #e.g k= 3
in_glosh_settings_metric      = args[29] #e.g manhattan
in_glosh_settings_mode        = args[30] #e.g soft
in_knn_settings_metric        = args[31] #e.g manhattan
in_knn_settings_mode          = args[32] #e.g soft
in_lof_settings_metric        = args[33] #e.g manhattan
in_lof_settings_mode          = args[34] #e.g soft
in_lof_settings_minpts        = as.numeric(args[35]) #e.g soft
in_boot_settings_run_bool     = args[36] #bool
in_boot_settings_maxrecords   = as.numeric(args[37]) #numb or max to initate bootstraping
in_boot_settings_nb           = as.numeric(args[38]) #numeric e.g 30 defualt
in_boot_settings_seed         = as.numeric(args[39]) #rnadomisation e.g 1123..
in_boot_settings_threshold    = as.numeric(args[40]) #decimal e.g. 0.6
in_pca_settings_exec_bool     = args[41] #bool
in_pca_settings_npc           = as.numeric(args[42]) #integer..number of pcs retained
in_pca_settings_quiet         = args[43] #bool to print the cummulative variance
in_pca_settings_pcvar         = args[44] #pc used for outlier detectiion defualt is PC1
in_verbose_bool               = args[45] #implementation messages
in_warn_bool                  = args[46] #return warning msgs
in_spname_auto                = args[47] #auto detected
in_sdm_bool                   = args[48] # if multidimensional data then its TRUE. univariate data sdm is F
in_na.inform_bool             = args[49] # msgs on handling NAs defualt FALSE
in_missingness                = as.numeric(args[51]) #req_adj# e.g. 0.1

#parameter for extracting clean data

in_bool_loess                 =   args[52] #e.g.g TRUE or FALSE
in_threshold_clean            =   args[53] #defualt is 0.8
in_mode_clean                 =   args[54]#e.g. 'abs', "best"
in_outliertoNA_bool           =   args[55]     
in_classifymode               =   args[56] #default med
in_eif_bool                   =   args[57] #e.g. F/T  
in_autothreshold_bool         =   args[58] 
in_percent_abs                =   as.numeric(args[59])    #ranges 0.1 to 1 


#classify data 



###=========
#Extrain clean data
#===========


# in_threshold = args[8] # can be "0.7", then loess is set to FALSE. Can be "NULL", then loess is set to TRUE
# in_colname_species = args[9]
# out_result_path = args[10]
# #out_summary_path = args[10]


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

#=========================================
# (3) Make string booleans boolean
#=========================================
in_bool_multiple_species   = "true"
in_silence_true_errors     = "true"
in_warn_bool               = "true"
in_na.inform               = "true"
in_verbose_bool            = "true"
in_sdm_bool                = "true"
in_pca_settings_exec_bool  = "true"
in_boot_settings_run_bool  = "true"
in_ecoranges_settings_checkFB_bool = "true"
in_pca_settings_quiet      = "true"
in_bool_loess              =  "true"
in_outliertoNA_bool        =  "true"   
in_eif_bool                =  "true" 
in_autothreshold_bool      =  "true"



in_bool_multiple_species   = tolower(in_bool_multiple_species) == 'true'
in_silence_true_errors     = tolower(in_silence_true_errors) == 'true'
in_warn_bool               = tolower(in_warn_bool) == 'true'
in_na.inform               = tolower(in_na.inform) == 'true'
in_sdm_bool                = tolower(in_sdm_bool) == 'true'
in_pca_settings_exec_bool  = tolower(in_pca_settings_exec_bool) == 'true'
in_boot_settings_run_bool  = tolower(in_boot_settings_run_bool) == 'true'
in_ecoranges_settings_checkFB_bool = tolower(in_ecoranges_settings_checkFB_bool) == 'true'
in_verbose_bool            = tolower(in_verbose_bool) == 'true'
in_pca_settings_quiet      = tolower(in_pca_settings_quiet) == 'true'
in_bool_loess              = tolower(in_bool_loess ) == 'true'
in_outliertoNA_bool        = tolower(in_outliertoNA_bool) == 'true'
in_eif_bool                = tolower(in_eif_bool) == 'true'
in_autothreshold_bool      = tolower(in_autothreshold_bool) == 'true'


in_optpar = list(optdf = in_ecoranges_settings_df, ecoparam = in_ecoranges_settings_ecoparam,
              optspcol = in_ecoranges_settings_optspcol, direction =in_ecoranges_settings_direction,
              maxcol = in_ecoranges_settings_maxcol, mincol = in_ecoranges_settings_mincol,
              maxval = in_ecoranges_settings_maxval, minval = in_ecoranges_settings_minval,
              checkfishbase =in_ecoranges_settings_checkFB_bool,
              mode = in_ecoranges_settings_mode, lat = in_ecoranges_settings_lat,
              lon = in_ecoranges_settings_lon, pct = in_ecoranges_settings_pct,
              warn = in_ecoranges_settings_warn)

in_kmpar =list(k= in_kmeans_settings_k, method= in_kmeans_settings_method, mode= in_kmeans_settings_mode)

in_ifpar = list(cutoff = in_isoforest_settings_cutoff, size=in_isoforest_settings_size)

in_mahalpar = list(mode= in_mahal_settings_mode)

in_jkpar = list(mode=in_reversejk_settings)

in_zpar = list(type=in_zscore_settings_type, mode= in_zscore_settings_mode)

in_gloshpar = list(k= in_glosh_settings_k, metric= in_glosh_settings_metric, mode= in_glosh_settings_mode)

knnpar = list(metric= in_knn_settings_metric, mode= in_knn_settings_mode)

in_lofpar = list(metric= in_lof_settings_metric, mode= in_lof_settings_mode, minPts= in_lof_settings_minpts)

in_bootSettings = list(run= in_boot_settings_run_bool, nb= in_boot_settings_nb,
                    maxrecords = in_boot_settings_maxrecords, seed= in_boot_settings_seed,
                    th = in_boot_settings_threshold)

in_pc = list(exec = in_pca_settings_exec_bool, npc= in_pca_settings_npc,
          q = in_pca_settings_quiet, pcvar = in_pca_settings_pcvar)


#===============
#predefined variables
#===========
data,
var,
select = NULL,
output = "outlier",
exclude = NULL,
multiple,
var_col = NULL,
optpar = list(optdf = NULL, ecoparam = NULL, optspcol = NULL, direction =NULL,
              maxcol = NULL, mincol = NULL, maxval = NULL, minval = NULL,
              checkfishbase =FALSE, mode = NULL, lat = NULL, lon = NULL, pct = 80,
              warn = FALSE),
kmpar =list(k=6, method='silhouette', mode='soft'),
ifpar = list(cutoff = 0.5, size=0.7),
mahalpar = list(mode='soft'),
jkpar = list(mode='soft'),
zpar = list(type='mild', mode='soft'),
gloshpar = list(k= 3, metric='manhattan', mode='soft'),
knnpar = list(metric='manhattan', mode='soft'),
lofpar = list(metric='manhattan', mode='soft', minPts= 10),
methods,
bootSettings = list(run=FALSE, nb=5, maxrecords = 30, seed=1135, th = 0.6),
pc = list(exec = FALSE, npc=2, q = T, pcvar = 'PC1'),
verbose=FALSE, spname=NULL,warn=FALSE,
missingness = 0.1,
silence_true_errors = TRUE,
sdm = TRUE,
na.inform = FALSE





# Only provide column name for group if multiple = FALSE

if (!in_bool_multiple_species) {
  print('Setting group column name to NULL (only needed in case of multiple groups).')
  in_group_columnname <- NULL
}


# # (4) Run multidetect
# print(paste('Running specleanr::multidetect...'))
# print(paste('Var:', in_colname_var))
# print(paste('Multiple:', in_bool_multiple_species))
# print(paste('Colname Species:', in_colname_species))
# print(paste('Exclude columns:', paste0(in_colnames_exclude, collapse=' + ')))
# print(paste('Missingness:', in_missingness))
# print(paste('SilenceErrors:', in_silence_true_errors))
# print(paste('Methods:', paste0(in_methods, collapse=' + ')))

outlieriris_mult <- multidetect(
  data            = dfinal,
  var             = in_colname_var,
  select          = in_select_var,
  output          = in_output_type,
  exclude         = in_colnames_exclude,
  multiple        = in_bool_multiple_species,
  var_col         = in_group_columnname,
  optpar          = in_optpar,
  kmpar           = in_kmpar,
  ifpar           = in_ifpar,
  mahalpar        = in_mahalpar,
  jkpar           = in_jkpar,
  zpar            = in_zpar,
  gloshpar        = in_gloshpar,
  knnpar          = in_knnpar,
  lofpar          = in_lofpar,
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
  refdata             = dfinal,
  outliers            = outlieriris_mult,
  mode                = in_mode_clean,
  var_col             = in_group_columnname,
  threshold           = in_threshold_clean,
  warn                = in_warn_bool,
  verbose             = in_verbose_bool,
  autothreshold       = in_autothreshold_bool,
  pabs                = in_percent_abs,
  loess               = in_bool_loess,
  outlier_to_NA       = in_outliertoNA_bool
)
print(paste('Running specleanr::extract_clean_data... DONE.'))


classifyout <- classify_data(
  refdata     = dfinal,
  outliers    = outlieriris_mult,
  var_col     = in_group_columnname,
  threshold   = in_threshold_clean,
  warn        = in_warn_bool,
  verbose     = in_verbose_bool,
  classify    = in_classifymode,
  EIF         = in_eif_bool
)



# (6) Write summary to txt file
# Note: I don't know how to get the string summary. The object is an S4 object, and I cannot access the string.
#print(paste0('Write result to txt file: ', out_summary_path))
#fileConn<-file(out_summary_path)
#writeLines(outlieriris_mult, fileConn)
#close(fileConn)


# (7) Write the result to csv file:
print(paste0('Write result to csv file: ', out_result_path))
data.table::fwrite(cleandata2 , file = out_result_path)

#=========================================
#PROVIDE MODE TO ALLOW DATA CLASSIFICATION
#==========================================


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


