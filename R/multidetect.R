#' @title List of outlier detection methods implemented in this package.
#'
#' @return List of methods
#' @export
#'
#' @examples
#' \dontrun{
#' extractMethods()
#' }
#'
#'
extractMethods <- function(){

  methodcategories <- c('reference','univariate', 'opt', 'modelbased', 'cluster', 'densitybased', 'covariance')

  for (imethods in methodcategories) {

    if(imethods=='univariate'){

      univariate <- c('adjbox', 'iqr', 'hampel', 'jknife', 'seqfences','mixediqr',
                      'distboxplot','semiqr',  'zscore', 'logboxplot', "medianrule")

    }else if(imethods=='modelbased'){

      modelb <- c('onesvm', 'iforest')

    }else if(imethods=='cluster'){

      clb <- c('kmeans')

    }else if(imethods=='densitybased'){

      dbd <- c('lof', 'knn', 'glosh')

    }else if(imethods=='opt'){

      opt <- c('optimal')

    }else if(imethods=='reference'){

      reference <- c('reference')

    }else{
      covmd <- c('mahal')
    }
  }

  return(list(reference =reference, univariate = univariate, clustermethods = clb,
              densistybased = dbd, optimal = opt,
              modelbased = modelb, covariance=covmd))
}



#' @title Outlier detection method broad classification.
#'
#' @param category The different outlier categories including \code{mult}, \code{uni} and \code{ref}
#'
#' @return \code{vector} method broad categories
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- broad_classify(category = "mult")
#'
#' }
#'

broad_classify <- function(category){

  if(category=='uni'){

    methodsout <- c('adjbox', 'iqr', 'hampel', 'jknife', 'seqfences','mixediqr',
                    'distboxplot','semiqr',  'zscore', 'logboxplot', "medianrule", 'optimal')

  }else if(category=='mult'){

    methodsout <- c('onesvm', 'iforest','mahal', 'lof', 'knn', 'glosh','kmeans')
  }else{
    methodout <- "reference"
  }
  return(methodsout)
}

#Detect multiple outliers or clean data


#' @title Catch errors during methods implementation.
#'
#' @param func Outlier detection function
#' @param fname function name for messaging or warning identification.
#' @param spname species name being handled
#' @param verbose whether to return messages or not. Default \code{FALSE}.
#' @param warn whether to return warning or not. Default TRUE.
#' @param showErrors show execution errors and therefore for multiple species the code will break if one of the
#'      methods fails to execute.
#'
#' @return Handle errors
#'
tcatch <- function(func, fname=NULL, spname=NULL, verbose=FALSE, warn=FALSE, showErrors = TRUE){

  if(showErrors==FALSE){

    tout <- tryCatch(expr = func, error = function(e) e)

    if(inherits(tout, "error")){

      if(isTRUE(warn)) warning('The output for ', fname, ' returned an error, Please check data or parameters for species ', spname, '.')

      return(NA)

    } else {

      if(isTRUE(verbose))message('The function ', fname, ' was implemented successfully for species ', spname, '.')

      return(tout)
    }
  }else{
    func
  }
}

#' @noRd
detect <- function(x,
                   var,
                   output,
                   exclude,
                   optpar,
                   kmpar,
                   ifpar,
                   lofpar,
                   jkpar,
                   gloshpar,
                   mahalpar,
                   knnpar,
                   zpar,
                   methods,
                   verbose,
                   spname,
                   warn,
                   missingness,
                   showErrors,
                   sdm,
                   na.inform){

  if(missing(x)) stop('Species data missing')

  if(length((colnames(x)[colnames(x)==var]))<1) stop('Variable ', var, ' is  not found in the species data provided for species ', spname, ' .')

  #check if the variable provided to check in outliers is numeric

  varcheck <- unlist(x[,var])

  if(!is(varcheck, 'numeric')) stop('Only numeric column is allowed for parameter var, variable.')

  if(isTRUE(sdm)){

  #check if the variable parameter provided in var does not have NAs

  if(any(is.na(unlist(varcheck)))==TRUE){

    if(isTRUE(verbose)) message("NAs found in the ", var, " parameter and removed successfully during computation.")

    vecNAs <- which(is.na(unlist(varcheck)==TRUE))

    #calculate the missingness in the var variable

    lenvar_NA <- length(vecNAs)/length(varcheck)

    if(lenvar_NA>missingness) stop(var, " will be removed from data due to NAs. Either increase missingness parameter > ", round(lenvar_NA, 3), " or use different var.")

    x <- x[-vecNAs,]

  }else{
    x
  }

  #check if a particular column has unnecessarily high numbers of NAs

  mValues <- apply(x, 2, function(col)sum(is.na(col))/length(col))


  #remove a column with high NAs instead of the rows if % missing values are greater than the user set %missingness. Default is 10%
  if(all(mValues<missingness)) xdata <- x else xdata <- x[, -which(mValues>missingness)]

  #exclude columns that are not needed in the computation like the coordinates mostly for multivariate methods

  if(!is.null(exclude)) {
    #check if the columns to be excluded are in the data.
    check.exclude(x=x, exclude = exclude)

    if(!is.null(optpar$mode)) x2data <- na.omit(xdata) else x2data <- na.omit(xdata[,!colnames(xdata) %in% exclude])

  } else {
    x2data <- na.omit(xdata)
  }

  #identify and remove non-numeric columns if sdm is TRUE



    df <- x2data[, which(sapply(x2data, class) =='numeric')]

    xd <- setdiff(colnames(x2data), y=colnames(df))

    if(length(xd)>=1) if(isTRUE(verbose)) message('Non numeric columns ', paste(xd, collapse =','), ' were removed from data.')

    if(is.null(ncol(df)) | !is(df, 'data.frame')){

      stop("Only one column left after discarding non-numeric columns and cannot compute SDMs. Check the str of your data.")

    } else if(ncol(df)==2) {

      warning("Only ", ncol(df), " are remaining and may fial in detecting for SDMs.")
    }
  }else{
    #remove NAs in the var
    vecNAs <- which(is.na(unlist(varcheck)==TRUE))

    totNA <- length(vecNAs)

    propNA <- round((totNA/nrow(x))*100, 2)

    if(isTRUE(na.inform)) message(totNA, ' (', propNA, '%) NAs removed for parameter ', var, '.')

    if(length(vecNAs)>=1) xdata <- x[-vecNAs,] else xdata <- x


    multivarmethods <- broad_classify(category = "mult")

    removemet <- methods[which(methods%in%multivarmethods==TRUE)]

    if(length(removemet)>=1) stop("Please remove ", paste(removemet, collapse = ','), " from the methods to continue.", call. = FALSE)

    df <- xdata
  }

  #run through each method
  methodList <- list()

  for (cii in methods) {

    if(cii=='reference'){

      methodList[[cii]] = df

    }else if (cii=='optimal'){

      methodList[[cii]] <-  tcatch(func = ecological_ranges(df, var = var, output= output, species = spname,
                                                            optimumSettings = list(optdf = optpar$optdf, optspcol = optpar$optspcol,
                                                                                   mincol = optpar$mincol, maxcol = optpar$maxcol,
                                                                                   ecoparam = optpar$ecoparam, direction= optpar$direction),
                                                            minval=optpar$minval, maxval=optpar$maxval, lat = optpar$lat, lon = optpar$lon,
                                                            ecoparam=optpar$ecoparam, direction = optpar$direction,
                                                            pct= optpar$par,
                                                            checkfishbase = optpar$checkfishbase, mode=optpar$mode, warn=optpar$warn),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if (cii=='adjbox'){

      methodList[[cii]]  <-  suppressMessages(tcatch(func =  adjustboxplots(data = df, var = var, output = output),
                                                     fname = cii, verbose = verbose, spname = spname,
                                                     warn=warn, showErrors = showErrors))

    }else if(cii=='zscore'){

      methodList[[cii]] <-  tcatch(func = zscore(data = df, var = var, output = output, mode = zpar$mode, type = zpar$type),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='iqr'){

      methodList[[cii]] <-  tcatch(func =  interquartile(data = df, var = var, output = output),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='semiqr'){

      methodList[[cii]] <-  tcatch(func =  semiIQR(data = df, var = var, output = output),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='hampel'){

      methodList[[cii]] <-  tcatch(func = hampel(data = df, var = var, output = output),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='jknife'){

      methodList[[cii]] <-  tcatch(func = jknife(data = df, var = var, output = output, mode = jkpar$mode),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='mahal'){

      methodList[[cii]] = tcatch(func = mahal(data = df, exclude = exclude, output = output, mode=mahalpar$mode),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, showErrors = showErrors)

    }else if(cii=='kmeans'){

      methodList[[cii]] <-  tcatch(func = xkmeans(data = df, k= kmpar$k, exclude = exclude, output = output, mode = kmpar$mode,
                                                  method = kmpar$method, verbose=verbose),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)
    }else if(cii=='iforest'){

      methodList[[cii]] <-  tcatch(func = isoforest(data = df, size = ifpar$size, output=output,
                                                    cutoff = ifpar$cutoff, exclude = exclude),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='onesvm'){

      methodList[[cii]] <-  tcatch(func = onesvm(data = df,  exclude = exclude, output = output),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='lof'){

      methodList[[cii]] <-  tcatch(func = xlof(data = df, output =output, minPts = lofpar$minPts,
                                               exclude = exclude, metric = lofpar$metric, mode=lofpar$mode),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='logboxplot'){

      methodList[[cii]] <-  tcatch(func = logboxplot(data = df,  var = var, output = output, x= 1.5),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='medianrule'){

      methodList[[cii]] <-  tcatch(func = logboxplot(data = df,  var = var, output = output, x= 2.3),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='distboxplot'){

      methodList[[cii]] <-  tcatch(func = distboxplot(data = df,  var = var, output = output),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='seqfences'){

      methodList[[cii]] <-  tcatch(func = seqfences(data = df,  var = var, output = output),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='mixediqr'){

      methodList[[cii]] <-  tcatch(func = mixediqr(data = df,  var = var, output = output),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='glosh'){

      methodList[[cii]] <-  tcatch(func = xglosh(data = df, k = gloshpar$k,  output = output, metric = gloshpar$metric, mode=gloshpar$mode, exclude = exclude),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else if(cii=='knn'){

      methodList[[cii]] <-  tcatch(func = xknn(data = df, output = output, metric = knnpar$metric, mode=knnpar$mode, exclude = exclude),
                                   fname = cii, verbose = verbose, spname = spname,
                                   warn=warn, showErrors = showErrors)

    }else{
      message('No outlier detection method selected.')
    }
  }
  return(methodList)
}

#' @title Ensemble multiple outlier detection methods.
#'
#' @description
#' The function allows to ensemble multiple outlier detection methods to ably compare the outliers flagged
#' by each method.
#'
#' @param data \code{dataframe or list}. Data sets for multiple or single species after of extraction of environment predictors.
#' @param var \code{character}. A variable to check for outliers especially the one with directly affects species distribution such as
#' maximum temperature of the coldest month for bioclimatic variables \code{(IUCN Standards and Petitions Committee, 2022))} or
#' stream power index for hydromorphological parameters \code{(Logez et al., 2012)}. This parameter is
#' necessary for the univariate outlier detection methods such as Z-score.
#' @param output \code{character}. Either \code{clean}: for a data set with no outliers, or \code{outlier}: to output a dataframe with outliers. Default \code{outlier}.
#' @param exclude \code{vector}. Exclude variables that should not be considered in the fitting the one class model, for example \code{x} and \code{y} columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param ifpar \code{list}. Isolation forest parameter settings. Parameters of the isolation model that are required include
#'     the \strong{cutoff} to be used for denoting outliers. It ranges from \code{0 to 1} but Default \code{0.5}. Also,
#'     the \strong{size} of data partitioning for training should be determined. For more details check \code{(Liu et al. 2008)}
#' @param methods \code{vector}. Outlier detection methods considered. Use \code{\link{extractMethods}} to get outlier detection methods implemented in this package.
#' @param multiple \code{logical}. If the multiple species are considered, then multiple must be set to \code{TRUE} and \code{FALSE} for single species.
#' @param colsp \code{string}. A column with species names if \code{dataset} for species is a dataframe not a list. See \code{\link{pred_extract}} for extracting environmental data.
#' @param optpar \code{list}. Parameters for species optimal ranges like temperatures ranges. For details check \code{\link{ecological_ranges}}.
#' @param kmpar \code{list}. Parameters for k-means clustering like method and number of clusters for tuning. For details, check \code{\link{xkmeans}}.
#' @param lofpar \code{list}. Parameters for local outlier factor such as the distance matrix and mode of method implementation
#'  such as robust and soft mode. For details \code{\link{xlof}}.
#' @param jkpar \code{list}. Parameters for reverse jack knifing mainly the mode used. For details \code{\link{jknife}}.
#' @param gloshpar \code{list}. Parameters for global local outlier score from hierarchies such as distance metric used. For details \code{\link{xglosh}}.
#' @param mahalpar \code{list}. Parameters for Malahanobis distance which includes varying the mode of output  \code{\link{mahal}}.
#' @param knnpar \code{list}. Parameters for varying the distance matrix such as \code{Euclidean} or \code{Manhattan distance}. For details \code{\link{xknn}}
#' @param zpar \code{list}. Parameters for z-score such as \code{mode} and \code{x} parameter. For details \code{\link{zscore}}
#' @param spname \code{string}. species name being handled.
#' @param missingness \code{numeric}. Allowed missing values in a column to allow a user decide whether to remove the individual
#' columns or rows from the data sets. Default 0.1. Therefore,
#'      if a column has more than 10\% missing values, then it will be removed from the dataset rather than the rows.
#' @param verbose \code{logical}. whether to return messages or not. Default \code{FALSE}.
#' @param warn \code{logical}. Whether to return warning or not. Default \code{TRUE}.
#' @param showErrors \code{logical}. Show execution errors and therefore for multiple species the code will break if one of the
#'      methods fails to execute.
#' @param sdm {logical} If the user sets \code{TRUE}, strict data checks will be done including removing all non-numeric
#'      columns from the datasets before identification of outliers. If set to \code{FALSE} non numeric columns will be left
#'      in the data but the variable of concern will checked if its numeric. Also, only univariate methods are allowed. Check
#'      \code{\link{broad_classify}} for the broad categories of the methods allowed.
#' @param na.inform \code{logical} Inform on the NAs removed in executing general datasets. Default \code{FALSE}.
#'
#' @details
#' This function computes different outlier detection methods including univariate, multivariate and species
#'      ecological ranges to enables seamless comparison and similarities in the outliers detected by each
#'      method. This can be done for multiple species or a single species in a dataframe or lists or dataframes
#'      and thereafter the outliers can be extracted using the \code{\link{clean_data_extract}} function.
#'
#' @return A \code{list} of outliers or clean dataset of \code{datacleaner} class. The different attributes are
#' associated with the \code{datacleaner} class from \code{multidetect} function.
#'
#' \itemize{
#'         \item{\code{result: }}{\code{dataframe}. list of dataframes with the outliers flagged by each method.
#'         }
#'        \item{\code{mode: }}{\code{logical}. Indicating whether it was multiple TRUE or FALSE.}
#'         \item{\code{varused: }}{\code{character}. Indicating the variable used for the univariate outlier detection methods. }
#'         \item{\code{out: }}{\code{character}. Whether outliers where indicated by the user or no oultier data. }
#'         \item{\code{methodsused: }}{\code{vector}. The different methods used the outlier detection process.}
#'         \item{\code{dfname: }}{\code{character}. The dataset name for the species records.}
#'         \item{\code{exclude: }}{\code{vector}. The columns which were excluded during outlier detection if any.}
#'         }
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#' data("jdsdata")
#'
#' matchdata <- match_datasets(datasets = list(jds = jdsdata, efi=efidata),
#'                             lats = 'lat',
#'                             lons = 'lon',
#'                             species = c('speciesname','scientificName'),
#'                             date = c('Date', 'sampling_date'),
#'                             country = c('JDS4_site_ID'))
#'
#'
#'datacheck <- check_names(matchdata, colsp= 'species', pct = 90, merge =TRUE)
#'
#'
#'db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)
#'
#'
#'worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#'rdata <- pred_extract(data = datacheck,
#'                      raster= worldclim ,
#'                      lat = 'decimalLatitude',
#'                     lon= 'decimalLongitude',
#'                     colsp = 'speciescheck',
#'                     bbox = db,
#'                      multiple = TRUE,
#'                     minpts = 10,
#'                     list=TRUE,
#'                     merge=F)
#'
#'
#'out_df <- multidetect(data = rdata, multiple = TRUE,
#'                      var = 'bio6',
#'                      output = 'outlier',
#'                      exclude = c('x','y'),
#'                      methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel', 'kmeans',
#'                                 'logboxplot', 'lof','iforest', 'mahal', 'seqfences'))
#'
#'
#' #optimal ranges in the multidetect: made up
#'
#' optdata <- data.frame(species= c("Salmo trutta", "Abramis brama"),
#'                       mintemp = c(6, 1.6),maxtemp = c(20, 21),
#'                        meantemp = c(8.5, 10.4), #ecoparam
#'                       direction = c('greater', 'greater'))
#' #species record
#'
#' salmoabramis <- rdata["Salmo trutta"]
#'
#' #even if one species, please indicate multiple to TRUE, since its picked from pred_extract function
#'
#' out_df <- multidetect(data = salmoabramis, multiple = TRUE,
#'                       var = 'bio1',
#'                       output = 'outlier',
#'                       exclude = c('x','y'),
#'                       methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel', 'kmeans',
#'                                   'logboxplot', 'lof','iforest', 'mahal', 'seqfences', 'optimal'),
#'                       optpar = list(optdf=optdata, optspcol = 'species',
#'                                     mincol = "mintemp", maxcol = "maxtemp"))
#' #plot the number of outliers
#'
#' #ggoutliers(out_df, 1)
#'
#' }
#'
#' @references
#' \enumerate{
#'   \item IUCN Standards and Petitions Committee. (2022). THE IUCN RED LIST OF THREATENED SPECIESTM Guidelines for Using the IUCN Red List
#' Categories and Criteria Prepared by the Standards and Petitions Committee of the IUCN Species Survival Commission.
#' https://www.iucnredlist.org/documents/RedListGuidelines.pdf.
#' \item Liu, F. T., Ting, K. M., & Zhou, Z. H. (2008, December). Isolation forest.
#' In 2008 eighth ieee international conference on data mining (pp. 413-422). IEEE.
#' }
#'

multidetect <- function(data,
                        var,
                        output = "outlier",
                        exclude = NULL,
                        multiple,
                        colsp = NULL,
                        optpar = list(optdf = NULL, ecoparam = NULL, optspcol = NULL, direction =NULL,
                                      maxcol = NULL, mincol = NULL, maxval = NULL, minval = NULL,
                                      checkfishbase =FALSE, mode='geo', lat = NULL, lon = NULL, pct = 80,
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
                        verbose=FALSE, spname=NULL,warn=FALSE,
                        missingness = 0.1, showErrors = TRUE, sdm = TRUE, na.inform = FALSE){

  #check if var is the excluded strings

  if(any(var%in%exclude==TRUE)==TRUE) stop("Remove ", var, " among the strings to be excluded.")

  match.argc(output, c("clean", "outlier"))

  if(length(var)>1 && isFALSE(multiple)) stop("For mulitple variables of concern, set multiple to TRUE.")

  #check if all methods indicated exist in the package

  unxmethods <- unique(methods)

  if(length(unxmethods)<length(methods)) dup_methods = unxmethods else dup_methods = methods

  allowedmethods <- c('reference','adjbox', 'zscore','kmeans', 'iforest', 'distboxplot','optimal',
                      'mixediqr', 'seqfences', 'mahal', 'medianrule', 'iqr','hampel',
                      'logboxplot', 'onesvm', 'jknife', 'semiqr', 'lof','glosh', 'knn')

  tfcheck <- dup_methods%in%allowedmethods

  if(any(tfcheck==FALSE)){

    notsupported <- dup_methods[which(tfcheck==FALSE)]

    stop('The methods -', paste(notsupported, collapse = ', '), '- are/is not accepted. Check extractMethods() for allowed methods.')
  }
  if(multiple ==FALSE & !is.null(colsp)) stop("For single species do not provide the colsp parameter.")

  #run for single dataframe

  if(isFALSE(multiple) && is.null(colsp) ){

    if(!is(data, 'data.frame')) stop('For a single species only a dataframe is accepted.')

    if(nrow(data)<ncol(data)) warning('Number of rows are less than variables and some methods may not function properly.')

    outdata <-  detect(x = data, var = var, output = output,
                       exclude = exclude,optpar = optpar,
                       kmpar = kmpar, ifpar = ifpar, jkpar = jkpar,
                       mahalpar = mahalpar, lofpar = lofpar,
                       zpar = zpar, gloshpar = gloshpar,
                       knnpar = knnpar,
                       methods = dup_methods,
                       verbose = verbose,
                       spname = spname,warn=warn,
                       missingness = missingness, showErrors = showErrors, sdm = sdm, na.inform = na.inform)

  }else {

    if(is(data, 'list')){

      df<- data

    }else if(is(data, 'data.frame') ){

      if(length(var)>1){

        df <- sapply(var, function(x) x <-  data, simplify = FALSE)

      }else{

        if(is.null(colsp)) stop('For multiple species dataframe, provide the species column name in the colsp parameter.')

        if(!any(colnames(data)%in%colsp)==TRUE) stop('The column name provided in colsp parameter is not in the species data.')

        if(!is.null(exclude)) if((colsp%in%exclude)==TRUE) warning("Remove the column for species names in the exclude parameter.")

        df <- split(data, f= data[,colsp])
      }

    }else{
      stop('Data format not recognised, Only lists of datasets or dataframe are accepted.')
    }
    outdata <- list()

    for (mdi in names(df)) {

      dfinal<- df[[mdi]]

      if(isTRUE(warn)) if(nrow(dfinal)<ncol(dfinal)) warning('Number of rows for ',mdi,' are less than variables and some methods may not function properly.')

      if(length(var)>1) var1 = mdi else var1 = var
      print(var1)

      d <-  detect(x = dfinal, var = var1, output = output,
                   exclude = exclude,optpar = optpar,
                   kmpar = kmpar, ifpar = ifpar, jkpar = jkpar,
                   mahalpar = mahalpar, lofpar = lofpar,
                   zpar = zpar, gloshpar = gloshpar, knnpar = knnpar,
                   methods = dup_methods, verbose = verbose, spname = mdi,warn=warn,
                   missingness = missingness, showErrors = showErrors, sdm = sdm, na.inform = na.inform)
      outdata[[mdi]] <- d
    }
  }
  if(is.null(exclude)){
    return(new('datacleaner', result = outdata, mode = multiple, varused = var,
               out = output, methodsused = dup_methods, dfname = deparse(substitute(data)),
               excluded = NA))
  }else{
    return(new('datacleaner', result = outdata, mode = multiple, varused = var,
               out = output, methodsused = dup_methods, dfname = deparse(substitute(data)),
               excluded = exclude))
  }
}


