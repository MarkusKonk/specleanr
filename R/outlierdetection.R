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

  methodcategories <- c('univariate', 'opt', 'modelbased', 'cluster', 'densitybased', 'covariance')

  for (imethods in methodcategories) {

    if(imethods=='univariate'){

      univariate <- c('adjustboxplots', 'interquartile', 'hampel', 'rjknife', 'seqfences','mixediqr',
                      'distboxplot','semiinterquartile',  'zscore', 'logboxplot')

    }else if(imethods=='modelbased'){

      modelb <- c('onesvm', 'isoforest')

    }else if(imethods=='cluster'){

        clb <- c('xkmeans','xkmedoids', 'xkmedian')

    }else if(imethods=='densitybased'){

        dbd <- c('xlof', 'xknn', 'xglosh')

    }else if(imethods=='opt'){

        opt <- c('optimal')

    }else{
        covmd <- c('mahal')
      }
  }

  return(list(univariate = univariate, clustermethods = clb, densistybased = dbd, optimal = opt,
              modelbased = modelb, covariance=covmd))
}

#Detect multiple outliers or clean data


#' @title Catch errors during methods implementation.
#'
#' @param func Outlier detection function
#' @param fname function name for messaging or warning identification.
#' @param spname species name being handled
#' @param verbose whether to return messages or not. Default \code{FALSE}.
#' @param warn whether to return warning or not. Default TRUE.
#' @param debug show execution errors and therefore for multiple species the code will break if one of the
#'      methods fails to execute.
#'
#' @return Handle errors
#'
tcatch <- function(func, fname=NULL, spname=NULL, verbose=FALSE, warn=FALSE, debug = TRUE){

  if(debug==FALSE){

    tout <- tryCatch(expr = func, error = function(e) e)

    if(inherits(tout, "error")){

      if(isTRUE(warn))warning('The output for ', fname, ' returned an error, Please check data or parameters for species ', spname, '.')

      return(NA)

    } else {

      if(isTRUE(verbose))message('The fucntion ', fname, ' implemented successfully for species ', spname, '.')

      return(tout)
    }
  }else{
    func
  }
}


#' @title Flags outliers or no outliers for multiple outlier methods.
#'
#' @param x Dataframe with species occurrences
#' @param var Environmental parameter considered in flagging suspicious outliers.
#' @param output Either \strong{clean}: for a data set with no outliers, or \strong{outlier}:
#'        to output a dataframe with outliers.
#' @param exclude Exclude variables that should not be considered in the fitting the one
#'      class model, for example x and y columns or
#'      latitude/longitude or any column that the user doesn't want to consider.
#' @param ifpar Isolation forest parameter settings. Parmeters of the isolation
#'      model that are required include
#'     the \strong{cutoff} to be used for denoting outliers. It ranges from \strong{0 to 1}
#'      but Default \strong{0.5}. Also,
#'     the \strong{size} of data partitioning for training should be determined.
#'     For more details check \strong{(Liu et al. 2008)}
#' @param methods Outlier detection methods considered. Use \strong{\code{extractMethod}}
#'      to get outliers implemented in the package.
#' @param colsp Column with species names.
#' @param optpar Parameters for species optimal ranges like temperatures ranges \link[specleanr]{multidetect}.
#' @param kmpar Parameters for kmeans clustering like method and number of clusters for tuning \link[specleanr]{multidetect}.
#' @param kmedoidpar Parameters for adjusting the distance metrics used in kmedoid method \link[specleanr]{multidetect}.
#' @param lofpar Parameters for local outlier factor such as the distance matrix and mode of method implementation
#'  such as robust and soft modes \link[specleanr]{multidetect}.
#' @param jkpar Parameters for reverse jack knifing mainly the mode used \link[specleanr]{multidetect}.
#' @param gloshpar Parameters for global local outlier score from hierarchies such as distance metric used \link[specleanr]{multidetect}.
#' @param mahalpar Parameters for Malahanobis distance which includes varying the mode of output \link[specleanr]{multidetect}.
#' @param knnpar Parameters for varying the distance matrix such euclidean or Manhattan \link[specleanr]{multidetect}.
#' @param zpar Parameters for z-score such mode and x parameter \link[specleanr]{multidetect}.
#' @param verbose whether to return messages or not. Default FALSE.
#' @param warn whether to return warning or not. Default TRUE.
#' @param spname species name being handled.
#' @param missingness Allowed missing values in a column to allow a user decide whether to remove the individual columns or rows from the data sets. Default 0.1. Therefore, if
#'      if a column has more than 10\% missing values, then it will be removed from the dataset rather than the rows.
#' @param debug show execution errors and therefore for multiple species the code will break if one of the
#'      methods fails to execute.
#'
#' @return Dataframe with or with no outliers.
#'
#'
#' @references
#'
#' \enumerate{
#'   \item Liu FeiT, Ting KaiM, Zhou Z-H. 2008. Isolation Forest. Pages 413–422
#' In 2008 Eighth IEEE International Conference on Data Mining. Available from
#' https://ieeexplore.ieee.org/abstract/document/4781136 (accessed November 18, 2023).
#'   \item Reference on different methods
#' }
#' @seealso  \code{\link[specleanr]{multidetect}}

detect <- function(x,
                    var,
                    output,
                    colsp,
                    exclude,
                    optpar,
                    kmpar,
                    kmedoidpar,
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
                   debug){

  if(missing(x)) stop('Species data missing')

  if(length((colnames(x)[colnames(x)==var]))<1) stop('Variable ', var, ' is  not found in the species data provided for species ', spname, ' .')

  if(!is(x[,var], 'numeric')) stop('Only numeric column is allowed for parameter var, variable.')

  #check if a particular column has unnecessarily high numbers of NAs

  mValues <- apply(x, 2, function(col)sum(is.na(col))/length(col))

  if(all(mValues<missingness)) xdata <- x else xdata <- x[, -which(mValues>missingness)]

  if(!is.null(exclude)) {

    if(!is.null(optpar$mode)) x2data <- na.omit(xdata) else x2data <- na.omit(xdata[,!colnames(xdata) %in% exclude])

  } else {
    x2data <- na.omit(xdata)
  }

  df <- x2data[, which(sapply(x2data, class) =='numeric')]

  if(isTRUE(verbose)){

    xd <- base::setdiff(colnames(x2data), y=colnames(df))

    if(length(xd)>=1) message('Non numeric columns ', paste(xd, collapse =','), ' were removed from data.')

  }

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
      warn=warn, debug = debug)

    }else if (cii=='adjbox'){

      methodList[[cii]]  <-  suppressMessages(tcatch(func =  adjustboxplots(data = df, var = var, output = output),
                                  fname = cii, verbose = verbose, spname = spname,
                                  warn=warn, debug = debug))

    }else if(cii=='zscore'){

      methodList[[cii]] <-  tcatch(func = zscore(data = df, var = var, output = output, mode = zpar$mode, type = zpar$type),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='iqr'){

      methodList[[cii]] <-  tcatch(func =  interquartile(data = df, var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='semiqr'){

      methodList[[cii]] <-  tcatch(func =  semiIQR(data = df, var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='hampel'){

      methodList[[cii]] <-  tcatch(func = hampel(data = df, var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='jknife'){

      methodList[[cii]] <-  tcatch(func = jknife(data = df, var = var, output = output, mode = jkpar$mode),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='mahal'){

      methodList[[cii]] = tcatch(func = mahal(data = df, exclude = exclude, output = output, mode=mahalpar$mode),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='kmeans'){

      methodList[[cii]] <-  tcatch(func = xkmeans(data = df, k= kmpar$k, exclude = exclude, output = output, mode = kmpar$mode,
                                          method = kmpar$method, verbose=verbose),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)
    }else if(cii=='kmedoid'){

      methodList[[cii]] <-  tcatch(func = xkmedoid(data = df, k = kmedoidpar$k, metric = kmedoidpar$metric, output = output, exclude = exclude, x=1.5),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='iforest'){

      methodList[[cii]] <-  tcatch(func = isoforest(data = df, size = ifpar$size, output=output,
                                                    cutoff = ifpar$cutoff, exclude = exclude),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='onesvm'){

      methodList[[cii]] <-  tcatch(func = onesvm(data = df,  exclude = exclude, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='lof'){

      methodList[[cii]] <-  tcatch(func = xlof(data = df, output =output, minPts = lofpar$minPts,
                                       exclude = exclude, metric = lofpar$metric, mode=lofpar$mode),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='logboxplot'){

      methodList[[cii]] <-  tcatch(func = logboxplot(data = df,  var = var, output = output, x= 1.5),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='medianrule'){

      methodList[[cii]] <-  tcatch(func = logboxplot(data = df,  var = var, output = output, x= 2.3),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='distboxplot'){

      methodList[[cii]] <-  tcatch(func = distboxplot(data = df,  var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='seqfences'){

      methodList[[cii]] <-  tcatch(func = seqfences(data = df,  var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='mixediqr'){

      methodList[[cii]] <-  tcatch(func = mixediqr(data = df,  var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='glosh'){

      methodList[[cii]] <-  tcatch(func = xglosh(data = df, k = gloshpar$k,  output = output, metric = gloshpar$metric, mode=gloshpar$mode, exclude = exclude),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else if(cii=='knn'){

      methodList[[cii]] <-  tcatch(func = xknn(data = df, output = output, metric = knnpar$metric, mode=knnpar$mode, exclude = exclude),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn, debug = debug)

    }else{
      message('No outlier detection method selected.')
    }
  }
  return(methodList)
}

#' @title Allows ensemble multiple outlier detection methods.
#'
#' @param data Dataframe or list of data sets for multiple or single species after after of extraction of environment predictors.
#' @param var A variable to check for outliers especially the one with directly affects species distribution such as
#' maximum temperature of the coldest month for bioclimatic variables \code{(IUCN Standards and Petitions Committee, 2022))} or
#' stream power index for hydromorphological parameters \code{(Logez et al., 2012)}.
#' @param output Either \strong{clean}: for a data set with no outliers, or \strong{outlier}: to output a dataframe with outliers.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param ifpar Isolation forest parameter settings. Parameters of the isolation model that are required include
#'     the \strong{cutoff} to be used for denoting outliers. It ranges from \strong{0 to 1} but Default \strong{0.5}. Also,
#'     the \strong{size} of data partitioning for training should be determined. For more details check \strong{(Liu et al. 2008)}
#' @param methods Outlier detection methods considered. Use \strong{\code{extractMethod}} to get outliers implemented in the package.
#' @param multiple If the multiple species considered, then multiple set to TRUE: If FALSE then multiple should be set to FALSE.
#' @param colsp A column with species columns if the data set for species is a dataframe not a list. see \link[specleanr]{pred_extract} for extracting environmental data.
#' @param optpar Parameters for species optimal ranges like temperatures ranges.
#' @param kmpar Parameters for k-means clustering like method and number of clusters for tuning.
#' @param kmedoidpar Parameters for adjusting the distance metrics used in kmedoid method.
#' @param lofpar Parameters for local outlier factor such as the distance matrix and mode of method implementation
#'  such as robust and soft modes..
#' @param jkpar Parameters for reverse jack knifing mainly the mode used.
#' @param gloshpar Parameters for global local outlier score from hierarchies such as distance metric used..
#' @param mahalpar Parameters for Malahanobis distance which includes varying the mode of output.
#' @param knnpar Parameters for varying the distance matrix such as \code{Euclidean} or \code{Manhattan distance}.
#' @param zpar Parameters for z-score such mode and x parameter.
#' @param spname species name being handled.
#' @param missingness Allowed missing values in a column to allow a user decide whether to remove the individual columns or rows from the data sets. Default 0.1. Therefore, if
#'      if a column has more than 10\% missing values, then it will be removed from the dataset rather than the rows.
#' @param verbose whether to return messages or not. Default FALSE.
#' @param warn whether to return warning or not. Default TRUE.
#' @param debug show execution errors and therefore for multiple species the code will break if one of the
#'      methods fails to execute.
#'
#' @details
#' This function computes different outlier detection methods including univariate, multivariate and species
#'      ecological ranges to enables seammless comaprision and similarities in the outliers detected by each
#'      method. This can be done for multiple species or a single species in a dataframe or lists or dataframes
#'      and thereafter the outliers can be extracted using the \link[specleanr]{extract_clean_data} function.c
#'
#' @return Outliers or clean dataset.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(jdsdata)
#'
#' data(efidata)
#'
#'
#' matchdata <- merge_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                     lats = 'lat',
#'                     lons = 'lon',
#'                     species = c('speciesname','scientificName'),
#'                     country= c('JDS4_site_ID),
#'                     dates=c('sampling_date', 'Date'))
#'
#' datacheck <- check_names(matchdata, colsp= 'species', pct = 90, merge =TRUE)
#'
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)
#'
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' rdata <- pred_extract(data = datacheck,
#'                           raster= worldclim ,
#'                           lat = 'decimalLatitude',
#'                           lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = TRUE,
#'                           minpts = 10,
#'                           list=TRUE,
#'                           merge=F)
#'
#'
#' out_df <- multidetect(data = rdata, multiple = TRUE,
#'                        var = 'bio6',
#'                         output = 'outlier',
#'                         exclude = c('x','y'),
#'                         methods = c('zscore', 'adjbox',iqr', 'semiqr','hampel'),
#'                         kmpar =list(k = 6, method='silhouette', mode='soft'),
#'                         ifpar = list(cutoff = 0.5, size=0.7))
#'
#'
#' }
#'
#' @references
#' \enumerate{
#'   \item IUCN Standards and Petitions Committee. (2022). THE IUCN RED LIST OF THREATENED SPECIESTM Guidelines for Using the IUCN Red List
#' Categories and Criteria Prepared by the Standards and Petitions Committee of the IUCN Species Survival Commission.
#' https://www.iucnredlist.org/documents/RedListGuidelines.pdf.
#' \item Liu FeiT, Ting KaiM, Zhou Z-H. 2008. Isolation Forest. Pages 413–422 In 2008 Eighth IEEE International Conference on Data Mining.
#' Available from https://ieeexplore.ieee.org/abstract/document/4781136 (accessed November 18, 2023).
#' }
#'
#'  \code{\link{extract_clean_data}}

multidetect <- function(data,
                        var,
                        output = 'oultier',
                        exclude,
                        multiple,
                        colsp = NULL,
                        optpar = list(df=NULL, min=NULL, max=NULL, sp=NULL, colsp=NULL, sciname=NULL),
                        kmpar =list(k=6, method='silhouette', mode='soft'),
                        kmedoidpar = list(k=2, metric='manhattan'),
                        ifpar = list(cutoff = 0.5, size=0.7),
                        mahalpar = list(mode='soft'),
                        jkpar = list(mode='soft'),
                        zpar = list(type='mild', mode='soft'),
                        gloshpar = list(k= 3, metric='manhattan', mode='soft'),
                        knnpar = list(metric='manhattan', mode='soft'),
                        lofpar = list(metric='manhattan', mode='soft', minPts= 10),
                        methods,
                        verbose=FALSE, spname=NULL,warn=TRUE,
                        missingness = 0.1, debug = FALSE){

  allowedmethods <- c('reference','adjbox', 'zscore','kmeans', 'iforest', 'distboxplot','optimal',
                      'mixediqr', 'seqfences', 'mahal', 'medianrule', 'iqr','hampel',
                      'logboxplot', 'onesvm', 'jknife', 'semiqr', 'lof','glosh', 'knn', "kmedoid")

  tfcheck <- methods%in%allowedmethods

  if(any(tfcheck==FALSE)){

    notsupported <- methods[which(tfcheck==FALSE)]

    stop('The methods ', paste(notsupported, collapse = ','), ' are not accepted.')
  }
  if(multiple ==FALSE & !is.null(colsp)) stop("For single species do not provide the colsp parameter.")

  #check for number of species

  if(isTRUE(multiple) && !is(data, 'list') && is.null(colsp)){
    stop('For multiple species dataframe, set provide the column species in the colsp parameter.')
  }

  #check if there enough data to run mahalanobis distance matrix

  if(isFALSE(multiple) && is.null(colsp) ){

    if(nrow(data)<ncol(data)){

      if(isTRUE(warn))warning('Number of rows are less than variables and some methods may not function properly.')

      outdata <-  detect(x = data, var = var, output = output,
                          exclude = exclude,optpar = optpar,
                          kmpar = kmpar, ifpar = ifpar, jkpar = jkpar,
                          mahalpar = mahalpar, lofpar = lofpar, kmedoidpar = kmedoidpar,
                          zpar = zpar, gloshpar = gloshpar,
                          knnpar = knnpar,
                          methods = methods,
                         verbose = verbose,
                         spname = spname,warn=warn,
                         missingness = missingness, debug = debug)
    }
  }else {

    if(is(data, 'list')){

      df<- data

    }else if(is(data, 'data.frame') ){

      df <- split(data, f= data[,colsp])

    }else{
      stop('Data format not recognised')
    }
    outdata <- list()
    for (mdi in names(df)) {

      dfinal<- df[[mdi]]

      d <-  detect(x = dfinal, var = var, output = output, colsp=colsp,
                   exclude = exclude,optpar = optpar,
                   kmpar = kmpar, ifpar = ifpar, jkpar = jkpar,
                   mahalpar = mahalpar, lofpar = lofpar, kmedoidpar = kmedoidpar,
                   zpar = zpar, gloshpar = gloshpar, knnpar = knnpar,
                   methods = methods, verbose = verbose, spname = mdi,warn=warn,
                   missingness = missingness, debug = debug)
      outdata[[mdi]] <- d
    }
  }
  if(is.null(exclude)){
    return(new('datacleaner', result = outdata, mode = multiple, varused = var,
               out = output, methodsused = methods, dfname = deparse(substitute(data)),
               excluded = NA))
  }else{
    return(new('datacleaner', result = outdata, mode = multiple, varused = var,
               out = output, methodsused = methods, dfname = deparse(substitute(data)),
               excluded = exclude))
  }
}


