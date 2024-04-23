#' @title List of outlier detection methods implemented in this package.
#'
#' @return List of methods
#' @export
#'
#' @examples
#' extractMethods()
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

        opt <- c('bulkopt', 'optimal')

    }else{
        covmd <- c('mahal')
      }
  }

  return(list(univariate = univariate, clustermethods = clb, densistybased = dbd, optimal = opt,
              modelbased = modelb, covariance=covmd))
}

#Detect multiple outliers or clean data


#' Title
#'
#' @param f Outlier detection function
#' @param fname function name for messaging or warning identification.
#' @param spname species name being handled
#' @param verbose whether to return messages or not. Default FALSE.
#' @param warn whether to return warning or not. Default TRUE.
#'
#' @return Handle errors
#'
tcatch <- function(f, fname=NULL, spname=NULL, verbose=FALSE, warn=TRUE){

  tout <- tryCatch(expr = f, error = function(e) e)

  if(inherits(tout, "error")){

    if(isTRUE(warn))warning('The output for ', fname, ' returned an error, Please check data or parameters for species ', spname, '.')

    return(NA)

  } else {

    if(isTRUE(verbose))message('The fucntion ', fname, ' implemented successfully for species ', spname, '.')

    return(tout)
  }
}


#' @title Flags outliers or no outliers for multiple outlier methods.
#'
#' @param var Environmental parameter considered in flagging suspicious outliers.
#' @param output Either \strong{clean}: for a data set with no outliers, or \strong{outlier}: to output a dataframe with outliers.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param ifpar Isolation forest parameter settings. Parmeters of the isolation model that are required include
#'     the \strong{cutoff} to be used for denoting outliers. It ranges from \strong{0 to 1} but Default \strong{0.5}. Also,
#'     the \strong{size} of data partitioning for training should be determined. For more details check \strong{(Liu et al. 2008)}
#' @param methods Outlier detection methods considered. Use \strong{\code{extractMethod}} to get outliers implemented in the package.
#' @param df Dataframe with species occurrences
#' @param colsp Column with species names.
#' @param optpar Parameters for species optimal ranges like temperatures ranges \link[specleanr]{multidetect}.
#' @param kmpar Parameters for kmeans clustering like method and number of clusters for tunning \link[specleanr]{multidetect}.
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
#'

detect <- function(df,
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
                   verbose=FALSE,
                   spname=NULL,
                   warn=TRUE){

  if(missing(df)) stop('Species data missing')

  if(length((colnames(df)[colnames(df)==var]))<1) stop('Variable ', var, ' is  not found in the species data provided for species ', spname, ' .')

  if(is.null(optpar$sciname) && ('optimal'%in%methods)==TRUE){

    stop('Provide the scientific name for the species to execute the optimal method')
  }

  methodList <- list()

  for (cii in methods) {

    if(cii=='reference'){

      methodList[[cii]] = df

    }else if (cii=='optimal'){


      methodList[[cii]] =  tcatch(bulkopt(data = df, min = optpar$min, max = optpar$max, species=optpar$species,
                                            var = var, optimal = optpar$df, sciname=optpar$sciname, output = output),
                                  fname = cii, verbose = verbose, spname = spname,
                                  warn=warn)

    }else if (cii=='adjbox'){

      methodList[[cii]]  = tcatch(adjustboxplots(data = df, var = var, output = output),
                                  fname = cii, verbose = verbose, spname = spname,
                                  warn=warn)

    }else if(cii=='zscore'){

      methodList[[cii]] = tcatch(zscore(data = df, var = var, output = output, mode = zpar$mode, type = zpar$type),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='iqr'){

      methodList[[cii]] = tcatch(interquartile(data = df, var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='semiqr'){

      methodList[[cii]] = tcatch(semiIQR(data = df, var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='hampel'){

      methodList[[cii]] = tcatch(hampel(data = df, var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='jknife'){

      methodList[[cii]] = tcatch(jknife(data = df, var = var, output = output, mode = jkpar$mode),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='mahal'){

      methodList[[cii]] = tcatch(mahal(data = df, exclude = exclude, output = output, mode=mahalpar$mode),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='kmeans'){

      methodList[[cii]] = tcatch(xkmeans(data = df, k= kmpar$k, exclude = exclude, output = output, mode = kmpar$mode,
                                          method = kmpar$method, verbose=verbose),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)
    }else if(cii=='kmedoid'){

      methodList[[cii]] = tcatch(xkmedoid(data = df, k = kmedoidpar$k, metric = kmedoidpar$metric, output = output, exclude = exclude, x=1.5),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='iForest'){

      methodList[[cii]] = tcatch(isoforest(data = df, size = ifpar$size, output=output, cutoff = ifpar$cutoff, exclude = exclude),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='onesvm'){

      methodList[[cii]] = tcatch(onesvm(data = df,  exclude = exclude, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='lof'){

      methodList[[cii]] = tcatch(xlof(data = df, output =output, minPts = lofpar$minPts,
                                       exclude = exclude, metric = lofpar$metric, mode=lofpar$mode),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='logboxplot'){

      methodList[[cii]] = tcatch(logboxplot(data = df,  var = var, output = output, x= 1.5),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='medianrule'){

      methodList[[cii]] = tcatch(logboxplot(data = df,  var = var, output = output, x= 2.3),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='distboxplot'){

      methodList[[cii]] = tcatch(distboxplot(data = df,  var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='seqfences'){

      methodList[[cii]] = tcatch(seqfences(data = df,  var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='mixediqr'){

      methodList[[cii]] = tcatch(mixediqr(data = df,  var = var, output = output),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='glosh'){

      methodList[[cii]] = tcatch(xglosh(data = df, k = gloshpar$k,  output = output, metric = gloshpar$metric, mode=gloshpar$mode, exclude = exclude),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else if(cii=='knn'){

      methodList[[cii]] = tcatch(xknn(data = df, output = output, metric = knnpar$metric, mode=knnpar$mode, exclude = exclude),
                                 fname = cii, verbose = verbose, spname = spname,
                                 warn=warn)

    }else{
      message('No outlier detection method selected')
    }
  }
  return(methodList)
}



#' @title Outlier detection with multiple functions.
#'
#' @param data Dataframe or list of data sets for multiple or single species after after of extraction of environment predictors.
#' @param var A variable to check for outliers especially the one with directly affects species distribution such as
#' maximum temperature of the coldest month for bioclimatic variables ((IUCN Standards and Petitions Committee, 2022)) or
#' stream power index for hydromorphological parameters (Logez et al., 2012).
#' @param output zzzzzz
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param ifpar Isolation forest parameter settings. Parameters of the isolation model that are required include
#'     the \strong{cutoff} to be used for denoting outliers. It ranges from \strong{0 to 1} but Default \strong{0.5}. Also,
#'     the \strong{size} of data partitioning for training should be determined. For more details check \strong{(Liu et al. 2008)}
#' @param methods Outlier detection methods considered. Use \strong{\code{extractMethod}} to get outliers implemented in the package.
#' @param multiple zz
#' @param colsp zzz
#' @param optpar Parameters for species optimal ranges like temperatures ranges \link[specleanr]{multidetect}.
#' @param kmpar Parameters for kmeans clustering like method and number of clusters for tunning \link[specleanr]{multidetect}.
#' @param kmedoidpar Parameters for adjusting the distance metrics used in kmedoid method \link[specleanr]{multidetect}.
#' @param lofpar Parameters for local outlier factor such as the distance matrix and mode of method implementation
#'  such as robust and soft modes \link[specleanr]{multidetect}.
#' @param jkpar Parameters for reverse jack knifing mainly the mode used \link[specleanr]{multidetect}.
#' @param gloshpar Parameters for global local outlier score from hierarchies such as distance metric used \link[specleanr]{multidetect}.
#' @param mahalpar Parameters for Malahanobis distance which includes varying the mode of output \link[specleanr]{multidetect}.
#' @param knnpar Parameters for varying the distance matrix such euclidean or Manhattan \link[specleanr]{multidetect}.
#' @param zpar Parameters for z-score such mode and x parameter \link[specleanr]{multidetect}.
#' @param spname species name being handled
#' @param verbose whether to return messages or not. Default FALSE.
#' @param warn whether to return warning or not. Default TRUE.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun
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
#' rdata <- predextract(data = datacheck,
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
#'                         kmpar =list(k = 6, method='silhouette'),
#'                         ifpar = list(cutoff = 0.5, size=0.7))
#'
#'
#' }
#'
#'@reference
#'IUCN Standards and Petitions Committee. (2022). THE IUCN RED LIST OF THREATENED SPECIESTM Guidelines for Using the IUCN Red List
#'Categories and Criteria Prepared by the Standards and Petitions Committee of the IUCN Species Survival Commission.
#'https://www.iucnredlist.org/documents/RedListGuidelines.pdf.

multidetect <- function(data,
                        var,
                        output,
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
                        verbose=FALSE, spname=NULL,warn=TRUE){


  allowedmethods <- c('reference','adjbox', 'zscore','kmeans', 'iForest', 'distboxplot','optimal',
                      'mixediqr', 'seqfences', 'mahal', 'medianrule', 'iqr','hampel',
                      'logboxplot', 'onesvm', 'jknife', 'semiqr', 'lof','glosh', 'knn')

  tfcheck <- methods%in%allowedmethods

  if(any(tfcheck==FALSE)){

    notsupported <- methods[which(tfcheck==FALSE)]

    stop('The methods ', paste(notsupported, collapse = ','), ' are not accepted.')
  }

  #check for number of species

  if(isTRUE(multiple) && !is(data, 'list') && is.null(colsp)){
    stop('For multiple species dataframe, set provide the column species in the colsp parameter.')
  }

  #check if there enough data to run mahalanobis distance matrix

  if(isFALSE(multiple) && is.null(colsp) ){

    if(nrow(data)<ncol(data)){

      if(isTRUE(warn))warning('Number of rows are less than variables and some methods may not function properly.')

      outdata <-  detect(df = data, var = var, output = output,
                          exclude = exclude,optpar = optpar,
                          kmpar = kmpar, ifpar = ifpar, jkpar = jkpar,
                          mahalpar = mahalpar, lofpar = lofpar, kmedoidpar = kmedoidpar,
                          zpar = zpar, gloshpar = gloshpar,
                          knnpar = knnpar,
                          methods = methods,
                         verbose = verbose,
                         spname = spname,warn=warn)
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

      d <-  detect(df = dfinal, var = var, output = output, colsp=colsp,
                    exclude = exclude,optpar = optpar,
                    kmpar = kmpar, ifpar = ifpar, jkpar = jkpar,
                    mahalpar = mahalpar, lofpar = lofpar, kmedoidpar = kmedoidpar,
                    zpar = zpar, gloshpar = gloshpar, knnpar = knnpar,
                    methods = methods,
                   verbose = verbose,
                   spname = spname,warn=warn)
      outdata[[mdi]] <- d
    }
  }

  return(new('datacleaner', result = outdata, mode = multiple, varused = var, out = output,
             methodsused = methods))
}


