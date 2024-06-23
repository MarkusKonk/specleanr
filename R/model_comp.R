#' @title Comparing the model performance before and after outlier removal.
#'
#' @param refdata Species data generated during pre-cleaning process. This data set is used as the reference data in accessing the outlier detection performance using both
#'      threshold dependent and independent metrics. This data set must be same as when in detecting outliers to avoid mismatch.
#' @param outliers The \code{datacleaner} class data set with all outliers obtained using the \code{multdetect} function. Please check \code{ocindex}
#' @param mode Either \code{abs} to use absolute outliers to filter data or \code{best} for best method to outliers from best method.
#' @param models The models to be used to examine the relationship between species occurrences and environmental parameters. Only Random forest and generalized linear
#'      models are accepted. \code{GLM} is used to set to Generalized Linear Models and \code{RF} for Random
#'      Forest. \code{RF1} variant is slower and is suggested if \code{RF} fails.
#' @param metrics Either to only consider threshold-dependent, threshold-independent or all evaluation metrics to determine model performance.
#' @param binary Either \code{FALSE} if the species dataset do not have label column for presence absence. Therefore, the \code{FALSE} is used
#'        when the parameter \code{label} is NULL. If only species presences and absences are indicated, the user should select \code{TRUE} option.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param species A list of species names and they must be contained in the outlier data set and precleaned data.
#' @param exclude Excluded the columns before checking for outliers. Automatic identification of these from the datacleaner is implemented.
#' @param colsp The column with species names if the species occurrence is a data frame not list,
#' @param raster The environmental where pseudo absences are extracted using \link[specleanr]{envextract}.
#' @param thresholds The different scenarios are implemented. Currently the reference data set and threshold, outlier cleaned data are allowed.
#' @param nboots Creating sub samples for modeling evaluation.
#' @param testprop The probability to be used for partitioning data.
#' @param lat If the species occurrences don't have the geometry column or not spatial vector file, the latitude must be provided for data extraction form the raster layers.
#' @param lon If the species occurrences don't have the geometry column or not spatial vector file, the longitude must be provided for data extraction form the raster layers.
#' @param geom the coordinates column from the species occurrence data. It should be included from the data at the outlier detection step to avoid
#' considering the cordinate din outlier removal.
#' @param prbackground the proportion of pseudo absences compare to the species presences.
#' @param geom Is used in data extraction when the species occurrences geometry column instead of latitude and longitude.
#' @param verbose Return messages or not. Default is \code{FALSE}.
#' @param warn Return warning or not. Default is \code{FALSE}.
#' @param pabs Percentage of outliers allowed to be extracted from the data. If \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'      the absolute outliers are removed instead. This because some records  in the best methods are repeated and they will likely to remove true values as outliers.
#' @param ... other parameters can be used from \link[specleanr]{envextract} function.
#'
#' @return A list of model instance and model performance metrics.
#'
#' @export
#'
#' @examples
#'
#'
#' @seealso \link[specleanr]{sdmfit}
#' @seealso \link[specleanr]{multidetect}
#' @seealso \link[specleanr]{pred_extract}
#' @seealso \link[specleanr]{envextract}
#' @seealso \link[specleanr]{boots}
#'
modelcomparison <- function(refdata, outliers, raster, models = 'GLM', metrics ='all',
                            thresholds = c(0.6, 0.7), autothreshold = FALSE,
                         mode='best', exclude =NULL, colsp =NULL, species = NULL, nboots= 10,
                         testprop=0.2, geom = NULL, lat = NULL, lon = NULL,
                         prbackground=1, binary=FALSE, verbose = F, warn = F, full = FALSE, pabs = 0.1,...){

  perfdf <- list()
  modelist <- list()
  modelout <- list()

  for (spl in species) {

    ref <- 1.1 #reference dataset to avoid masking 1.0 threshold

    checkzero <- thresholds%in%0

    if(any(checkzero) ==TRUE) sfinal <- thresholds[which(checkzero==FALSE)] else sfinal <- thresholds

    tvalues <- c(ref, sfinal)*100

    for (tv in 1:length(tvalues)){

      mdl = tvalues[tv]


      if(is(refdata, "data.frame")){

        df <- split(refdata, f = refdata[, colsp])

      }else{
        df <- refdata
      }

      if(mdl==110){

        dfspx <- df[[spl]]

      }else if(mdl !=110){

        #reconvert to allowed thresholds

        reconvtr <- mdl/100 #use it in threshold parameter for extract_clean data

        print(reconvtr)

        #try and catch methods without absolute outliers that returns and error.

        dfspx <- tryCatch(expr =  extract_clean_data(refdata = df, outliers = outliers, sp=spl,
                                                warn = warn, colsp = colsp, mode = mode,
                                                threshold = reconvtr, autothreshold = autothreshold,
                                                verbose = verbose, pabs = pabs ),
                          error = function(e) e)

        if(inherits(dfspx, 'error')){

          if(isTRUE(verbose)) message('No absolute outliers at a threshold of ', reconvtr,', so data is similar to reference dataset.' )

          dfspx <- df[[spl]]
        }

      }else{
        stop('Mode not recorganised.')
      }

      dataprep <- envextract(occurences = dfspx, raster = raster, lat = lat,
                             lon = lon, geom = geom, binary = binary,...)

      btdata <- boots(data = dataprep, nboots = nboots, testprob = testprop)

      modelout[[tv]] <- sdmfit(data=btdata, models = models, metrics = metrics, full = full)

      names(modelout)[tv]  <- mdl

      attr(modelout,'records')[tv] <- nrow(dataprep)

    }
    modelist[[spl]] = modelout

  }
  return(modelist)
}

#' @title Extract the model performance
#'
#' @param modeloutput The model output object where data is extracted.
#' @param type Either test or train data
#'
#' @return A data frame with performance metrics between reference and outlier cleaned dataset.
#' @export
#'
#' @examples

extract_performance <- function(modeloutput, type='test'){

  match.arg(type, choices = c('train', 'test'))

  perfdata <- list()
  splist <- list()
  runlist<- list()
  pdata <- list()

  for (nm in 1:length(modeloutput)) {

    spname = names(modeloutput)[nm]

    spd = modeloutput[[spname]]

    for(ref in 1:length(spd)){

      rec<- attr(spd, 'records')[ref]

      refnames <- names(spd)[ref]

      modedata <- spd[[refnames]]

      for(r in 1:length(modedata)){

        rundata <- modedata[[r]]

        pdata[[r]]  <- switch (type, test = rundata[[4]], train=rundata[[5]] )

        runlist <- do.call(rbind, pdata)

        runlist$species = spname

        runlist$scenario = refnames

        runlist$records = rec

      }

      perfdata[[ref]] <- runlist

      pfinal <- do.call(rbind, perfdata)

    }
    splist[[nm]] <- pfinal

    runfinal <- do.call(rbind, splist)

  }
  return(runfinal)
}


