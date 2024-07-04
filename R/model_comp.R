#' @title Comparing the model performance before and after outlier removal.
#'
#' @param refdata Species data generated during pre-cleaning process. This data set is used as the reference data in accessing the outlier detection performance using both
#'      threshold dependent and independent metrics. This data set must be same as when in detecting outliers to avoid mismatch.
#' @param outliers The \code{datacleaner} class data set with all outliers obtained using the \code{multdetect} function. Please check \code{ocindex}.
#' @param mode Either \code{abs} to use absolute outliers to filter data or \code{best} to use the best method to filter the data \code{\link{bestmethod}}.
#' @param models The models to be used to examine the relationship between species occurrences and environmental parameters. Only Random forest and generalized linear
#'      models are accepted. \code{GLM} is used to set to Generalized Linear Models and \code{RF} for Random
#'      Forest. \code{RF1} variant is slower and is suggested if \code{RF} fails.
#' @param metrics Either to only consider threshold-dependent, threshold-independent or all evaluation metrics to determine model performance.
#' @param binary Either \code{FALSE} if the species dataset do not have label column for presence absence. Therefore, the \code{FALSE} is used
#'        when the parameter \code{label} is NULL. If only species presences and absences are indicated, the user should select \code{TRUE} option.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' #param species A list of species names and they must be contained in the outlier data set and precleaned data.
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
#' @param full Either full model output for \code{TRUE} or \code{FALSE} for only performance metrics output.
#' @param minpts Minimum number of records required to run the species distribution model.
#' @param ... other parameters can be used from \code{\link{envextract}} function.
#'
#' @return A list of model instance and model performance metrics.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  data(jdsdata)
#'
#' data(efidata)
#'
#' matchdata <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
#'                            lats = 'lat', lons = 'lon',
#'                            country = 'JDS4_site_ID',
#'                            species = c('scientificName', 'speciesname'),
#'                           date=c('sampling_date','Date'))
#'
#' datacheck <- check_names(matchdata, colsp= 'species', pct = 90, merge =TRUE, verbose = FALSE)
#'
#' #select species with enough records
#'
#' datacheckf <- subset(datacheck, subset = speciescheck %in% c("Salmo trutta",
#' "Gasterosteus aculeatus", "Squalius cephalus"))
#'
#' #basin to get the bounding box to delineate the area of concern
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)
#'
#' #worldclim data for to extract enviromental predictctors
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' #initial data extraction and preliminary analysis
#'
#' rdata <- pred_extract(data = datacheckf,
#'                      raster= worldclim ,
#'                      lat = 'decimalLatitude',
#'                      lon= 'decimalLongitude',
#'                      colsp = 'speciescheck',
#'                     bbox  = db,
#'                     multiple = TRUE,
#'                      minpts = 10,
#'                      list=TRUE,
#'                      merge=F, verbose = F)
#'
#'
#' #apply ensemble outlier detection. Note: (x and y in exclude parameter are
#' #internally generally in pred_extract during environmental data extraction )
#'
#'outliers <- multidetect(data = rdata, multiple = TRUE,
#'                        var = 'bio6',
#'                        output = 'outlier',
#'                        exclude = c('x','y'),
#'                        methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))
#'
#'
#' modecom <- modelcomparison(refdata = rdata, outliers = outliers, raster = worldclim,
#'                           lat = 'y', lon = 'x', models = c("GLM"),
#'                           mode = 'best', testprop = 0.2, metrics = 'indep',
#'                          thresholds = seq(0.2, 0.9, 0.1), full = FALSE)
#'
#' modelperfom <- get_performance(modecom)
#'
#' }
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
                         mode='best',colsp =NULL, nboots= 10,
                         testprop=0.2, geom = NULL, lat = NULL, lon = NULL,
                         prbackground=1, binary=FALSE, verbose = F, warn = F, full = FALSE, pabs = 0.1, minpts=10,...){

  modelist <- list()
  modelout <- list()

  #if multiple species are considered
  if(outliers@mode==TRUE){

    if(is(refdata, "data.frame")){

      if(is.null(colsp)) stop('Provide the column with species names in parameter, colsp .')

      df <- split(refdata, f = refdata[, colsp])

    }else{
      df <- refdata
    }
  }else{
    #single species considered in outlier detection.
    df <- list(refdata)
  }

  for (mp in seq_along(df)) {

    if(outliers@mode==TRUE) spl <- names(df)[mp]

    ref <- "REF" #reference dataset to avoid masking 1.0 threshold

    #remove threshold in case its indicated by the user

    checkzero <- thresholds%in%0

    if(any(checkzero) ==TRUE) sfinal <- thresholds[which(checkzero==FALSE)] else sfinal <- thresholds

    tvalues <- c(ref, sfinal)

    for (tv in 1:length(tvalues)){

      mdl = tvalues[tv]

      if(mdl=="REF"){

        dfspx <- df[[mp]]

        if(outliers@mode==FALSE) spv <- NULL else spv <-  spl


      }else if(mdl !="REF"){

        dfsp<- df[[mp]]

        mdl <- as.numeric(mdl)

        #For a single species, the sp parameter is NULL but other parameters remain the same
        if(outliers@mode==FALSE) spv <- NULL else spv <-  spl

        #the sp parameter is assigned the species name
        dfspx <- tryCatch(expr =  clean_data(data = dfsp, outliers = outliers, sp=spv, warn = warn, colsp = colsp, mode = mode,
                                                threshold = mdl, autothreshold = autothreshold, verbose = verbose, pabs = pabs ),
                          error = function(e) e)

        if(inherits(dfspx, 'error')){

          if(isTRUE(verbose)) message('No absolute outliers at a threshold of ', mdl,', so data is similar to reference dataset.' )

          dfspx <- df[[mp]]
        }

      }else{
        stop('Mode not recorganised.')
      }

      if(nrow(dfspx)>=minpts){

        dataprep <- envextract(occurences = dfspx, raster = raster, lat = lat,
                               lon = lon, geom = geom, binary = binary,...)

        btdata <- boots(data = dataprep, nboots = nboots, testprob = testprop)

        modelout[[tv]] <- sdmfit(data=btdata, models = models, metrics = metrics, full = full)

        names(modelout)[tv]  <- mdl

        #attr(modelout,'records')[tv] <- nrow(dataprep)

      }else{

        warning("The threshold option of ", mdl ,"  didnot run as the ", nrow(dfspx), " records for species ",spv, " are too few for a species distribution models.")

         modelout[[tv]] <- NA

        names(modelout)[tv]  <- mdl

        #attr(modelout,'records')[tv] <- nrow(dfspx)
      }
    }
    if(outliers@mode==TRUE) modelist[[spl]] = modelout else modelist[[mp]] = modelout

    attr(modelist, "full") <- full

  }
  return(modelist)
}

#' @title Extract the model performance
#'
#' @param modeloutput The model output object where data is extracted.
#' @param type Either test or train performance metrics.
#'
#' @return A data frame with performance metrics between reference and outlier cleaned dataset. This function takes the output from \code{\link{modelcomparison}} function.
#'
#' @export
#'
get_performance <- function(modeloutput, type='test'){

  match.argc(type, choices = c('train', 'test'))

  perfdata <- list()
  splist <- list()
  runlist<- list()
  pdata <- list()

 #run through the species
  for (nm in 1:length(modeloutput)) {

    if(is.null(names(modeloutput)[nm])) spname <-  1 else spname <- names(modeloutput)[nm]

    spd = modeloutput[[spname]]

    #remove species elements which didnot execute

    lenout <- sapply(spd, length)

    if(any(lenout<=1)) spd <- within(spd, rm(list=names(spd[lenout <=1]))) else  spd

    #run through the thresholds from REF to 0.9..
    for(ref in 1:length(spd)){

      refnames <- names(spd)[ref] #the thresholds

      modedata <- spd[[refnames]]

      for(r in 1:length(modedata)){

        rundata <- modedata[[r]] #check if all thresholds executed

        if((attributes(modeloutput)$full)==FALSE){

          pdata[[r]]  <- switch (type, test = rundata[[1]], train=rundata[[2]] )

        }else{
          pdata[[r]]  <- switch (type, test = rundata[[4]], train=rundata[[5]] )

        }

        runlist <- do.call(rbind, pdata)

       runlist$species = spname

        runlist$scenario = refnames

      }

      perfdata[[ref]] <- runlist

      pfinal <- do.call(rbind, perfdata)

    }
    splist[[nm]] <- pfinal

    runfinal <- do.call(rbind, splist)


  }
  return(runfinal)
}


