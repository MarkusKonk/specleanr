#' @title Comparing the model performance before and after outlier removal.
#'
#' @param refdata A list or dataframe of species data generated during
#'   pre-cleaning process. This data set is used as the reference data in
#'   accessing the outlier detection performance using both threshold dependent
#'   and independent metrics. This data set must be same as when in detecting
#'   outliers to avoid mismatch.
#' @param outliers The \code{datacleaner} class data set with all outliers
#'   obtained using the \code{multdetect} function. Please check \code{ocindex}.
#' @param mode A character of either \code{abs} to use absolute outliers to
#'   filter data or \code{best} to use the best method to filter the data
#'   \code{\link{bestmethod}}.
#' @param models The models to be used to examine the relationship between
#'   species occurrences and environmental parameters. Only Random forest and
#'   generalized linear models are accepted. \code{GLM} is used to set to
#'   Generalized Linear Models and \code{RF} for Random Forest. \code{RF1}
#'   variant is slower and is suggested if \code{RF} fails.
#' @param metrics Either to only consider threshold-dependent,
#'   threshold-independent or all evaluation metrics to determine model
#'   performance.
#' @param binary logical of \code{FALSE} if the species dataset do not have
#'   label column for presence absence. Therefore, the \code{FALSE} is used when
#'   the parameter \code{label} is NULL. If only species presences and absences
#'   are indicated, the user should select \code{TRUE} option.
#' @param autothreshold logical which identifies the threshold with mean number
#'   of absolute outliers.The search is limited within 0.51 to 1 since
#'   thresholds less than are deemed inappropriate for identifying absolute
#'   outliers. The autothreshold is used when \code{threshold} is set to
#'   \code{NULL}.
#' @param colsp The column with species names if the species occurrence is a
#'   data frame not list,
#' @param raster The environmental where pseudo absences are extracted using
#'   \link[specleanr]{envextract}.
#' @param thresholds The different scenarios are implemented. Currently the
#'   reference data set and threshold, outlier cleaned data are allowed.
#' @param nboots Creating sub samples for modeling evaluation.
#' @param testprop The probability to be used for partitioning data.
#' @param lat,lon character if the species occurrences don't have the geometry
#'   column or not spatial vector file, the latitude and longitude must be
#'   provided for data extraction from the raster layers.
#' @param prbackground numeric value indicating the proportion of pseudo
#'      absences compare to the species presences. Used in \code{spatSample} function.
#'      Ranges from at least 0.1 to 1
#' @param geom character which is used in data extraction when the species
#'   occurrences is of \code{sf} class and have geometry column instead of latitude
#'   and longitude.
#' @param verbose Return messages or not. Default is \code{FALSE}.
#' @param warn Return warning or not. Default is \code{FALSE}.
#' @param pabs Percentage of outliers allowed to be extracted from the data. If
#'   \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'   the absolute outliers are removed instead. This because some records  in
#'   the best methods are repeated and they will likely to remove true values as
#'   outliers.
#' @param full logical of either full model output for \code{TRUE} or \code{FALSE} for only
#'   performance metrics output.
#' @param minpts numeric value indicating the minimum number of records required to run the species
#'   distribution model. Default is \code{10}.
#' @param ... other parameters can be used from \code{\link{envextract}}
#'   function.
#'
#' @return A list of model instance and model performance metrics.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #worldclim data for to extract environmental predictors

#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' #get 500 records online using getdata function to compliment salmo trutta records and basin polygon
#' #basin to get the bounding box to delineate the area of concern
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)
#'
#' salmonline <- getdata(data = "Salmo trutta", gbiflim = 500, inatlim = 3, vertlim = 3, bbox = db)
#'
#' salextract <- extract_online(salmonline)
#'
#' #merge both online and offline data and filter Salmo trutta
#'
#' #select species with enough records
#'
#' datafinal <- salextract[salextract[,'species'] == "Salmo trutta", ]
#'
#' #initial data extraction and preliminary analysis
#'
#' rdata <- pred_extract(data = datafinal,
#'                       raster= worldclim ,
#'                       lat = 'decimalLatitude',
#'                       lon= 'decimalLongitude',
#'                       colsp = 'species',
#'                       bbox  = db,
#'                       multiple = FALSE,
#'                       minpts = 10,
#'                       list=TRUE,
#'                       merge=F, verbose = F)
#'
#'
#' #apply ensemble outlier detection. Note: (x and y in exclude parameter are
#' #internally generally in pred_extract during environmental data extraction )
#'
#' outliersdf <- multidetect(data = rdata, multiple = FALSE,
#'                           var = 'bio6',
#'                           output = 'outlier',
#'                           exclude = c('x','y'),
#'                           methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))
#'
#' modeout <- modelcomparison(refdata = rdata, outliers = outliersdf, raster = worldclim,
#'                            lat = 'y', lon = 'x', models = c("GLM"),
#'                            mode = 'best', testprop = 0.2, metrics = 'all',
#'                            thresholds = 0.2, full = FALSE, minpts = 10)
#'
#' getper <- get_performance(modeloutput = modeout)
#'
#' ggperform(modelout = modeout, cutoff = 0.1)
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
                         prbackground=1, binary=FALSE, verbose = F, warn = F, full = FALSE,
                         pabs = 0.1, minpts=10,...){


  if(all(1>thresholds)==FALSE)stop("The threshold values should ranges from at least 0.1 to 1")

  match.argc(mode, choices = c('best', 'abs'))

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

  modelist <- list()
  modelout <- list()

  for (mp in seq_along(df)) {

    if(outliers@mode==TRUE) spl <- names(df)[mp]

    ref <- "REF" #reference dataset to avoid masking 1.0 threshold

    #remove the zero threshold in case its indicated by the user

    checkzero <- thresholds%in%0

    if(any(checkzero) ==TRUE) sfinal <- thresholds[which(checkzero==FALSE)] else sfinal <- thresholds

    #merge both the references and other thresholds
    tvalues <- c(ref, sfinal)

    #loop through the thrsholds
    for (tv in 1:length(tvalues)){

      mdl = tvalues[tv]

      #specifically for reference since no data extraction is required.
      if(mdl=="REF"){

        dfspx <- df[[mp]]

        if(outliers@mode==FALSE) spv <- NULL else spv <-  spl

      #ohter references require data extraction.
      }else if(mdl !="REF"){

        dfsp<- df[[mp]]

        mdl <- as.numeric(mdl)

        #For a single species, the sp parameter is NULL but other parameters remain the same
        if(outliers@mode==FALSE) spv <- NULL else spv <-  spl

        #the sp parameter is assigned the species name
        #data extraction: try catch since if no absolute outliers are obtained with a particular
        #the function will break yet, it needs to run check the next threshold.
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
      #minpts allows no to break the SDMs as few records will not allow evlauting the models.
      if(nrow(dfspx)>=minpts){
        #data extraction and setting pesudo absences using terra::spatSample
        #for presence only species data: if species have presence/absence , the spatSample will not be used.
        dataprep <- envextract(occurences = dfspx, raster = raster, lat = lat,
                               lon = lon, geom = geom, binary = binary,...)

        #bootstrapping to create replicate samples, nboots can be set to any value
        btdata <- boots(data = dataprep, nboots = nboots, testprob = testprop)

        #fit species distribution models using with random forest or glm
        modelout[[tv]] <- sdmfit(data=btdata, models = models, metrics = metrics, full = full)

        names(modelout)[tv]  <- mdl

        #attr(modelout,'records')[tv] <- nrow(dataprep)

      }else{
        #if the records are few and cannot allow model evaluation, a warning is raised.
        warning("The threshold option of ", mdl ,"  didnot run as the ", nrow(dfspx), " records for species ",spv, " are too few for a species distribution models.")

         modelout[[tv]] <- NA

        names(modelout)[tv]  <- mdl

        #attr(modelout,'records')[tv] <- nrow(dfspx)
      }
    }
    #saving model outputs
    if(outliers@mode==TRUE) modelist[[spl]] = modelout else modelist[[mp]] = modelout

    #the attribute of full allows to only save the evaluation results and not model outputs as the memory intensive.
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
    #extract species name
    if(is.null(names(modeloutput)[nm])) spname <-  1 else spname <- names(modeloutput)[nm]

    spd = modeloutput[[spname]]

    #remove species elements which didn't execute
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
      #save data which has been extracted
      perfdata[[ref]] <- runlist

      pfinal <- do.call(rbind, perfdata)

    }
    splist[[nm]] <- pfinal

    runfinal <- do.call(rbind, splist)
  }
  return(runfinal)
}


