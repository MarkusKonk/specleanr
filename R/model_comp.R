#' @title Comparing the model performance before and after outlier removal.
#'
#' @description
#' The function is used to compare model performance before and outlier removal.
#' The different thresholds are indicated and the model performance outputs are obtained
#' based on threshold dependent and independent metrics such as accuracy, sensitivity,
#' specificity, true skill statistics, and Area Under the Curve.
#'
#' @param refdata \code{list} or \code{dataframe} of species data generated during
#'   pre-cleaning process. This data set is used as the reference data in
#'   accessing the outlier detection performance using both threshold dependent
#'   and independent metrics. This data set must be same as when in detecting
#'   outliers to avoid mismatch.
#' @param outliers \code{datacleaner} class data set with all outliers
#'   obtained using the \code{multdetect} function.
#' @param mode \code{character} of either \code{abs} to use absolute outliers to
#'   filter data or \code{best} to use the best method to filter the data
#'   \code{\link{bestmethod}} function.
#' @param models \code{character}. The models to be used to examine the relationship between
#'   species occurrences and environmental parameters. Only Random forest and
#'   generalized linear models are accepted. \code{GLM} is used to set to
#'   Generalized Linear Models and \code{RF} for Random Forest. \code{RF1}
#'   variant is slower and is suggested if \code{RF} fails.
#' @param metrics \code{character}. Either to only consider threshold-dependent \code{dep},
#'   threshold-independent \code{indep} or all evaluation metrics \code{all} to determine model
#'   performance.
#' @param binary \code{logical}. If \code{FALSE} the species dataset do not have
#'   label column for presence absence and \code{label} is NULL.
#'   If only species presences and absences  are indicated,
#'   the user should select \code{TRUE}.
#' @param autothreshold \code{logical}. Identifies the threshold with mean number
#'   of absolute outliers.The search is limited within 0.51 to 1 since
#'   thresholds less than are deemed inappropriate for identifying absolute
#'   outliers. The autothreshold is used when \code{threshold} is set to
#'   \code{NULL}.
#' @param colsp \code{character}. Column with species names if the species occurrence is a
#'   data frame not list,
#' @param raster \code{SpatRaster}. The environmental where pseudo absences are extracted using
#'   \code{\link{envextract}} function.
#' @param thresholds \code{numeric}. The thresholds ranging from 0.1 to 1 but greater than 0.5 is advisable.
#' @param nboots \code{numeric} Creating sub samples for modeling evaluation. At least more than 5 is advisable.
#' @param testprop \code{numeric}. The proportion to be used for partitioning data. Ranges from 0.1 to 1.
#'      Less than 0.5 is advisable to allow have more model training data.
#' @param lat,lon \code{character}. Column names for latitude/longitude if the species
#'  occurrences don't have the geometry column or not spatial vector file.
#'  The latitude and longitude must be
#'   provided for data extraction from the raster layers.
#' @param geom \code{character}. Column name used to indicate geometry column during data extraction when the species
#'   occurrences is of \code{sf} class and have geometry column instead of latitude
#'   and longitude.
#' @param prbackground \code{numeric}. Value indicating the proportion of pseudo
#'      absences extracted in relation to the species presences. Used in \code{spatSample} function.
#'      Ranges from at least 0.1 to 1. Check \code{(Barbet-Massin et al., 2012)} on the number of
#'      pseudo absences required during species distribution modeling.
#' @param verbose \code{logical}. Return messages or not. Default is \code{FALSE}.
#' @param warn \code{logical}. Return warning or not. Default is \code{FALSE}.
#' @param pabs \code{numeric}. Percentage of outliers allowed to be extracted from the data. If
#'   \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'   the absolute outliers are removed instead. This because some records  in
#'   the best methods are repeated and they will likely to remove true values as
#'   outliers.
#' @param full \code{logical}. Either full model output for \code{TRUE} or \code{FALSE} for only
#'   performance metrics output.
#' @param minpts \code{numeric} value indicating the minimum number of records required to run the species
#'   distribution model. Default is \code{10}.
#' @param loessthreshold whether to consider loess or not.
#' @param plot true or false
#' @param ... Other parameters can be used from \code{\link{envextract}}
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
#' getper <- get_performance(modelcomp = modeout)
#'
#' ggperform(modelout = modeout)
#'
#' }
#'
#' @seealso \code{\link{sdmfit}}, \code{\link{multidetect}}, \code{\link{pred_extract}}, \code{\link{envextract}}, \code{\link{boots}}
#'
#' @references
#' Barbet-Massin, M., Jiguet, F., Albert, C. H., & Thuiller, W. (2012).
#' Selecting pseudo-absences for species distribution models:
#' how, where and how many? Methods Ecol Evol 3: 327–338.
#'
modelcomparison <- function(refdata, outliers, raster, models = 'GLM', metrics ='all',
                            thresholds = NULL, loessthreshold = FALSE, autothreshold = FALSE,
                         mode='abs',colsp =NULL, nboots= 10,
                         testprop=0.2, geom = NULL, lat = NULL, lon = NULL,
                         prbackground=1, binary=FALSE, verbose = F, warn = F, full = FALSE,
                         pabs = 0.1, minpts=10, plot=FALSE,...){


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

  #sequence through species

  #thresholds by loess function

  #spthresholds <- threshold_opt(refdata = refdata, outliers = outliers, mode = mode)

  for (mp in seq_along(df)) {

    if(outliers@mode==TRUE) spl <- names(df)[mp]

    ref <- "REF" #reference dataset to avoid masking 1.0 threshold

    if(loessthreshold==TRUE){
      #run the loess threshold finder
      #get for one species
      if(outliers@mode==FALSE){
        thresholdvalues <- optimal_threshold(refdata = refdata, outliers = outliers, plot = plot)

        sfinal <- unname(thresholdvalues)[2] #pick the maxima at index 2
      }else{
        #for multiple species
        thresholdvalues<- optimal_threshold(refdata = refdata, outliers = outliers, plot = plot,
                                           colsp = colsp)
        #it generates a dataframe for all species
        #extract for the species in the loop
        speciesthreshold <- unlist(thresholdvalues$species)
        speciesinloop <- spl
        #get maxima or optimal threshold
        sfinal <- unlist(thresholdvalues$maxima)[which(speciesthreshold==speciesinloop)]
      }

    }else{
      #remove the zero threshold in case its indicated by the user
      checkzero <- thresholds%in%0

      if(any(checkzero) ==TRUE) sfinal <- thresholds[which(checkzero==FALSE)] else sfinal <- thresholds
    }

    #merge both the references and other thresholds
    tvalues <- c(ref, sfinal)

    #loop through the thresholds
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



