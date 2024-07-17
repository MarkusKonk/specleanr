#' @title Extract final clean data using either absolute or best method generated outliers.
#'
#' @param data \code{dataframe}. The reference data for the species used in outlier detection.
#' @param outliers \code{string}. Output from the outlier detection process.
#' @param sp \code{string}. species name. NULL if a single species is considered in outlier detection.
#' @param mode \code{character}. Either \code{abs} to use absolute outliers to filter data or \code{best} to outliers from best method.
#' @param threshold \code{numeric}. Value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold \code{vector}. Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param colsp \code{string}. A parameter to be used if the \code{data} is a data frame and the user must indicate the column wih species names.
#' @param warn \code{logical}. If \strong{FALSE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param pabs \code{numeric}. Percentage of outliers allowed to be extracted from the data. If \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'      the absolute outliers are removed instead. This because some records  in the best methods are repeated and they will likely to remove true values as outliers.
#' @param verbose \code{logical}. Produces messages or not. Default \strong{FALSE}.
#' @param loess \code{logical}. Set to \code{TRUE} to use loess threshold optimisation to extract clean data.
#'
#' ###param multiple TRUE for multiple species and FALSE for single species considered during outlier detection.
#'
#' @return \code{dataframe} Cleaned dataframe of species records.
#'
#' @seealso \code{\link{thresh_search}}

clean_data <- function(data, outliers, sp=NULL, mode = 'best', colsp = NULL, threshold = NULL,
                       verbose=FALSE, warn=FALSE, pabs = 0.1, autothreshold=FALSE,
                       loess= FALSE){

  if(!is.null(threshold) && loess==TRUE) stop("Set either loess to FALSE and provide the threshold manually but not both.")

  var <- outliers@varused

  match.argc(mode, choices = c('best', 'abs'))

  if(outliers@mode==FALSE){

    if(mode =='best'){

      if(loess==TRUE && is.null(threshold)){

        optthreshold <- thresh_search(data = data, outliers = outliers, warn = warn, verbose = verbose)

        bs <- bestmethod(x= outliers,  threshold =  unname(optthreshold[2]), warn = warn, verbose = verbose,
                         autothreshold = FALSE)
      }else if(!is.null(threshold)){
        bs <- bestmethod(x= outliers,  threshold = threshold, warn = warn, verbose = verbose,
                         autothreshold = autothreshold)
      }else{
        bs <- bestmethod(x= outliers,  threshold = NULL, warn = warn, verbose = verbose,
                         autothreshold = TRUE)
      }

      varc <- unlist(data[, var])

      datOut <- outliers@result[[bs]][[var]]

      indx <- which(!varc%in%datOut)

      #best method sometimes flags records that are repeated across the total species records.

      #the selection will change to absolute outlier removal type if the percentage out the data to be removed exceeds pabs.

      pctabs <- (length(varc)-length(indx))/length(varc)*100

      pabsconv <- pabs*100

      if(pctabs>=pabsconv){

        if(isTRUE(autothreshold)){

          datOut <- ocindex(x= outliers, absolute = TRUE, threshold = NULL, warn = warn, autothreshold = TRUE)
        }else{

          if(loess==TRUE && is.null(threshold)){

            optthreshold <- thresh_search(data = data, outliers = outliers, warn = warn, verbose = verbose)

            datOut <- ocindex(x= outliers, absolute = TRUE, threshold = unname(optthreshold[2]), warn = warn,
                              autothreshold = FALSE)
          }else{
            datOut <- ocindex(x= outliers, absolute = TRUE, threshold = threshold, warn = warn,
                              autothreshold = FALSE)
          }

        }

        if(isTRUE(verbose)) message('If  bestmethod  is implemented, the number of rows removed exceed ', pabsconv, '%, so only absolute outliers will be removed.' )

        varc <- unlist(data[, var])

        indx <- which(!varc %in% datOut)

        datIn <- data[indx,]

      }else{
        datIn <- data[indx,]
      }

    }else{

      if(isTRUE(autothreshold)){

        datOut <- ocindex(x= outliers,  absolute = TRUE, threshold = NULL, warn = warn,
                          autothreshold = TRUE)
      }else{

        if(loess==TRUE && is.null(threshold)){

          optthreshold <- thresh_search(data = data, outliers = outliers, warn = warn, verbose = verbose)

          datOut <- ocindex(x= outliers, absolute = TRUE, threshold = unname(optthreshold[2]), warn = warn,
                            autothreshold = FALSE)
        }else{
          datOut <- ocindex(x= outliers, absolute = TRUE, threshold = threshold, warn = warn,
                            autothreshold = FALSE)
        }

      }
      varc <- unlist(data[, var])

      indx <- which(!varc %in% datOut)

      datIn <- data[indx,]
    }

  }else{

    if(mode =='best'){

      if(loess==TRUE && is.null(threshold)){

        optthreshold <- thresh_search(data = data, outliers = outliers, warn = warn, verbose = verbose)

        bs <- bestmethod(x= outliers, sp= sp,  threshold =  unname(optthreshold[2]), warn = warn, verbose = verbose,
                         autothreshold = FALSE)
      }else if(!is.null(threshold)){
        bs <- bestmethod(x= outliers, sp= sp,  threshold = threshold, warn = warn, verbose = verbose,
                         autothreshold = autothreshold)
      }else{
        bs <- bestmethod(x= outliers, sp= sp,  threshold = NULL, warn = warn, verbose = verbose,
                         autothreshold = TRUE)
      }

      varc <- unlist(data[, var])

      datOut <- outliers@result[[sp]][[bs]][[var]]

      indx <- which(!varc%in%datOut)

      #best method sometimes flags records that are repeated across the total species records.

      #the selection will change to absolute outlier removal type.

      pctabs <- (length(varc)-length(indx))/length(varc)*100
      pabsconv <- pabs*100

      if(pctabs>=pabsconv){

        if(isTRUE(autothreshold)){

          datOut <- ocindex(x= outliers, sp =sp, absolute = TRUE, threshold = NULL, warn = warn,
                            autothreshold = TRUE)

        }else{


          if(loess==TRUE && is.null(threshold)){

            optthreshold <- thresh_search(data = data, outliers = outliers, warn = warn, verbose = verbose)

            datOut <- ocindex(x= outliers,  sp =sp, absolute = TRUE, threshold = unname(optthreshold[2]), warn = warn,
                              autothreshold = FALSE)

          }else{
            datOut <- ocindex(x= outliers,  sp =sp, absolute = TRUE, threshold = threshold, warn = warn,
                              autothreshold = FALSE)
          }
        }

        if(isTRUE(verbose)) message('If  bestmethod  is implemented, the rows removed exceed ', pabsconv, '%, so only absolute outliers will be removed.' )

        varc <- unlist(data[, var])

        indx <- which(!varc %in% datOut)

        datIn <- data[indx,]

      }else{
        datIn <- data[indx,]
      }
    }else{

      if(isTRUE(autothreshold)){

        datOut <- ocindex(x= outliers,  sp =sp, absolute = TRUE,  threshold = NULL, warn = warn,
                          autothreshold = TRUE)
      }else{

        if(loess==TRUE && is.null(threshold)){

          optthreshold <- thresh_search(data = data, outliers = outliers, warn = warn, verbose = verbose)

          datOut <- ocindex(x= outliers,  sp =sp, absolute = TRUE, threshold = unname(optthreshold[2]), warn = warn,
                            autothreshold = FALSE)

        }else{
          datOut <- ocindex(x= outliers,  sp =sp, absolute = TRUE, threshold = threshold, warn = warn,
                            autothreshold = FALSE)
        }

      }

      varc <- unlist(data[, var])

      indx <- which(!varc %in% datOut)

      datIn <- data[indx,]
    }
  }

  return(datIn)
}




#' @title Extract cleaned record for multiple species.
#'
#' @param refdata \code{dataframe}. Species data frame from precleaned analysis
#' @inheritParams clean_data
#'
#' @return Either a \code{list} or \code{dataframe} of cleaned records for multiple species.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(jdsdata)
#' data(efidata)
#' matchdata <- match_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                             lats = 'lat',
#'                             lons = 'lon',
#'                             species = c('speciesname','scientificName'),
#'                             country= c('JDS4_site_ID'),
#'                             date=c('sampling_date', 'Date'))
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
#'                       raster= worldclim ,
#'                       lat = 'decimalLatitude',
#'                       lon= 'decimalLongitude',
#'                       colsp = 'speciescheck',
#'                       bbox = db,
#'                       multiple = TRUE,
#'                       minpts = 10,
#'                       list=TRUE,
#'                       merge=F)
#'
#'
#' out_df <- multidetect(data = rdata, multiple = TRUE,
#'                       var = 'bio6',
#'                       output = 'outlier',
#'                       exclude = c('x','y'),
#'                       methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))
#'
#' #extracting use the absolute method for one species
#'
#' extractabs <- clean_data_extract(refdata = rdata, outliers = out_df,
#'                                  mode = 'abs', threshold = 0.6,
#'                                  autothreshold = FALSE)
#'
#' bestmout_bm <- clean_data_extract(refdata = rdata, outliers = out_df,
#'                                   mode = 'best', threshold = 0.6,
#'                                  autothreshold = FALSE)
#' }
#'
#'
#'

clean_data_extract <- function(refdata, outliers, mode ='best',colsp = NULL,
                               threshold =NULL, warn=FALSE, verbose=FALSE,
                               autothreshold =FALSE, pabs = 0.1, loess = FALSE){

  if(deparse(substitute(refdata))!=outliers@dfname)stop('The dataset for species occurences and outliers are different.')

  var <- outliers@varused

  #for a single species: clean data extraction

  if(outliers@mode==FALSE){

    dfdata <- clean_data(data = refdata, outliers = outliers, mode = mode, threshold = threshold, verbose = verbose,
                         warn = warn, pabs = pabs, autothreshold = autothreshold, colsp = colsp,
                         loess = loess)

  }else if(outliers@mode==TRUE){

    if(is(refdata, 'list')){

      if(length(refdata)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

      splist <- refdata

    } else if(is(refdata, 'data.frame')){

      if(is.null(colsp)) stop('Provide the column with species names in parameter, colsp .')

      splist <- split(refdata, f= refdata[,colsp])

      if(length(splist)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

    }else{
      stop('Only list or dataframe of species occurences accepted or set the `colsp parameter`.')
    }
    spdata <- list()

    for (dv in seq_along(splist)) {

      spnames <- names(splist)[dv]

      if(is(refdata, 'data.frame'))  data2<- refdata[refdata[,colsp] == spnames, ] else  data2<- refdata[[spnames]]

      cx <- tryCatch(expr = clean_data(data = data2, outliers = outliers, sp = spnames,
                                       mode = mode, threshold = threshold, colsp = colsp,
                                       warn = warn, verbose = verbose, autothreshold = autothreshold,
                                       pabs = pabs, loess = loess),

                     error=function(e) e)
      if(!inherits(cx, 'error')){

        spdata[[dv]] <- cx

      }else{

        if(isTRUE(verbose)) message('No absolute outliers found for ', spnames, ' and the reference data will be provided.')

        spdata[[dv]] <- data2
      }
      spdata[[dv]]['species'] <- spnames

      dfdata <- do.call(rbind, spdata)

    }
  }else{
    stop("Either multiple or single species allowed.")
  }
  return(dfdata)
}



