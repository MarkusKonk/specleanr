#' @title Extract final clean data using either absolute or best method generated outliers.
#'
#' @param data The reference data for the species used in outlier detection.
#' @param outliers Output from the outlier detection process.
#' @param sp species name. NULL if a single species is considered in outlier detection.
#' @param mode Either absolute to use absolute outliers to filter data or bestmethod to outliers from best method
#' @param threshold value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param colsp A parameter to be used if the \code{data} is a data frame and the user must indicate the column wih species names.
#' @param warn If \strong{FALSE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param pabs Percentage of outliers allowed to be extracted from the data. If \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'      the absolute outliers are removed instead. This because some records  in the best methods are repeated and they will likely to remove true values as outliers.
#' @param verbose Produces messages or not. Default \strong{FALSE}.
#' ###param multiple TRUE for multiple species and FALSE for single species considered during outlier detection.
#'
#' @return cleaned data frame of species records.


clean_data <- function(data, outliers, sp=NULL, mode = 'best', colsp = NULL, threshold = NULL,
                       verbose=FALSE, warn=FALSE, pabs = 0.1, autothreshold=FALSE){

  var <- outliers@varused

  match.argc(mode, choices = c('best', 'abs'))

  if(outliers@mode==FALSE){

    if(mode =='best'){

      bs <- bestmethod(x= outliers,  threshold = threshold, warn = warn, verbose = verbose,
                       autothreshold = autothreshold)

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
          datOut <- ocindex(x= outliers, absolute = TRUE, threshold = threshold, warn = warn, autothreshold = FALSE)
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
        datOut <- ocindex(x= outliers, absolute = TRUE, threshold = threshold, warn = warn,
                          autothreshold = FALSE)
      }
      varc <- unlist(data[, var])

      indx <- which(!varc %in% datOut)

      datIn <- data[indx,]
    }

  }else{

    if(mode =='best'){

      bs <- bestmethod(x= outliers, sp= sp,  threshold = threshold, warn = warn, verbose = verbose, autothreshold = autothreshold)

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

          datOut <- ocindex(x= outliers,  sp =sp, absolute = TRUE, threshold = threshold, warn = warn,
                            autothreshold = FALSE)
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
        datOut <- ocindex(x= outliers,  sp =sp, absolute = TRUE, threshold = threshold, warn = warn,
                          autothreshold = FALSE)
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
#' @param refdata Species data frame from precleaned analysis
#' @param outliers Output from the outlier detection
#' @param threshold value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param mode Either all to consider all similarity measure to consider suitable method or custom.
#'  to indicate the similarity measures or interest. simple matching coefficient, jaccard, sorensen,
#'  and overlap coefficient can be used.
#'  Otherwise a data frame will be produced. Default FALSE.
#' @param warn If \strong{TRUE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param verbose Produces messages or not. Default \strong{FALSE}.
#' @param colsp Column with species names.
#' @param pabs Percentage of outliers allowed to be extracted from the data. If \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'      the absolute outliers are removed instead. This because some records  in the best methods are repeated and they will likely to remove true values as outliers.

#'
#' @return Either a list or data frame of cleaned records for multiple species.
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

clean_data_extract <- function(refdata, outliers, mode ='best',colsp = NULL, threshold =NULL, warn=FALSE, verbose=FALSE,
                               autothreshold =FALSE, pabs = 0.1){

  if(deparse(substitute(refdata))!=outliers@dfname)stop('The dataset for species occurences and outliers are different.')

  var <- outliers@varused

  #for a single species: clean data extraction

  if(outliers@mode==FALSE){

    dfdata <- clean_data(data = refdata, outliers = outliers, mode = mode, threshold = threshold, verbose = verbose,
                         warn = warn, pabs = pabs, autothreshold = autothreshold, colsp = colsp)

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
                                       pabs = pabs),

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



