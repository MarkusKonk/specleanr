#' @title Extract final clean data using either absolute or best method generated outliers.
#'
#' @param data Species data frame from precleaned analsyis.
#' @param outliers Output from the outlier detection
#' @param sp species name. NULL if a single species is considered in outlier detection.
#' @param var variable used to identify outliers in the univariate outlier detection methods and species optimal ranges.
#' @param type Either absolute to use absolute outliers to filter data or bestmethod to outliers from best method
#' @param threshold value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param similarity_tests indicate the similarity_tests to consider including jaccard, overlap,sorensen, ocindex, and smc.
#' @param mode Either all to consider all similarity measure to consider best method or custom
#'  to indicate the similarity measures or interest. simple matching coefficient, jaccard, sorensen,
#'  and overlap coefficient can be used.
#' @param colsp A parameter to be used if the \code{data} is a data frame and the user must indicate the column wih species names.
#' @param warn If \strong{FALSE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param pabs Percentage of outliers allowed to be extracted from the data. If \code{bestmethod} is used to extract outliers and the \code{pabs} is exceeded,
#'      the absolute outliers are removed instead. This because some records  in the best methods are repeated and they will likely to remove true values as outliers.
#' @param verbose Produces messages or not. Default \strong{FALSE}.
#'
#' @return cleaned data frame of species records.
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
#'  #extracting use the absolute method for one species
#'
#'  extractclean_abs <- extract_clean_data(data = precleaned, outliers = out_df, sp = 1,
#'  var = 'bio6', type = 'absolute')
#'
#'  bestmout <- extract_clean_data(data = rdata, outliers = out_df, sp = 1,
#'  var = 'bio6', type = 'bestmethod')

#'
#' }
#'
extract_clean_data<- function(data, outliers, sp=NULL, var, type = 'bestmethod', colsp = NULL,
                         similarity_tests=NULL,threshold = NULL, mode='all',
                         verbose=TRUE, warn=TRUE, pabs =0.1,
                         autothreshold=FALSE){

  match.arg(type, choices = c('bestmethod', 'absolute'))

  var0 <- outliers@varused

  if(!var0==var)stop('Different variable `-', var,'-´ is provided for data cleaning which differs from `-', var0,'-´ used for outlier detection.')

  if(is(data,'data.frame') && is.null(colsp) && isTRUE(outliers@mode)){

    stop('Provide the column with species names in parameter, ´colsp´.')

  }else if(is(data, 'list')){

    df = data[[sp]]

  }else if(is(data, 'data.frame')){

    df = data[data[,colsp] == sp, ]

  }else{
    stop('Only lists of species datasets or a dataframe with a column of species names are accepted.')
  }

  if(isTRUE(outliers@mode)==TRUE && is.null(sp)){

    stop('Provide a particular name or index')

  }else if(isTRUE(outliers@mode)==FALSE && !is.null(sp)) {

    stop('Either species name or index is not required if outliers was done on a single species')

  }else if(isTRUE(outliers@mode)==FALSE && is.null(sp)){

    if(type =='bestmethod'){

      bs <- bestmethod(x= outliers, var = var, threshold = threshold, mode = mode,
                       similarity_tests = similarity_tests,
                       warn = warn, verbose = verbose, autothreshold = autothreshold)

      varc <- unlist(df[, var])

      datOut <- outliers@result[[bs]][[var]]

      indx <- which(!varc%in%datOut)

      #best method sometimes flags records that are repeated across the total species records.
      #the selection will change to absolute outlier removal type.
      pctabs <- (length(varc)-length(indx))/length(varc)

      if(pctabs>=pabs){

        if(isTRUE(autothreshold)){
          datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE,
                            threshold = NULL, warn = warn,
                            autothreshold = TRUE)
        }else{
          datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE,
                            threshold = threshold, warn = warn,
                            autothreshold = FALSE)
        }

        if(isTRUE(verbose)) message('Even if ´bestmethod´ is selected rows removed exceed ', pabs, '%, so only absolute outliers will be removed.' )

        varc <- unlist(df[, var])

        indx <- which(!varc %in% datOut)

        datIn <- df[indx,]

      }else{
        datIn <- df[indx,]
      }

    }else{

      if(isTRUE(autothreshold)){
        datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE, threshold = NULL, warn = warn,
                          autothreshold = TRUE)
      }else{
        datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE, threshold = threshold, warn = warn,
                          autothreshold = FALSE)
      }
      varc <- unlist(df[, var])

      indx <- which(!varc %in% datOut)

      datIn <- df[indx,]
    }

  }else{
    if(type =='bestmethod'){

      bs <- bestmethod(x= outliers, sp= sp, var = var, threshold = threshold, mode = mode,
                       warn = warn, verbose = verbose, autothreshold = autothreshold)

      varc <- unlist(df[, var])

      datOut <- outliers@result[[sp]][[bs]][[var]]

      indx <- which(!varc%in%datOut)

      #best method sometimes flags records that are repeated across the total species records.
      #the selection will change to absolute outlier removal type.
      pctabs <- (length(varc)-length(indx))/length(varc)

      if(pctabs>=pabs){

        if(isTRUE(autothreshold)){
          datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE, threshold = NULL, warn = warn,
                            autothreshold = TRUE)

        }else{
          datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE,
                            threshold = threshold, warn = warn,
                            autothreshold = FALSE)
        }

        if(isTRUE(verbose)) message('Even if ´bestmethod´ is selected rows removed exceed ', pabs, '%, so only absolute outliers will be removed.' )

        varc <- unlist(df[, var])

        indx <- which(!varc %in% datOut)

        datIn <- df[indx,]

      }else{
        datIn <- df[indx,]
      }
    }else{

      if(isTRUE(autothreshold)){
        datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE,
                          threshold = NULL, warn = warn,
                          autothreshold = TRUE)
      }else{
        datOut <- ocindex(x= outliers, var = var, sp =sp, absolute = TRUE,
                          threshold = threshold, warn = warn,
                          autothreshold = FALSE)
      }

      varc <- unlist(df[, var])

      indx <- which(!varc %in% datOut)

      datIn <- df[indx,]
    }
  }

  return(datIn)
}

#Extract for multiple species

#' @title Extract cleaned record for multiple species.
#'
#' @param data Species data frame from precleaned analysis
#' @param outliers Output from the outlier detection
#' @param var variable considered in outlier detection.
#' @param type Either absolute to use absolute outliers to filter data or suitable method to outliers from suitable method.
#' @param threshold value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param mode Either all to consider all similarity measure to consider suitable method or custom
#'  to indicate the similarity measures or interest. simple matching coefficient, jaccard, sorensen,
#'  and overlap coefficient can be used.
#'  Otherwise a data frame will be produced. Default FALSE.
#' @param warn If \strong{TRUE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param verbose Produces messages or not. Default \strong{FALSE}.
#' @param colsp Column with species names.
#'
#' @return Either a list or data frame of cleaned records for multiple species.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
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
#'  #extracting use the absolute method for one species
#'
#'  multextract_abs <- multextract_clean(data = precleaned, outliers = out_df,
#'  var = 'bio6', type = 'absolute', threshold = 0.6, autothreshold = FALSE)
#'
#'  bestmout_bm <- multextract_clean(data = rdata, outliers = out_df,
#'  var = 'bio6', type = 'bestmethod', threshold = 0.6, autothreshold = FALSE)
#'
#'
#' }
#'
#'
#'

multextract_clean <- function(data, outliers, var, type ='bestmethod',colsp = NULL,
                        threshold =NULL, mode = 'all', warn=FALSE, verbose=FALSE,
                        autothreshold =FALSE){

  if(deparse(substitute(data))!=outliers@dfname)stop('The dataset for species occurences and outliers are different.')

  if(is(data, 'list')){

    if(length(data)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

    splist <- data

  } else if(is(data, 'data.frame') && !is.null(colsp)){

    splist <- split(data, f= data[,colsp])

    if(length(splist)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

  }else{
    stop('Only list or dataframe of species occurences accepted or set the `colsp parameter`.')
  }

  spdata <- list()

  for (dv in seq_along(splist)) {

    spnames <- names(splist)[dv]

    if(is(data, 'data.frame'))  refdata<- data[data[,colsp] == spnames, ] else  refdata<- data[[spnames]]

    cx <- tryCatch(expr = extract_clean_data(data = data, outliers = outliers, sp = spnames, var = var,
                                        type = type, mode = mode, threshold = threshold, colsp = colsp,
                                        warn = warn, verbose = verbose, autothreshold = autothreshold),

                   error=function(e) e)

    if(!inherits(cx, 'error')){

      spdata[[dv]] <- cx

    }else{

      if(isTRUE(verbose)) message('No absolute outliers found for ', spnames, ' and the reference data will be provided.')

      spdata[[dv]] <- refdata
    }
    spdata[[dv]]['species'] <- spnames

    dfdata <- do.call(rbind, spdata)

  }
  return(dfdata)
}

