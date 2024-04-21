#' @title Extract final clean data using either absolute or best method generated outliers.
#'
#' @param data Species data frame from precleaned analsyis
#' @param outliers Output from the outlier detection
#' @param sp species name. NULL if a single species is considered in outlier detection.
#' @param var variable used to identify outliers in the univariate outlier detection methods and species optimal ranges.
#' @param type Either absolute to use absolute outliers to filter data or bestmethod to outliers from best method
#' @param threshold value to consider whether the outlier is an absolute outlier or not.
#' @param mode Either all to consider all similarity measure to consider best method or custom
#'  to indicate the similarity measures or interest. simple matching coefficient, jaccard, sorensen,
#'  and overlap coefficient can be used.
#' @param warn If \strong{TRUE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param verbose Produces messages or not. Default \strong{FALSE}.
#'
#' @return cleaned data frame of species records.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' a = 1+1
#'
#' }
#'
extract_clean<- function(data, outliers, sp=NULL, var, type = 'bestmethod',
                         threshold =.6, mode='all', verbose=FALSE, warn=TRUE ){#absolute from extract_clean to extract

  match.arg(type, choices = c('bestmethod', 'absolute'))

  if(isTRUE(outliers@mode)==TRUE && is.null(sp)){

    stop('Provide a particular name or index')

  }else if(isTRUE(outliers@mode)==FALSE && !is.null(sp)) {

    stop('Either species name or index is not required if outliers was done on a single species')

  }else if(isTRUE(outliers@mode)==FALSE && is.null(sp)){

    if(type =='bestmethod'){

      bs <- bm(x= outliers, var = var, threshold = threshold, mode = mode,
               warn = warn, verbose = verbose)

      varc <- unlist(data[, var])

      datOut <- outliers@result[[bs]][[var]]

      indx <- which(!varc%in%datOut)

      datIn <- data[indx,]

    }else{

      datOut <- ocindex(x= outliers, var = var, absolute = TRUE, threshold = threshold, warn = warn)

      varc <- unlist(data[, var])

      indx <- which(!varc %in% datOut)

      datIn <- data[indx,]
    }

  }else{
    if(type =='bestmethod'){

      bs <- bm(x= outliers, sp= sp, var = var, threshold = threshold, mode = mode, warn = warn, verbose = verbose)

      df <- data[[sp]]

      varc <- unlist(df[, var])

      datOut <- outliers@result[[sp]][[bs]][[var]]

      indx <- which(!varc%in%datOut)

      datIn <- df[indx,]

    }else{

      datOut <- ocindex(x= outliers, sp= sp, var = var, absolute = TRUE, threshold = threshold)

      df <- data[[sp]]

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
#' @param mode Either all to consider all similarity measure to consider suitable method or custom
#'  to indicate the similarity measures or interest. simple matching coefficient, jaccard, sorensen,
#'  and overlap coefficient can be used.
#' @param list If TRUE the a list of multiple species data will be produced.
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
multextract <- function(data, outliers, var, type ='bestmethod',colsp = NULL,
                        threshold, mode = 'all', list=TRUE, warn=TRUE, verbose=TRUE){

  #If the species are in a list or data frame
  if(is(data,'data.frame')&& is.null(colsp))stop('Provide the column with species names.')


  if(is(data, 'list')){

    if(length(data)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

    dfinal <- data

  }else if(is(data, 'data.frame') && !is.null(colsp)){

    dfinal <- split(data, f= data[,colsp])

    if(length(dfinal)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

  }else{
    stop('Only list and dataframe from pred_extract function should be used')
  }

  spdata <- list()

  for (dv in seq_along(dfinal)) {

    spnames <- names(dfinal)[dv]

    dx <- dfinal[[dv]]

    cx <- tryCatch(extract_clean(data = dfinal, outliers = outliers, sp = spnames, var = var,
                                 type = type, mode = mode, threshold = threshold, warn = warn, verbose = verbose),

                   error=function(e) {
                     if(isTRUE(verbose)) message('No absolute outliers found and the reference data will be provided.')
                     return(dx)
                   } )

    if(!inherits(cx, 'error')){

      spdata[[dv]] <- cx

    }else{
      spdata[[dv]] <- dx
    }
    spdata[[dv]]['species'] <- spnames

    dfdata <- do.call(rbind, spdata)

  }
  return(dfdata)
}
