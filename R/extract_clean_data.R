#' @noRd
#'
cleandata <- function(data, outliers,
                           sp = NULL,
                           mode = 'best', threshold = NULL,
                           colsp = NULL,
                           warn = FALSE, verbose = FALSE,
                           autothreshold = FALSE,
                           pabs = 0.1, loess = FALSE ){
  var <- outliers@varused

  if(length(var)>1) var <- sp else var

  if(isTRUE(loess)){
    loess = TRUE
    optthreshold <- thresh_search(data = data, outliers = outliers,warn = warn,verbose = verbose)
    threshold = unname(optthreshold[2])
  }else if(!is.null(threshold)){
    threshold
  }else{
    autothreshold = TRUE
    threshold = NULL
    loess = FALSE
  }
  if(mode =='best'){
    bs <- bestmethod(x= outliers,
                     threshold =  threshold,
                     warn = warn,
                     verbose = verbose,
                     autothreshold = autothreshold)

    if(outliers@mode==FALSE) dataOut <- outliers@result[[bs]][[var]] else datOut <- outliers@result[[sp]][[bs]][[var]]

  }else{
    datOut <- ocindex(x= outliers,
                      absolute = TRUE,
                      threshold = threshold,
                      warn = warn,
                      autothreshold = autothreshold)
  }
  varc <- unlist(data[, var])

  indx <- which(!varc%in%datOut)

  pctabs <- (length(varc)-length(indx))/length(varc)

  if(pctabs >= pabs && mode=='best'){

    datOut <- ocindex(x= outliers, absolute = TRUE, threshold = threshold, warn = warn,
                      autothreshold = autothreshold)

    if(isTRUE(verbose)) message('the number of rows removed exceed ', pabs, ', so only absolute outliers removed.' )

    indx <- which(!varc %in% datOut)

    datIn <- data[indx,]

  }else{
    datIn <- data[indx,]
  }
  return(datIn)
}


#' @title Extract final clean data using either absolute or best method generated outliers.
#'
#' @param refdata \code{dataframe}. The reference data for the species used in outlier detection.
#' @param outliers \code{string}. Output from the outlier detection process.
#' @param mode \code{character}. Either \code{abs} to use absolute outliers to filter data or \code{best} to outliers from best method.
#' @param threshold \code{numeric}. Value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold \code{vector}. Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param colsp \code{string}. A parameter to be used if the \code{data} is a data frame and the user must indicate the column wih species names.
#' @param warn \code{logical}. If \strong{FALSE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param pabs \code{numeric}. Percentage of outliers allowed to be extracted from the data. If \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'      the absolute outliers are removed instead. This because some records  in the best methods are repeated and they will likely to remove true values as outliers.
#' @param verbose \code{logical}. Produces messages or not. Default \strong{FALSE}.
#' @param loess \code{logical}. Set to \code{TRUE} to use loess threshold optimization to extract clean data.
#' @param outlier_to_NA \code{logical} If \code{TRUE} a clean dataset will have outliers replaced with NAs.
#'        This parameter is experimented to ouput dataframe when multiple variables of concerns are considered
#'        during outlier detection.
#'
#' ###param multiple TRUE for multiple species and FALSE for single species considered during outlier detection.
#'
#' @return Either a \code{list} or \code{dataframe} of cleaned records for multiple species.
#'
#' @seealso \code{\link{thresh_search}}
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
#' extractabs <- extract_clean_data(refdata = rdata, outliers = out_df,
#'                                  mode = 'abs', threshold = 0.6,
#'                                  autothreshold = FALSE)
#'
#' bestmout_bm <- extract_clean_data(refdata = rdata, outliers = out_df,
#'                                   mode = 'best', threshold = 0.6,
#'                                  autothreshold = FALSE)
#' }
#'
#' @export
#'
#'

extract_clean_data <- function(refdata, outliers, mode ='best',colsp = NULL,
                               threshold =NULL, warn=FALSE, verbose=FALSE,
                               autothreshold =FALSE, pabs = 0.1, loess = FALSE,
                               outlier_to_NA  = FALSE){

  #the allowed modes: best for best method and abs : extract out only absolute outliers.
  match.argc(mode, choices = c('best', 'abs'))

  if(deparse(substitute(refdata))!= outliers@dfname)stop('The reference dataset used in outlier detection and the output of outlier detection are different.')

  #handle multiple variables especially for general data not species distribution models.
  var <- outliers@varused

  if(isTRUE(outlier_to_NA) && length(var)>1){

    for (ivar in var) {
      #try catch to handle parameters with no outliers
      absout <- tryCatch(expr =  ocindex(x=outliers, sp = ivar, absolute = TRUE,
                                         threshold = threshold,
                                         warn = warn),
                         error = function(e)return(NULL))
      if(!is.null(absout)){

        vals <- unlist(refdata[,ivar])

        datOut <- which(vals %in% absout)

        refdata[,ivar][datOut] <- NA

        dfcleaned <- refdata
      }else{
        message('No absolute outliers for ',ivar , ' at the indicated threshold of ', threshold)
      }
    }
  }else{

    #for a single species: clean data extraction

    if(outliers@mode==FALSE){

      splist <- list(data = refdata)

    }else{

      if(is(refdata, 'list')){

        if(length(refdata)!= length(outliers@result)) stop('Number of species in refdata and outlier detection are not equal')

        splist <- refdata

      } else if(is(refdata, 'data.frame')){

        if(length(outliers@varused)>1){

          vars <- outliers@varused

          splist <- sapply(vars, function(x) x <-  refdata, simplify = FALSE)

        }else{

          if(is.null(colsp)) stop('Provide the column with species names in parameter, colsp .')

          splist <- split(refdata, f= refdata[,colsp])

          if(length(splist)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')
        }

      }else{
        stop('Only list and dataframes accepted.')
      }
    }

    dfdata <- sapply(names(splist), function(fd){

      if(isFALSE(outliers@mode)) spnames <- NULL else spnames <- fd

      cdata <- tryCatch(cleandata(data = splist[[fd]], outliers = outliers,
                                        sp = spnames,
                                        mode = mode, threshold = threshold,
                                        colsp = colsp,
                                        warn = warn, verbose = verbose,
                                        autothreshold = autothreshold,
                                        pabs = pabs, loess = loess ),
                        error=function(e) return(NULL))

      if(!is.null(cdata)) spdata <- cdata else spdata <- splist[[fd]]
        spdata['species'] <- fd
        spdata
    }, simplify = FALSE)

    dfcleaned <- do.call(rbind, dfdata)
    rownames(dfcleaned) <- NULL
  }
  return(dfcleaned)
}
