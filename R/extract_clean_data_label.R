#' @noRd
#'
cleandata_label <- function(refdata, outliers, sp = NULL,
                            threshold = 0.1, warn = FALSE, classify = 'med'){

  #var of interest
  varc <- outliers@varused

  if(outliers@mode==FALSE){
    var <- varc
  }else{
    if(length(varc)>1) var <- sp else var <- varc
  }

  #get absolute outliers# set the threshold to 0.1 to cater for outlier identified.

  absoutliers <- ocindex(x=outliers,
                         sp = sp,
                         props = TRUE,
                         absolute = TRUE, threshold = threshold,
                         warn = warn)

  #get the column data
  vardata <- unlist(refdata[, var])

  props <- absoutliers$absolute_propn

  if(classify == 'med'){

    labelval <- c('perfect outlier', 'very strong', 'moderate', 'fair', 'poor')

    labdata <- sapply(labelval, function(xlab){

      if(xlab == "poor"){

        cvals <- which(props <= 0.2 & props >= 0.1)

      }else if(xlab =='fair'){
        cvals <- which(props <= 0.5 & props >= 0.3)

      }else if(xlab == "moderate"){
        cvals <- which(props <= 0.7 & props >= 0.6)

      }else if(xlab == "very strong"){
        cvals <- which(props <= 0.9 & props >= 0.8)

      }else{
        cvals <- which(props >= 1)
      }
      if(length(cvals)>=1){

        outvect <- absoutliers[cvals,]$absoluteoutliers

        indxout <- which(vardata %in% outvect)

        outdata <- refdata[indxout,]

        outdata["label"] <- xlab

        return(outdata)
      }else{
        outdata <- NULL
      }
    },simplify = FALSE, USE.NAMES = FALSE)

    #extract list of outlier labeled data
    outdf <- do.call(rbind, labdata)

  }else if(classify == 'ps'){

    labelval <- c('perfect', 'strong', 'moderate', 'weak')

    labdata <- sapply(labelval, function(xlab){

      if(xlab == "weak"){

        cvals <- which(props <= 0.3 & props >= 0.1)

      }else if(xlab =='moderate'){
        cvals <- which(props <= 0.4 & props >= 0.6)

      }else if(xlab == "strong"){
        cvals <- which(props <= 0.9 & props >= 0.7)

      }else{
        cvals <- which(props >= 1)
      }
      if(length(cvals)>=1){

        outvect <- absoutliers[cvals,]$absoluteoutliers

        indxout <- which(vardata %in% outvect)

        outdata <- refdata[indxout,]

        outdata["label"] <- xlab

        return(outdata)
      }else{
        outdata <- NULL
      }
    },simplify = FALSE, USE.NAMES = FALSE)

    #extract list of outlier labeled data
    outdf <- do.call(rbind, labdata)

  }else if(classify == 'po'){

    labelval <- c('perfect', 'very strong', 'strong', 'moderate', 'weak', 'negligible')

    labdata <- sapply(labelval, function(xlab){

      if(xlab == "negligible"){

        cvals <- which(props <= 0.1)

      }else if(xlab =='weak'){
        cvals <- which(props <= 0.2)

      }else if(xlab == "moderate"){
        cvals <- which(props <= 0.3)

      }else if(xlab == "strong"){
        cvals <- which(props <= 0.6 & props >= 0.4)

      }else if(xlab == "very strong"){
        cvals <- which(props <= 0.9 & props >= 0.7)

      }else{
        cvals <- which(props >= 1)
      }
      if(length(cvals)>=1){

        outvect <- absoutliers[cvals,]$absoluteoutliers

        indxout <- which(vardata %in% outvect)

        outdata <- refdata[indxout,]

        outdata["label"] <- xlab

        return(outdata)
      }else{
        outdata <- NULL
      }
    },simplify = FALSE, USE.NAMES = FALSE)

    #extract list of outlier labeled data
    outdf <- do.call(rbind, labdata)

  }else{
    stop('Only me, po, or ps are allowed')
  }

  #extract clean data
  indxclean <- which(!vardata %in% outdf[, var])

  #get clean data and label only if index is greater than 0: some data was not labelled.

  if(length(indxclean)>0) {

    cleandf <- refdata[indxclean,]

    cleandf["label"] <- "not outlier"

    cleanoutdf <- rbind(cleandf, outdf)
  }else{
    cleanoutdf <- outdf
  }
  return(cleanoutdf)
}


#' @title Extract final clean data using either absolute or best method generated outliers.
#'
#' @param refdata \code{dataframe}. The reference data for the species used in outlier detection.
#' @param outliers \code{string}. Output from the outlier detection process.
#' @param threshold \code{numeric}. Value to consider whether the outlier is an absolute outlier or not.
#' @param colsp \code{string}. A parameter to be used if the \code{data} is a data frame and the user must indicate the column with species names.
#' @param warn \code{logical}. If \strong{FALSE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param verbose \code{logical}. Produces messages or not. Default \strong{FALSE}.
#'
#' @details
#'
#' Outlier cluster weights were based on statistical classification of coefficients mostly for correlation based on \code{Akoglu 2018}.
#' They are classified based on three naming standards, namely Dancey & Reidy (Physchology), Quinni piac University (Politics) and Chan YH medicine.
#' All classifications have been used in the function and each affects the data clusters. The default is Chan YH (medicine).
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
#' extractabs <- extract_clean_data_label(refdata = rdata, outliers = out_df)
#' }
#'
#' @export
#'
#' @references Akoglu, H. 2018. User’s guide to correlation coefficients. - Turk J Emerg Med 18: 91–93.

extract_cleandata_label <- function(refdata, outliers, colsp = NULL,
                                    threshold = 0.1, warn=FALSE,
                                    verbose = TRUE,classify = 'med'){

  if(deparse(substitute(refdata))!= outliers@dfname)stop('The reference dataset used in outlier detection and the output of outlier detection are different.')

  #handle multiple variables especially for general data not species distribution models.
  var <- outliers@varused

  #for a single data labeling

  if(outliers@mode==FALSE){

    splist <- list(data = refdata)

    #multiple data labeling
  }else{

    if(is(refdata, 'list')){

      if(length(refdata)!= length(outliers@result)) stop('Number of species in refdata and outlier detection are not equal')

      splist <- refdata

      #multiple species in a dataframe

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

    cdata <- tryCatch(cleandata_label(refdata = splist[[fd]],
                                      outliers = outliers,
                                      sp = spnames,
                                      threshold = threshold,
                                      warn = warn,
                                      classify = classify),

                      error=function(e){

                        if(grepl("No absolute", e$message)==TRUE) {

                          if(isTRUE(verbose))  message('No absolute outlier so all the data will have a clean label.')

                          return(NULL)
                        } else {
                          message("Check input data for ", spnames)
                        }

                      }
    )

    if(!is.null(cdata)) {
      spdata <- cdata
    } else {
      spdata <- splist[[fd]]
      spdata['label'] <- 'not outlier'
    }

    if(outliers@mode==FALSE) spdata else spdata['groups'] <- fd

    spdata
  }, simplify = FALSE, USE.NAMES = FALSE)

  dfcleaned <- do.call(rbind, dfdata)

  return(dfcleaned)
}
