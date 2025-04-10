#' @title Extract outliers for a one species
#'
#' @param x \code{list}. Outlier outputs for both single and multiple species.
#' @param sp \code{string}. Species name or index in the list from datacleaner output. NULL for a single species
#'
#' @return \code{data frame} Outliers for each method
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(efidata)
#'
#' db <- sf::read_sf(system.file('extdata/danube/basinfinal.shp', package = "specleanr"), quiet = TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' checkname <- check_names(data=efidata, colsp ='scientificName', pct = 90, merge = T)
#'
#' extdf <- pred_extract(data = checkname, raster = wcd,
#'                       lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = 'speciescheck',
#'                      list = TRUE,verbose = F,
#'                      minpts = 6,merge = F)#basin removed
#'
#'  #outlier detection
#'
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"),
#'                          showErrors = FALSE, warn = TRUE, verbose = FALSE, sdm = TRUE)
#'
#' extoutlier <- extractoutliers(x=outliersdf, sp = 8)
#'
#' }
#'
#'
extractoutliers <- function(x, sp = NULL){

  if(!is(x, 'datacleaner')) stop('Only datacleaner class is accpeted')

  if(x@out!='outlier') stop('Only extracts outliers yet clean data has been produced.')

  if(isFALSE(x@mode)){

    dx <- x@result

    checknull <- sapply(dx, nrow) #methods which genuinely failed to implement

    metds <- dx[!sapply(checknull,is.null)]

    dxlists <- sapply(names(metds), function(xx){

      totout <- nrow(metds[[xx]])

      df <- data.frame(method = xx, totaloutliers = totout)

    }, simplify = FALSE)

    dfinal <- do.call(rbind, dxlists)

    row.names(dfinal) <- NULL

    return(dfinal)
  }else{

    if(!is.null(sp)) {

      dx <- x@result[sp]

      if(is.null(unlist(dx))) stop("Either index ", sp, " is out of bounds or the variable was not considered in the outlier detection.", call. = FALSE)

    } else {

      dx <- x@result

    }
    #loop through species
    spdata <- sapply(names(dx), function(spnames){

      spdf <- dx[[spnames]]

      checknull <- sapply(spdf, nrow)

      metds <- spdf[!sapply(checknull,is.null)]

      dxlists <- sapply(names(metds), function(xx){

        totout <- nrow(metds[[xx]])

        df <- data.frame(method = xx, totaloutliers = totout)

      }, simplify = FALSE)

      dfout <- do.call(rbind, dxlists)

      dfout["groups"] <- spnames

      return(dfout)

    }, simplify = FALSE)

    dfinal <- do.call(rbind, spdata)

    row.names(dfinal) <- NULL

  }
  return(dfinal)
}
