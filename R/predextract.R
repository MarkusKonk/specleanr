
#' @title  Preliminary data cleaning including removing duplicates, records
#'   outside a particular basin, and NAs.
#'
#' @param data \code{dataframe}. Data frame with multiple species or only one
#'   species for checking records with no coordinates, duplicates, and check for
#'   records that fall on land, sea, country or city centroids, and geographical
#'   outliers(Zzika et al., 2022).
#' @param raster \code{raster}. Environmental layers from different providers
#'   such as WORLDCLIM (), Hydrogaphy90m (), CHELSA, Copernicus ().
#' @param lat,lon \code{coordinates}. variable for latitude and longitude column
#'   names.
#' @param colsp \code{string}. variable already in the data.
#' @param minpts \code{numeric}. Minimum number of records for the species after
#'   removing duplicates and those within a particular basin.
#' @param multiple \code{logical}. TRUE if species are more than one and FALSE for one species
#'   in the data.
#' @param list \code{logical}. If TRUE the a list of multiple species data frames will be
#'   generated and FALSE for a dataframe of species data sets. Default TRUE
#' @param bbox \code{sf} or \code{vector}. Object of class 'shapefile' If only a particular basin is
#'   considered. Bounding box vector points can also be provided in the form
#'   \code{"c(xmin, ymin, xmax, ymax)"}. \code{xmin} is the minimum longitude,
#'   \code{ymin} is the minimum latitude, \code{xmax} is the maximum longitude
#'   and \code{xmax} is the minimum latitude.
#' @param verbose \code{logical}. if TRUE message and warnings will be produced. Default \code{TRUE}.
#' @param warn \code{logical}. indicating to whether to show implementation warning or
#'   not. Default \code{FALSE}.
#' @param merge \code{logical}. To add the other columns in the species data after data
#'   extraction. Default \strong{TRUE}.
#'
#' @importFrom sf st_drop_geometry st_crs st_coordinates st_filter st_as_sf
#'   st_bbox st_set_crs st_as_sfc
#' @importFrom terra extract ext
#'
#' @return \code{dataframe} or \code{list} of precleaned data sets for single or multiple species.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' danube <- system.file('extdata/danube/basinfinal.shp', package='specleanr')
#'
#' danubebasin <- sf::st_read(danube, quiet=TRUE)
#'
#' #Get environmental data
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' referencedata <- pred_extract(data = efidata,
#'                           raster= worldclim ,
#'                           lat ="decimalLatitude",
#'                           lon = 'decimalLongitude',
#'                           colsp = 'scientificName',
#'                           bbox = danubebasin,
#'                           multiple = TRUE,
#'                           list= TRUE, #list will be generated for all species
#'                           minpts = 7, merge=T)
#' }
#'
#'

pred_extract <- function(data, raster, lat, lon, bbox =NULL, colsp, minpts =10,
                         multiple=TRUE, list=TRUE, merge=FALSE, verbose= FALSE, warn = FALSE){

  if(missing(data)) stop('Data frame with species record missing')

  if(minpts<=5) stop('Minimum number of species records should be  atleast greater than ', minpts,'. Default is 10')

  if(length((colnames(data)[colnames(data)==colsp]))<1) stop(colsp, ' is  not found in the ', deparse(substitute(data)),' data provided')

  #To ungroup data if tidyverse function were applied on it.

  if(is(data, 'grouped_df')|| is(data, 'tbl') ||is(data, 'tbd_df')) dfnew <- as.data.frame(data) else dfnew <- data

  #check and removing missing coordinates

  species_lon_df <- dfnew[complete.cases(dfnew[ , c(lat, lon, colsp)]), ]

  #check if the species coordinates are within the bounding the bounding box of the raster layer

  rasterbbox <- as.vector(terra::ext(raster))

  xmin1 <- unname(rasterbbox[1])
  ymin1 <- unname(rasterbbox[2])
  xmax2 <- unname(rasterbbox[3])
  ymax2 <- unname(rasterbbox[4])

  xmin11 <- min(unlist(species_lon_df[, lon]))
  ymin11 <- min(unlist(species_lon_df[, lat]))
  xmax12 <- max(unlist(species_lon_df[, lon]))
  ymax12 <- max(unlist(species_lon_df[, lat]))

  if(isTRUE(warn)) if(xmin11<xmin1)warning("Some species points are outside the raster layers provided. Please check xmin.", call. = FALSE)

  if(isTRUE(warn)) if(ymin11<ymin1)warning("Some species points are outside the raster layers provided. Please check ymin.", call. = FALSE)

  if(isTRUE(warn))  if(xmax12>xmax2)warning("Some species points are outside the raster layers provided. Please check xmax.", call. = FALSE)

  if(isTRUE(warn))  if(ymax12>ymax2)warning("Some species points are outside the raster layers provided. Please check ymax.", call. = FALSE)

  #change the object into sf file format to enable geographical filtering outside the bounding box using st_filter
  spdata_new <- species_lon_df |> sf::st_as_sf(coords = c(lon, lat), crs = st_crs(4326))

  #filter out records outside the bounding box if provided.
  if(!is.null(bbox)){

    if(inherits(bbox, "sf")) {

      basin_df <- sf::st_filter(spdata_new, bbox)

    }else if(inherits(bbox, 'numeric') && length(bbox) == 4){

        class(bbox) <- "bbox"


        bb <- st_as_sfc(bbox) |> sf::st_set_crs(st_crs(spdata_new))

        basin_df <- sf::st_filter(spdata_new, bb)
    }else{
      stop("The bounding box provided is wrong.")
    }
  } else{
    basin_df <- spdata_new
  }

  #remove duplicate data for coordinates and each species

  dupdata <- as.data.frame(basin_df[!duplicated(basin_df[c('geometry',colsp)]),])

  #check there is enough species data after duplicate removal for at some species

  zz <- as.data.frame(table(dupdata[,colsp]))$Freq

  if(zz[which.max(zz)]<minpts) stop('All species do not have enough data after removing missing values and duplicates.')

  #run through each species
  unx <- unique(unlist(dupdata[, colsp]))

  rastdata <- list()

  if(length(unx)<1){

    stop('Species column provided should atleast have one species name')

  }else if(length(unx)==1 && multiple==TRUE){

    stop('multiple is set to TRUE yet the data has only one species name. Change multiple to FALSE')

  }else if(length(unx)==1 && multiple==FALSE){

    spdfext <- dupdata |>  st_as_sf()

    if(nrow(spdfext)<minpts) warning(unx, ' has less than ', minpts, 'records')

    bext<- as.data.frame(terra::extract(raster, spdfext , ID=FALSE,xy=TRUE))

    if(isTRUE(merge)) {

      biodata <- cbind(bext, spdfext |>  st_drop_geometry())

      rownames(biodata) <- NULL

      }else {
        biodata <- bext
      }

  }else if(length(unx)>1 && multiple==TRUE){

    for (ci in seq_along(unx)) {

      spnames <- unlist(unx)[ci]

      spdfdata <- dupdata[dupdata[, colsp]==spnames,]

      spdfext <- spdfdata|>st_as_sf()

      if(nrow(spdfext)<minpts) {

        if(isTRUE(verbose)==TRUE) message(spnames, ' will be removed because the records are less than  ', minpts, '.')
        next
      }

      if(list==FALSE){

        rastdata[[ci]] <- as.data.frame(terra::extract(raster, spdfext , ID=FALSE,xy=TRUE))

        if(isTRUE(merge)) rastdata[[ci]]<- cbind(rastdata[[ci]], spdfext |>  st_drop_geometry()) else rastdata[[ci]][,'species'] <- spnames

        biodata <- do.call(rbind, rastdata)

      }else if (list==TRUE){

        rastdata[[ci]] <- as.data.frame(terra::extract(raster, spdfext , ID=FALSE,xy=TRUE))

        if(isTRUE(merge)) rastdata[[ci]]<- cbind(rastdata[[ci]], spdfext |>  st_drop_geometry()) else rastdata[[ci]]

        names(rastdata)[ci] <- spnames

        lss <- sapply(rastdata, length)

        if(any(lss==0)) ldata <- rastdata[lss !=0] else ldata <- rastdata

        biodata <- ldata


      }else{

        stop('set list either TRUE or FALSE to return either dataframe or lists of species data')
      }
    }

  }else{
    stop('Either the species column is invalid or the multiple attribute is set to FALSE for more than one species')
  }
  return(biodata)
}


