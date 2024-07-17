
#'
#' @title Downloads the global basin data based on Tesedesco at al., 2017
#'
#' @param x \code{string} absolute path to cache global basin files
#'
#' @importFrom utils unzip download.file
#'
#' @return Download files for global basin
#'
#'
.gbdownload <- function(x='globalbasins'){#this will go into the mfn function for memoise

  fp <- .abspath(dir = x, verbose = F)#saved and first operations

  xfp <- .absx(fp, var = 'basinfiles')

  suppressMessages(suppressWarnings(suggested.packages(listpkgs=c("curl"),
                                                       reason="To check if internet is connected for the user pc to download data.")))

  if (!curl::has_internet()) stop('No internet connection, connect and try again later.')

  #Download the Tesedesco at al., 2017 basin data from figshare

  download.file(url = 'https://figshare.com/ndownloader/files/8964583',
                destfile = file.path(fp, 'gbshp.zip'), mode = 'wb')

  unzip(file.path(fp, 'gbshp.zip'), exdir = file.path(xfp))

  unlink(file.path(fp, 'gbshp.zip'))
  #load the saved shapefiles from the globalbasin folder created on the user computer.
  f <- list.files(path = xfp, pattern = '.shp$', full.names = TRUE)

  #read the sf files
  s <- sf::st_read(f, quiet=T)

  cat('Please cite this global basin dataset as:
      Tedesco, P., Beauchard, O., Bigorne, R. et al. A global database on freshwater fish species occurrence in drainage basins.
      Sci Data 4, 170141 (2017). https://doi.org/10.1038/sdata.2017.141', '\n')

  return(s)
}



#' @title Records within a particular basin range based on Tesedesco at al., 2017
#'
#' @param occurrences \code{dataframe}. Species occurrences for a particular to check for the basin where they lie in.
#' @param species \code{string}. Species name considered as its recorded in the occurrences.
#' @param lat,lon \code{coordinates}. Latitude and longitude column names in the species occurrences records.
#' @param verbose \code{logical}. Whether implementation messages are shown. Default \code{TRUE}.
#' @param discard \code{logical}. Whether to discard records with basins or not. Default \code{FALSE}.
#' @param output \code{character}. Whether to produce the records or the basin files. Default \code{records}.
#'
#' @return species occurrence records and their basins were they are found.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' sal <- subset(efidata, scientificName=='Anguilla anguilla')
#'
#' br <- basinranges(occurrences = sal, species = "Anguilla anguilla",
#'                   lat = 'decimalLatitude', lon = 'decimalLongitude')
#' }
#'
#'
basinranges <- function(occurrences, species, lat, lon, verbose=TRUE, discard=TRUE, output='records'){

  xdf <- as.data.frame(occurrences)

  if(all(c(lat, lon)%in%colnames(occurrences))==FALSE) stop("Both latitude and longitude must be in the data.")

  #convert the species data to sf format to be compatible with the global basin data downloaded from Tesedesco at al., 2017
  xdf_sf <- xdf |> sf::st_as_sf(coords=c(lon, lat), crs = st_crs(4326L))

  match.argc(output, choices = c('records', 'basin'))

  #the basin files are cached to avoid continued data download in the next user session

  gb_df <- .mem_files(fn=.gbdownload, path = 'globalbasins') #path where information will be cached

  #join data sets but after making the shape files valid ti properly intersect species data.

  df_join <- xdf_sf |> sf::st_join(sf::st_make_valid(gb_df), join = st_intersects, left = TRUE)

  #identify the records with no basin data, which will be flagged
  naVal <- is.na(df_join$BasinName)

  naTot <- length(naVal[which(naVal==TRUE)])

  if(naTot>0){

    if(isTRUE(discard)==TRUE){

      dffinal <- df_join[!is.na(df_join$BasinName),]

      basinnames <- unique(unlist(dffinal$BasinName))

      if(isTRUE(verbose)==TRUE) message('The ', naTot, ' records for ', species, ' are not found in any known basin for the species and they will be removed.')
    }else{

      dffinal <- df_join

      bn <- unique(unlist(dffinal$BasinName))

      basinnames <- bn[which(!is.na(bn))]

      if(isTRUE(verbose)==TRUE) message('The ', naTot, ' records for ', species, ' are not found in any known basin but they are retained if discard is FALSE.')
    }

  }else{

    dffinal <- df_join
    basinnames <- unique(unlist(dffinal$BasinName))

    if(isTRUE(verbose)==TRUE) message('All records for ', species, ' are in known freshwater fish basins.')
  }

  switch(output, records = return(dffinal), basin = return(basinnames))
}



#' @title Basin ranges for multiple species.
#'
#' @inheritParams basinranges
#' @param colsp \code{character}. Species column considered as its recorded in the occurrences.
#' @param batch \code{logical}. if \code{TRUE} all species records will be considered at once. Otherwise species-wise checks can
#' can be done. Default \code{TRUE}
#'
#' @return species occurrence records and their basins were they are found.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' salmo1 <- efidata[efidata[,'scientificName'] == c("Anguilla anguilla", "Cottus gobio"), ]
#'
#' br <- mbasinranges(occurrences = salmo1, lat = 'decimalLatitude', lon = 'decimalLongitude')
#'
#' }
mbasinranges <- function(occurrences, colsp = NULL, lat, lon, verbose=TRUE, discard=TRUE, batch = TRUE, output='records') {

  xdf <- as.data.frame(occurrences)

  #all the species records are considered at once. Other individual species are filtered out.
  if(isTRUE(batch)==TRUE){

    dfinal <- basinranges(occurrences = xdf, species = 'all species', lat =lat, lon = lon, verbose=verbose, discard= discard, output = output)

  }else{

    if(is.null(colsp)) stop("Provide the column for species names in the colsp paramter.")

    species <- unique(unlist(xdf[, colsp]))

    spl <- list()

    for (cv in seq_along(species)) {

      spnames <- species[cv]

      xsp <- xdf[xdf[,colsp] ==spnames,]

      spr <- basinranges(occurrences = xsp, species = spnames, lat =lat, lon = lon, verbose=verbose, discard= discard)

      spl[[cv]] <- spr

      dfinal <-  do.call(rbind, spl)
    }
  }
  return(dfinal)
}
