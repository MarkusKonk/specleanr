
#'
#' @title Downloads the global basin data based on Tesedesco at al., 2017
#'
#' @param x absolute path to cache global basin files
#'
#' @importFrom utils unzip download.file
#'
#' @return download files for global basin
#'
#'
.gbdownload <- function(x='globalbasins'){#this will go into the mfn function for memoise

  fp <- .abspath(dir = x, verbose = F)#saved and first operations

  xfp <- .absx(fp, var = 'basinfiles')

  download.file(url = 'https://figshare.com/ndownloader/files/8964583',
                destfile = file.path(fp, 'gbshp.zip'), mode = 'wb')

  unzip(file.path(fp, 'gbshp.zip'), exdir = file.path(xfp))

  unlink(file.path(fp, 'gbshp.zip'))

  f <- list.files(path = xfp, pattern = '.shp$', full.names = TRUE)

  s <- sf::st_read(f, quiet=T)

  cat('Please cite this global basin dataset as:
      Tedesco, P., Beauchard, O., Bigorne, R. et al. A global database on freshwater fish species occurrence in drainage basins.
      Sci Data 4, 170141 (2017). https://doi.org/10.1038/sdata.2017.141', '\n')

  return(s)
}

basinranges <- function(x, species, lat, lon, verbose=TRUE, discard=TRUE, output='records'){

  xdf <- as.data.frame(x)

  xdf_sf <- xdf |> sf::st_as_sf(coords=c(lon, lat), crs = st_crs(4326L))

  match.arg(output, choices = c('records', 'basin'))

  gb_df <- .mem_files(fn=.gbdownload, path = 'globalbasins') #path where information will be cached

  #join data sets

  df_join <- xdf_sf |> sf::st_join(sf::st_make_valid(gb_df), join = st_intersects, left = TRUE)

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



#for multiple species

mbasinranges <- function(x, colsp, lat, lon, verbose=TRUE, discard=TRUE, batch = TRUE, output='records') {

  xdf <- as.data.frame(x)

  if(isTRUE(batch)==TRUE){

    dfinal <- basinranges(x = x, species = 'all species', lat =lat, lon = lon, verbose=verbose, discard= discard, output = output)

  }else{

    species <- unique(unlist(xdf[, colsp]))

    spl <- list()

    for (cv in seq_along(species)) {

      spnames <- species[cv]

      xsp <- xdf[xdf[,colsp] ==spnames,]

      spr <- basinranges(x = xsp, species = spnames, lat =lat, lon = lon, verbose=verbose, discard= discard)

      spl[[cv]] <- spr

      dfinal <-  do.call(rbind, spl)
    }
  }
  return(dfinal)
}
