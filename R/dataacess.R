
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


#' @title Download NATURA2000 data
#' @param x path to save European data files.
#' @importFrom utils read.csv
#'
#'
.n2000download <- function(x){

  n2 <- .abspath(dir = x, verbose = F)

  download.file(url = 'https://cmshare.eea.europa.eu/s/HHGPnNsjqq5BdEa/download', destfile = file.path(n2, 'natura2000.zip'), mode = 'wb', method = 'libcurl')

  unzip(file.path(n2, 'natura2000.zip'), exdir = n2)

  unlink(file.path(n2, 'natura2000.zip'))

  #get species data
  urls <- 'https://sdi.eea.europa.eu/datashare/s/fAL7bpzZGHjiFL4/download?path=%2FTABULAR%2FCSV&files=Natura2000_end2021_rev1_SPECIES.csv'

  download.file(url = urls, destfile = file.path(n2, 'speciesnatura.csv'), mode = 'wb', method = 'libcurl')

  f <- list.files(path = file.path(n2), pattern = 'shp$', full.names = TRUE)

  s <- sf::st_read(f, quiet=T)

  sfile <- sf::st_transform(s, 4326)

  sdata <-  utils::read.csv(file = file.path(n2, 'speciesnatura.csv'))

  cat('Please visit https://www.eea.europa.eu/themes/biodiversity/natura-2000 for more information on the data.', '\n')

  return(list(sfile, sdata))
}



#' @title Check if the records are obtained within the NATURA2000 sites
#'
#' @param x Dataframe input with species records. Only dataframe accepted.
#' @param species Column name containing species names.
#' @param lat Column in the data with latitude coordinates. Only decimal degrees accepted.
#' @param lon Column in the data with longitude coordinates. Only decimal degrees accepted.
#' @param verbose TRUE, to show processing messages. Default FALSE
#' @param discard TRUE to discard records outside the NATURA2000 sites and FALSE to retained then in the final dataset. Defualt TRUE
#' @param batch TRUE, if the records have more than one species one species.
#'
#' @details
#' The fucntion is aimed at identifying records that located in the NATURA2000 protected sites mostly for invasive species.
#'     If records of invasive species are recorded in the protected, then management measures are required to avoid the loss
#'     of ecosytem diversity and species which are threatened with extinction. Also, foe species distribution modelling,
#'     records in the protected areas are vital to predicts the areas that are occupied/invaded to fdervie prioroty based
#'     conservation. To better understand the species on the NATURA2000 list, \code{check_natura} fucntion may be used.
#'
#'
#' @return Dataset with records found in NATURA2000 polygon sites.
#' @export
#'
#' @examples
#'
naturaranges <- function(x, species, lat, lon, verbose=FALSE, discard=TRUE, batch=FALSE){

  if(missing(x)) stop('Dataframe mising')

  if(is(x, 'vector') || is(x, 'atomic') || is(x, 'list')) stop('Only data.frame accepted for x input parameter.')

  if(nrow(x)<1) stop('Data provided has zero reocrds. Check to contine.')

  xdf <- as.data.frame(x)

  nat_df <- .mem_files(fn=.n2000download, path = 'natura2000') #path where information will be cached

  spsites <- nat_df[[2]]

  if(length(unique(unlist(xdf[, species])))>1 && batch==FALSE) stop('More than one species detected and batch processing is FALSE. Change to TRUE to continue or check species names.')

  if(isTRUE(batch)==FALSE) {

    sts <- spsites[spsites$SPECIESNAME %in% species,] #get site codes from natura2000 data

  }else{
    spx <- unique(unlist(xdf[, species]))

    sts <- spsites[spsites$SPECIESNAME %in% spx,]
  }

  #get the natura2000 polygons and points

  polygnat <- as.data.frame(nat_df[[1]])


  polysdata <- polygnat[polygnat$SITECODE%in%unlist(sts$SITECODE),]#polygon or points for the species

  xdf_sf <- xdf |> sf::st_as_sf(coords=c(lon, lat), crs = st_crs(4326))

  dfnat <- xdf_sf |> sf::st_join(sf::st_make_valid(polysdata |> st_as_sf()),
                                 join = st_intersects, left=TRUE)

  naVal <- is.na(dfnat$SITECODE)

  naTot <- length(naVal[which(naVal==TRUE)])

  if(naTot>0){

    if(isTRUE(discard)==TRUE){

      dffinal <- dfnat[!is.na(dfnat$SITECODE),]

      if(isTRUE(verbose)==TRUE) message('The ', naTot, ' records for ', species, ' are not found in NATURA2000 areas and they will be removed.')
    }else{

      dffinal <- dfnat
      if(isTRUE(verbose)==TRUE) message('The ', naTot, ' records for ', species, ' are not found in NATURA2000 areas but they are retained if discard is FALSE.')
    }

  }else{

    dffinal <- dfnat

    if(isTRUE(verbose)==TRUE) message('All records for ', species, ' are in known basins.')
  }

  return(dffinal)
}


#check if the species is among the Natura2000 List

#' @title Download NATURA2000 species list
#'
#' @param x Absolute path
#'
#' @return species list
#'

.n2000list <- function(x){

  n2 <- .abspath(dir = x, verbose = F)

  #get species data
  urls <- 'https://sdi.eea.europa.eu/datashare/s/fAL7bpzZGHjiFL4/download?path=%2FTABULAR%2FCSV&files=Natura2000_end2021_rev1_DIRECTIVESPECIES.csv'

  utils::download.file(url = urls, destfile = file.path(n2, 'natura2000list.csv'), mode = 'wb', method = 'libcurl')

  sdata <- utils::read.csv(file = file.path(n2, 'natura2000list.csv'))

  cat('Please visit https://www.eea.europa.eu/themes/biodiversity/natura-2000 for more information on the data.', '\n')

  return(sdata)
}


#' @title Check whether species is on NATURA2000 list
#'
#' @param x Absolute path
#' @param colsp column name with species names
#'
#' @return species list
#' @export
#'
#'
check_natura <- function(x, colsp){

  natura_list <- .mem_files(fn = .n2000list, path = 'natura2000')

  speciesnat <- unique(unlist(natura_list$SPECIESNAME))


  datalist <- check_names(data = x, colsp = colsp, pct = 90, merge = F, sn=FALSE, verbose = T) #harmonize the names

  spx <- unique(unlist(datalist$speciescheck))

  tf <- spx%in%speciesnat

  if(length(tf==TRUE)>0){
    spdata <- spx[which(tf==TRUE)]
  }else{
    message('Species does not exist in the NATURA2000 list')

    return(NULL)
  }
  return(spdata)
}











