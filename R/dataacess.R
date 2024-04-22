
#' @title Download species records from online database.
#'
#' @param data Data frame, list or vector of species names to retrieve records from online databases (GBIF, VertNET and iNaturalist).
#' @param colsp A variable of species names. Provided if data is a data frame, so not required for lists and vector.
#' @param basin shapefile to retrieve records within a particular area. Otherwise all records from the GBIF will be downloaded.
#' @param mode Consider all the databases, namely, GBIF, VertNet and iNaturalist, or custom for selected. Default 'all'
#' @param databases User defined combination of the databases to collate data from. Even only one can be considered.
#' @param gbiflim Limits on the records from the Global Biodiversity Information Platform
#' @param vertlim Limits on the records from VertNET.
#' @param inatlim Limits on the records from iNaturalist database.
#' @param verbose \strong{TRUE} if detailed messages should be indicated and \strong{FALSE} if download messages are not needed.
#'  Dafualt \strong{TRUE}.
#'
#'  @details
#'        Note always check the validity of the species name with standard database FishBase or World Register of Marine Species.
#'        If the records are more than 50000 in GBIF, and extent can be provide to limit the download.
#'
#' @importFrom sf st_bbox
#' @importFrom methods new
#'
#' @return Lists of species records from online databases
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' gbaloni <- check_names(data = 'gymnocephalus baloni', pct=90)
#'
#' gbdata <- getdata(data=gbaloni, gbiflim = 100, inatlim = 100, vertlim = 100)
#'
#' #Get for two species
#' sp_records <- getdata(data=c('Gymnocephalus baloni', 'Hucho hucho'),gbiflim = 100,
#'                             inatlim = 100,
#'                             vertlim = 100)
#'
#' #3. Generate species list from a dataframe
#'
#' data(efidata)
#'
#' danube <- system.file('extdata/danube/basinfinal.shp', package='specleanr')
#'
#' danubebasin <- sf::st_read(danube, quiet=TRUE)

#'
#' datachecked <- check_names(data= efidata,colsp = 'species', pct=90, merge = TRUE)#clean names
#'
#' Onlinedf <- getdata(data = datachecked, colsp='speciescheck', basin=danubebasin)
#'
#' }
#'
#'
getdata <- function(data, colsp = NULL, basin=NULL, mode='all',databases = NULL,
                          gbiflim = 1e6, vertlim = 1e3, inatlim =3e3, verbose=TRUE){

  if(missing(data)){
    stop('Provide a list of species or at least one species')
  }

  if(is(data, 'data.frame') && is.null(colsp)){

    stop('Provide the column list for species in the data to be downloaded')

  } else if(is(data, 'data.frame') && !is.null(colsp)){

    data <- unlist(data[, colsp])

  }else if(is(data, 'vector') || is(data, 'atomic')){

    data

  }else if(is(data, 'list')){

    data <- unlist(data)

  } else{
    stop('Data either list or dataframe not provided for download')
  }

  suppressMessages(suppressWarnings(suggested.packages(listpkgs=c("curl", "rvertnet", "rgbif", "rinat"), reason="to access GBIF, VertNET, and iNaturalist databased")))

  if (!curl::has_internet()) stop('No internet connection, connect and try again later')

  db_ <- c('gbif','vertnet', 'inat')

  if(!all(databases%in%db_)) stop('Only gbif, vertnet, and inat allowed')

  df_online <- list()
  dfselected <- list()

  if(mode=='all' && is.null(databases)){

    for (aii in data){

      ndata <- rgbif::occ_count(scientificName = aii)

      if(ndata==0){

        message('No records for ', aii, ' in GBIF')

        gbifx <- NULL
      } else if(ndata<=50000){

        if(gbiflim<50000){

          gbifsp <-rgbif::occ_data(scientificName = aii, limit = gbiflim)
        }else{

          gbifsp <-rgbif::occ_data(scientificName = aii, limit = ndata)
        }

        gbifx <- gbifsp$data

      }else if (ndata>50000 && is.null(basin)){

        ext2 <- unname(sf::st_bbox(basin))

        if(gbiflim<50000){

          gbifsp <-rgbif:: occ_data(scientificName = aii, limit = gbiflim,
                                    decimalLongitude = paste0(ext2[1],',' ,ext2[3]),
                                    decimalLatitude = paste0(ext2[2],',' ,ext2[4]))
        }else{
          if(isTRUE(verbose)) message('All species data in GBIF will be download')

          gbifsp <-rgbif:: occ_data(scientificName = aii, limit = gbiflim,
                                    decimalLongitude = paste0(ext2[1],',' ,ext2[3]),
                                    decimalLatitude = paste0(ext2[2],',' ,ext2[4]))
        }

        gbifx <- gbifsp$data
      }
      else{
        ext2 <- unname(sf::st_bbox(basin))
        gbifsp <-rgbif:: occ_data(scientificName = aii, limit = gbiflim,
                                  decimalLongitude = paste0(ext2[1],',' ,ext2[3]),
                                  decimalLatitude = paste0(ext2[2],',' ,ext2[4]))
        gbifx <- gbifsp$data

      }
      sptx <- scan(text = aii, what = ' ', quiet = T)
      vertx <- rvertnet::searchbyterm(genus= tolower( sptx[1]),  specificepithet = tolower(sptx[2]),
                                      limit = vertlim, messages = FALSE)
      if(is.null(vertx)){

        if(isTRUE(verbose)) message('No records for ', aii, ' in VertNet')

        vertxdf <- NULL
      }else{
        vertxdf  <- vertx$data
      }

      #handle iNaturalist data download using rinat::get_inat_obs

      inatx <- tryCatch(
        expr = {
          sx <- rinat::get_inat_obs(taxon_name= aii, maxresults = inatlim )
        },
        error= function(e){
          message('No data exist for species ', aii, ' in iNaturalist')
          return(0)
        })

      if(length(inatx) >1 ){
        inatx <-  sx
      }else{
        inatx <- NULL
      }
      df_online[[aii]] <- list(inaturalist = inatx, vertnet= vertxdf, gbif=gbifx)
    }

  }else if(mode=='custom' && !is.null(databases)){

    for (aii in data) {

      for (aiii in databases){

        if(aiii =='gbif'){

          ndata <- rgbif::occ_count(scientificName = aii)

          if(ndata==0){

            message('No records for ', aii, ' in GBIF')

            spout <- NULL

          } else if(ndata<=50000){

            gbifsp <-rgbif:: occ_data(scientificName = aii, limit = ndata)

            spout <- gbifsp$data

          }else if (ndata>50000 && is.null(basin)){

            if(isTRUE(verbose)) message('All species data in GBIF will be download')

            gbifsp <-rgbif:: occ_data(scientificName = aii, limit = gbiflim,
                                      decimalLongitude = paste0(ext2[1],',' ,ext2[3]),
                                      decimalLatitude = paste0(ext2[2],',' ,ext2[4]))
            spout <- gbifsp$data
          }else{
            ext2 <- unname(sf::st_bbox(basin))
            gbifsp <-rgbif:: occ_data(scientificName = aii, limit = gbiflim,
                                      decimalLongitude = paste0(ext2[1],',' ,ext2[3]),
                                      decimalLatitude = paste0(ext2[2],',' ,ext2[4]))
            spout <- gbifsp$data
          }

        }else if(aiii =='inat'){##

          #for (aii in data) {

          inatx <- tryCatch(
            expr = {
              sx <- rinat::get_inat_obs(taxon_name= aii, maxresults = inatlim )
            },
            error= function(e){
              message('No data exist for species ', aii, ' in iNaturalist')
              return(0)
            })

          if(length(inatx) >1 ){
            spout <-  sx
          }else{
            spout <- NULL
          }

        }else if(aiii =='vertnet'){

          #for (aii in data) {

          sptx <- scan(text = aii, what = ' ', quiet = T)

          vertx <- rvertnet::searchbyterm(genus= tolower( sptx[1]),  specificepithet = tolower(sptx[2]),
                                          limit = vertlim, messages = FALSE)
          if(is.null(vertx)){

            if(isTRUE(verbose)) message('No records for ', aii, ' in VertNet')

            vertxdf <- NULL
          }else{
            spout  <- vertx$data
          }

        }else{
          stop('Allowed databases include gbif, inat, and vernet')
        }

        dfselected[[aiii]] <-  spout
      }
      df_online[[aii]] <- dfselected
    }

  }else{
    stop('Please select the mode to custom if providing selected databases')
  }

  return(new('dataonline', output= df_online))
}



#Main function for merging all files

#' @title Extracts data from lists of files obtained from online databases.
#'
#' @param online List of online data sets, Only DataOnline class accepted.
#'
#' @return Extracts and arranges data from the online databases.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' gbdata <- getdata(data= 'Gymnocephalus baloni', gbiflim = 100, inatlim = 100,vertlim = 100)
#'
#' extractdata <- extract_online(online = gbdata)
#'
#' }
#'
#'
#'
extract_online <- function(online){

  if(!is(online, 'dataonline')) stop('Dataonline class created for online data accpeted')

  if(missing(online)) stop('Use function to process data from online databases and/or match with local data merged in match_df function')

  species <- list()

  output <- online@output

  for (bi in seq_along(output)) {

    dfdata <- output[[bi]]

    len <- sapply(dfdata, length)

    if(any(len==0)) fdata <- dfdata[len !=0] else fdata <-  dfdata

    species[[bi]] <- match_datasets(datasets = fdata,
                                    lats = c('latitude','decimallatitude'),
                                    lons = c('decimallongitude', 'longitude'),
                                    species = c('scientificname','scientific_name'),
                                    date = c('datetime', 'year','eventdate'),
                                    country = c('place_guess'))

    dfinal <- do.call(rbind, species)
  }

  return(dfinal)
}





#'
#' @title Downloads the global basin data based on Tesedesco at al., 2017
#'
#' @param x absolute path to cache global basin files
#'
#' @importFrom utils unzip download.file
#'
#' @return download files for global basin
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
#' @return
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











