
#' @title Extract basin ranges from Freshwater Information Platform.
#'
#
#' @param data Data frame to extract basin species records which are either native, endemic or both.
#' @param colsp A variable in the data with species names
#' @param basin Basin names as indicated in the Freshwater Information Platform (Schmidt-Kloiber et al., 2019)
#' @param range For each basin, either indicate n to return only native, a: alien (Schmidt-Kloiber et al., 2019)
#' @param pct The percentage similarity to be considered to assign a relatively similar name from FishBase if the exact match is absent.
#'     The higher the pct values, the higher the different similar are considered to replace a species name that is cheeked from FisBase.
#' @param verbose To indicate messages and the default is FALSE
#' @param sn Either \strong{FALSE} to return accepted names from FishBase rather than synonyms.Default FALSE.
#' @param verbose TRUE to return processing messages or FALSE for no messages. Default FALSE
#'
#' @importFrom stats complete.cases aggregate
#'
#' @return Species data frame with temperature ranges from FishBase
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(jdsdata)
#'
#'
#' nativedanube <- ecoranges(x = jdsdata, colsp = 'speciesname',
#' range = 'n', verbose = T, basin = 'Danu')
#'
#' #Species introduced in Danube
#'
#' aliendf <- ecoranges(x = jdsdata, colsp = 'species', basin = 'Danu', range = 'a')
#'
#' # Species check from FIP and FB
#'
#' aliennativedf <- ecoranges(x = 'Salmo trutta', basin = 'Danu', range = c('n','a')
#'
#' }
#'
#'
#' @references Schmidt-Kloiber, A., Bremerich, V., De Wever, A. et al. The Freshwater Information Platform:
#' a global online network providing data, tools and resources for science and policy support. Hydrobiologia 838, 1–11 (2019).
#'https://doi.org/10.1007/s10750-019-03985-5
#'
#'

fip_ranges <- function(data, colsp=NULL, basin, range, pct=90, verbose=F, sn=FALSE){

  if(missing(data)) stop('Data is missing with no default')

  if(!inherits(data, 'data.frame')) stop('Only dataframes accpeted.')

  fis <- specleanr::ndata

  if(all(basin%in%c(colnames(fis)[4:40]))==FALSE)stop('Basin should be within the stated basins in FIP. Check https://www.freshwaterecology.info/fwe_search.php?og=fish for more information')

  if(all(range%in%c('n','a'))==FALSE) stop('Range should be either a, n, or both. Check https://www.freshwaterecology.info/fwe_search.php?og=fish for more information')

  if(inherits(data, 'data.frame') && is.null(colsp)) stop('Provide species column name to continue. The species names should be checked in check_names function to proceed')

  x <- as.data.frame(data)

  x2 <- x[!is.na(x[,colsp]),]

  if('speciescheck'%in%colnames(x)){
    cdata <- x2
  }else{
    cdata <- check_names(data=x2, colsp = colsp, pct= 95, verbose = F, sn=sn, merge = T)
  }
  #If basin is provided, only species that native or alien to the database will be generated

  if(!is.null(basin) && !is.null(range)){

    nspecies <- fis$Taxon[which(fis[, basin]%in%range)]

    basinspecies <- check_names(data=as.list(nspecies), pct= 95, verbose = F, sn=sn)

    #filter out only native species
    dfinal <- cdata[which(unlist(cdata$speciescheck) %in% unlist(basinspecies$speciescheck)),]
  }

  cat('Please cite this dataset as:
      Schmidt-Kloiber, A., Bremerich, V., De Wever, A. et al. The Freshwater Information Platform:
      a global online network providing data, tools and resources for science and policy support.
      Hydrobiologia 838, 1–11 (2019).https://doi.org/10.1007/s10750-019-03985-5', '\n')

  return(dfinal)
}




#' @title Collates minimum, maximum, and preferable temperatures from FishBase.
#'
#' @param x kk
#' @param colsp kk
#' @param verbose kk
#' @param pct kk
#' @param sn kk
#'
#' @return Data table for minimum, maximum and preferable species temperatures from FishBase.
#' @export
#'
#' @examples
#'
thermal_ranges <- function(x, colsp =NULL, verbose=F, pct = 90, sn =FALSE){

  if(missing(x)) stop('Data is not provided', call. = FALSE)

  if(is(x, 'data.frame') && is.null(colsp)) {

    stop('Species column names is not provided', call. = FALSE)

  } else if(is(x, 'data.frame') && !is.null(colsp)){

    if(length((colnames(x)[colnames(x)==colsp]))<1){

      stop('Species column name ', colsp, ' is  not found in the ', deparse(substitute(x)), ' data provided')

    } else{

      spls <- as.list(unique(unlist(x[, colsp])))

      dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false
    }

  }else if(is(x, 'list')){

    spls <- unlist(x)

    dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false

  }else if(is(x, 'vector') || is(x, 'atomic')) {

    dsplist <- check_names(data = x, verbose = verbose, pct = pct, sn = sn)


  }else{
    stop('No data provided for species to check and merge')
  }

  #remove repeated species names

  if(is(dsplist, 'vector')){

    unx <- dsplist
  }else{
    unx <- unique(unlist(dsplist$speciescheck))

  }
  v <- unx[!is.na(unx)]#remove NA species from the vector data

  rc <- fishbase(tables = 'ranges')

  sy <- fishbase(tables = 'synonym')

  ld <- length(unx); lv <- length(v)

  if(ld<lv) message(ld-lv, ' dropped because species names are not in FishBase')

  tempmi <- c(); tempma <- c(); temppref <- c(); species <- c()

  for (ispt in seq_along(v)) {

    sp1 <- v[ispt]

    spc <- unlist(sy$synonym)%in%sp1

    if(any(spc==TRUE)){

      codesp <-  unlist(sy$SpecCode)[which( spc==TRUE)]

      #get the species temperature ranges

      tmnext <- unlist(rc$TempMin)[which(unlist(rc$SpecCode) %in% codesp)]

      tmaxext <- unlist(rc$TempMax)[which(unlist(rc$SpecCode) %in% codesp)]

      tprefext <- unlist(rc$TempPreferred)[which(unlist(rc$SpecCode) %in% codesp)]

      #check if there is no ranges

      if(all(is.na(tmnext))) {
        if(isTRUE(verbose)==TRUE) message('No minimum temperature for ', sp1, '.')
        tempminval = NA
      }else{
        tmpmnvalues <- tmnext[!is.na(tmnext)]

        if(length(tmpmnvalues)>1) tempminval <- min(tmpmnvalues) else tempminval = tmpmnvalues
      }

      if(all(is.na(tmaxext))) {
        if(isTRUE(verbose)==TRUE) message('No maximum temperature for ', sp1, '.')
        tmaxval = NA
      }else{
        tmaxvalues <- tmaxext[!is.na(tmaxext)]
        if(length(tmaxvalues)>1) tmaxval <- max(tmaxvalues) else tmaxval = tmaxvalues
      }
      if(all(is.na(tprefext))) {
        if(isTRUE(verbose)==TRUE) message('No maximum temperature for ', sp1, '.')
        tprefval = NA
      }else{
        tprefvalues <- tprefext[!is.na(tprefext)]
        if(length(tprefvalues)>1) tprefval <- max(tprefvalues) else tprefval = tprefvalues
      }
    }else{
      if(isTRUE(verbose)==TRUE) message('No minimum temperature for ', sp1, '.')
      tprefval = NA
    }

    tempma[ispt] <- tmaxval
    tempmi[ispt] <- tempminval
    temppref[ispt] <- tprefval
    species[ispt] <- sp1

    dftemp <- data.frame(species = species, tempmin= tempmi, tempmax= tempma, tempref = temppref)
  }
  return(dftemp)
}



#' @title Determines the species ranges, whether native or alien based on Freshwater Information Platform.
#'
#' @param basin Basin under consideration.
#' @param range Whether introduced (a) or native (n)
#'
#' @return dataframe showing species ranges
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' df <- endemicity(basin ='Danu', range =c('a','n'))
#' }
#'
endemicity <- function(basin, range){

  #check species names
  x <- specleanr::ndata

  dfcheck <- check_names(data = x, colsp = 'Taxon', verbose = F, pct = 95, merge = T)

  df_filter <- dfcheck[dfcheck[,basin]%in%range,]

  dfselect <- df_filter[,c('speciescheck', basin)]

  colnames(dfselect) <- c('speciesname','range')

  cat('Please cite this dataset as:
      Schmidt-Kloiber, A., Bremerich, V., De Wever, A. et al. The Freshwater Information Platform:
      a global online network providing data, tools and resources for science and policy support.
      Hydrobiologia 838, 1–11 (2019).https://doi.org/10.1007/s10750-019-03985-5', '\n')


  return(dfselect)
}


#Species distribution checking with IUCN Red List

#' @title NatureEarth data
#'
#' @param x Absolute path
#'
#' @importFrom utils download.file unzip
#'
#' @return shapefile form Natural Earth for Administrative boundaries
#'
#'
.nearthdownload<- function(x) {#which takes an absolute path

  inp <- .abspath(x, verbose = F)

  download.file('https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip',
                destfile =file.path(inp, 'naturalearthdata.zip'), method = "libcurl", mode = 'wb')

  unzip(file.path(inp, 'naturalearthdata.zip'), exdir = file.path(inp))

  unlink(file.path(inp, 'naturalearthdata.zip'))

  shpfile <- list.files(file.path(inp), pattern = 'shp$')

  shpfinal <- sf::st_read(dsn = file.path(inp, shpfile), quiet = TRUE)

  return(shpfinal)
}


#' @title Extract species IUCN status from IUCN RedList database.
#'
#' @param x species name
#' @param key API key to access IUCN Red List data. Use rl_use_iucn() to apply for it.
#'
#' @return species IUCN category
#' @export
#'
get_iucn <- function(x, key){

  specleanr::suggested.packages(listpkgs= c('rredlist'), reason='IUCN data')

  if(is.null(key)) stop('Provide the IUCN Redlist API Key. Use the rl_use_iucn() function to get the key from IUCN ')

  y <- check_names(data = x, verbose = F, pct = 90)

  if(is.na(y)){
    stop('No species was found after harmonisation check with FishBase.')
  } else{
    iucn <- rredlist::rl_search(name= y, key = key)

    iucncat <- unlist(iucn$result$category)
  }
  return(iucncat)
}


#' @title Determine species distribution ranges based on IUCN RedList.
#'
#' @param data species data frame to check with International Union for Conservation of Nature.
#' @param species species name
#' @param lat variable for latitude in the species data frame.
#' @param lon variable for longitude in the species data frame.
#' @param key API key to access IUCN RedList data. Use rl_use_iucn() to apply for it.
#' @param verbose if TRUE processing messages or warning will be produced.
#'
#' @importFrom stats na.omit
#' @importFrom sf st_intersects st_make_valid st_join st_read
#'
#' @return species records within the known distribution ranges of the species according to IUCN RedList database.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df <- 1
#'
#' }
#'
distr_ranges <- function(data, species, lat, lon, key =NULL, verbose=T){

  specleanr::suggested.packages(c('rredlist'), reason='IUCN data')

  if(is.null(key)) stop('Provide the IUCN Redlist API Key. Use the rl_use_iucn() function to get the key from IUCN ')

  spdf <- as.data.frame(data)#get length of the data

  df <- na.omit(spdf)

  spf <- df |> st_as_sf(coords=c(lon, lat), crs = st_crs(4326))#change the column names

  naturalearthdata <- .mem_files(fn=.nearthdownload, path = 'naturalearth')

  ctrstd <-  spf |> st_join(st_make_valid(naturalearthdata), join = st_intersects, left = TRUE)

  spoutdata <- ctrstd[!is.na(ctrstd$SOVEREIGNT),]#point with no sovereign country are removed

  noctry <- nrow(ctrstd)-nrow(spoutdata)

  if(noctry>0) if(isTRUE(verbose)==TRUE) message(noctry, ' records are removed because they dont have a sovereign country.' )

  coutfinal <- unique(spoutdata$SOVEREIGNT)

  dist <- rredlist::rl_occ_country(name = species, key=key)

  #check the species name is evaluated and archived on IUCN redlist

  result <- dist$result

  if(length(result)<1){

    dfsp <- spoutdata[, c('SOVEREIGNT')]

    colnames(dfsp)[1] <- 'country'

    if(isTRUE(verbose)==TRUE) warning(species, ' is not in the IUCN RedList database and distribution ranges cannot be determined')

  }else{

    countrydist <- unique(unlist(dist$result$country))


    idx <- coutfinal%in%countrydist

    ctntdist <- coutfinal[which(idx ==FALSE)] #country not in distribution of species in IUCN

    #harmonize country names between natural earth data and ICUN to detect names are have different names in nature earth

    if(length(ctntdist)>0){

      ctry <- sapply(ctntdist, grepl, countrydist, simplify = F) #detect countries in the distribution data i.e Russian Federation vs Russia

      nmsdetected <- names(ctry)[which(sapply(ctry, function(x) any(x==TRUE))==TRUE)]

      if(length(nmsdetected)>0) full_det <- c(coutfinal[which(idx==TRUE)], nmsdetected) else full_det <- coutfinal[which(idx==TRUE)]

      nmsd<- names(ctry)[which(sapply(ctry, function(x) all(x==FALSE))==TRUE)]

      if(length(nmsd)>0){#use another string detect method to recheck if there any other country thta can be extracted

        spsearch <- stringr::str_extract(nmsd, paste(countrydist, collapse="|") )

        idxt <- which(!is.na(spsearch))

        sp1 <- spsearch[idxt]

        sp2 <- spsearch[-idxt]

        if(length(sp1)>0) full_det2 <- c(full_det, sp1) else full_det2 <- full_det

        if(length(which(!is.na(sp2)))) {

          message('Records for ', species, ' in ', sp2, ' are not in the known IUCN species range. They will be removed')

        }else{
          message('All countires are found in the distribution list from IUCN for ', species,'.')
        }
      }else{
        message('No more countries to detect.')
        full_det2 <- full_det
      }

    }else{

      full_det2 <- coutfinal[which(idx==TRUE)]
    }

    dfsp <- spoutdata[spoutdata$SOVEREIGNT%in% full_det2, c('SOVEREIGNT')]

    colnames(dfsp)[1] <- 'country'

    if(nrow(dfsp)==0){
      warning('The records are not within the distribution rangaes from IUCN for', species,'.')
      return(NULL)
    }else{
      dfsp
    }

  }
  return(dfsp)
}




#' @title Determine species distribution ranges based on IUCN RedList.
#'
#' @param data species dataframe to check with International Union for Conservation of Nature.
#' @param colsp column with species names.
#' @param lat variable for latitude in the species data frame.
#' @param lon variable for longitude in the species data frame.
#' @param key API key to access IUCN RedList data. Use rl_use_iucn() to apply for it.
#' @param mode Either distribution only, or IUCN categories for species or both
#' @param verbose TRUE to reutn messages or warning during the processing of data. Default is FALSE
#'
#' @return species records within the known distribution ranges of the species according
#'  to IUCN RedList database for multiple species.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mdst <- 12
#'
#' }
#'
mdistr_ranges <- function(data, colsp, lat, lon, key, mode, verbose=T){

  xdata <- na.omit(data)

  spp <- unlist(xdata[,colsp])

  iucnnames <- c()
  spcol <- c()
  distdata <- list()

  unx <- unique(spp)

  for (div in seq_along(unx)) {

    spnames <- unx[div]

    x <- check_names(data = spnames, verbose = verbose, pct = 90)

    dfdata <- xdata[xdata[, colsp]== x,]

    if(mode=='dist'){


      distdata[[div]] <- distr_ranges(data = dfdata,  species = x,
                                     lat = lat, lon = lon,  key = key)

      distdata[[div]][,'species'] <- x

      dx <- do.call(rbind, distdata)

    }else if(mode == 'iucn'){

      sx <- tryCatch(expr = { #if its an error then x will be NULL
        get_iucn(x=x, key = key)
      },
      error=function(e){

        message(x, ' not found in IUCN RedList database')

        return(NULL)
      })

      if(!is.null(sx)){
        iucnnames[div] <- sx
      }else{
        iucnnames[div] <- 'NE'
      }
      spcol[div] <- x

      dx <- data.frame(species = spcol, iucncategroy = iucnnames)

    }else if(mode=='both'){


      distdata[[div]] <- distr_ranges(data = dfdata, species = x,
                                     lat = lat, lon = lon,  key = key, verbose = verbose)

      distdata[[div]][,'species'] <- x

      sx <- tryCatch(expr = { #if its an error then x will be NULL
        get_iucn(x=x, key = key)
      },
      error=function(e){

        message(x, ' not found in IUCN RedList database')

        return(NULL)
      })

      if(!is.null(sx)){
        distdata[[div]][,'iucnstatus'] <- sx
      }else{
        distdata[[div]][,'iucnstatus'] <- 'NE'
      }

      dx <- do.call(rbind, distdata)

    }else{

      stop('Only dist, both, and iucn are the allowed modes')
    }
  }
  return(dx)
}

#' @title Checks for geographic ranges from FishBase
#'
#' @param x Dataframe with species records to check for geographic ranges from FishBase.
#' @param colsp Column with species names from the data set.
#' @param lat Column with latitude names and decimal latitudes are accepted.
#' @param lon Column with longitude names and decimal longitudes are accepted.
#' @param verbose TRUE and messages will show. Default FALSE:
#' @param pct The percentage similarity of species names during standardization from FishBase.
#' @param sn TRUE and synonyms will be generated and not accepted ones. Default is FALSE, where species accepted names will be produced.
#' @param output clean to return geographical ranges.
#' @param warn FALSE, not to generate warnings and TRUE for warnings. Default is FALSE:
#'
#' @return Dataframe with geographical corrected ranges for species from FishBase.
#'
#' @export
#'
#' @examples
#'
#'
geo_ranges <- function(x, colsp =NULL, lat, lon, verbose=F, pct = 90, sn =FALSE, output='clean', warn=FALSE){

  if(missing(x)) stop('Data is not provided', call. = FALSE)

  if(is(x, 'data.frame') && is.null(colsp)) {

    stop('Species column names is not provided', call. = FALSE)

  } else if(is(x, 'data.frame') && !is.null(colsp)){

    if(length((colnames(x)[colnames(x)==colsp]))<1){

      stop('Species column name ', colsp, ' is  not found in the ', deparse(substitute(x)), ' data provided')

    } else{

      spls <- as.list(unique(unlist(x[, colsp])))

      dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false
    }

  }else if(is(x, 'list')){

    spls <- unlist(x)

    dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false

  }else if(is(x, 'vector') || is(x, 'atomic')) {

    dsplist <- check_names(data = x, verbose = verbose, pct = pct, sn = sn)


  }else{
    stop('No data provided for species to check and merge')
  }

  #remove repeated species names

  if(is(dsplist, 'vector')){

    unx <- dsplist
  }else{
    unx <- unique(unlist(dsplist$speciescheck))

  }
  v <- unx[!is.na(unx)]#remove NA species from the vector data

  rc <- fishbase(tables = 'ranges')

  sy <- fishbase(tables = 'synonym')

  ld <- length(unx); lv <- length(v)

  if(ld<lv) message(ld-lv, ' dropped because species names are not in FishBase')

  minLatvalues <- c(); species <- c(); maxLatvalues<- c(); maxLonvalues <- c(); minLonvalues <- c()

  for (ispg in seq_along(v)) {

    sp1 <- v[ispg]

    spc <- unlist(sy$synonym)%in%sp1

    if(any(spc==TRUE)){

      codesp <-  unlist(sy$SpecCode)[which( spc==TRUE)]

      #latmin
      latminv<- unlist(rc$Northernmost)[which(unlist(rc$SpecCode) %in% codesp)]

      latmindir<- unlist(rc$NorthSouthN)[which(unlist(rc$SpecCode) %in% codesp)]

      #lat max
      latmaxv<- unlist(rc$Southermost)[which(unlist(rc$SpecCode) %in% codesp)]

      latmaxdir<- unlist(rc$NorthSouthS)[which(unlist(rc$SpecCode) %in% codesp)]


      if(all(is.na(latminv))) {
        if(isTRUE(verbose)==TRUE) message('No minimum latitude for ', sp1, '.')
        minLat = NA
        dirLat <- NA
      }else{
        minLatvs <- latminv[!is.na(latminv)]

        dirLatvs <- latmindir[!is.na(latmindir)]

        if(length(minLatvs)!=length(dirLatvs)) if(isTRUE(warn)==TRUE)warning('Some latitudes dont have direction which affects consversion')

        if(length(minLatvs)>1) {

          minLatp <- max(minLatvs)

          dirLatp <- dirLatvs[which(minLatvs==minLatp)]

          if(length(dirLatp)>1) latdirN <- dirLatp[1] else latdirN <- dirLatp

          if(latdirN=='N' || minLatp ==0) minLat = minLatp else minLat = as.numeric(minLatp)*-1
        } else{

          minLatp = minLatvs

          dirLatp = dirLatvs

          if(dirLatp=='N' || minLatp==0) minLat = minLatp else minLat = as.numeric(minLatp)*-1
        }
      }

      #max latitude extraction

      if(all(is.na(latmaxv))) {
        if(isTRUE(verbose)==TRUE) message('No minimum latitude for ', sp1, '.')
        maxLat = NA
        dirLat <- NA
      }else{
        maxLatvs <- latmaxv[!is.na(latmaxv)]

        dirmaxLatvs <- latmaxdir[!is.na(latmaxdir)]

        if(length(maxLatvs)!=length(dirmaxLatvs)) if(isTRUE(warn)==TRUE) warning('Some latitudes dont have direction which affects consversion')

        if(length(maxLatvs)>1) {

          maxLatp <- max(maxLatvs)

          dirmaxLatp <- dirmaxLatvs[which(maxLatvs==maxLatp )]

          if(length(dirmaxLatp)>1) latdirNmx <- dirmaxLatp[1] else latdirNmx <- dirmaxLatp

          if(latdirNmx=='N' || maxLatp==0) maxLat = maxLatp  else maxLat = as.numeric(maxLatp)*-1
        } else{

          maxLatp = maxLatvs

          dirmaxLatp = dirmaxLatvs

          if(dirmaxLatp=='N' || maxLatp == 0) maxLat = maxLatp  else maxLat = as.numeric(maxLatp)*-1
        }
      }

      lonminv<- unlist(rc$Westernmost)[which(unlist(rc$SpecCode) %in% codesp)]

      lonmindir<- unlist(rc$WestEastW)[which(unlist(rc$SpecCode) %in% codesp)]

      #extract minimum longitudes
      if(all(is.na(lonminv))) {

        if(isTRUE(verbose)==TRUE) message('No minimum longitude for ', sp1, '.')
        minLon = NA
        dirLon <- NA
      }else{
        minLonvs <- lonminv[!is.na(lonminv)]

        dirLonvs <- lonmindir[!is.na(lonmindir)]

        if(length(minLonvs)!=length(dirLonvs)) if(isTRUE(warn)==TRUE) warning('Some longitudes dont have direction which affects consversion')

        if(length(minLonvs)>1) {

          minLonp <- max(minLonvs)

          dirLonp <- dirLonvs[which(minLonvs==minLonp)]

          if(length(dirLonp)>1) londirN <- dirLonp[1] else londirN <- dirLonp

          if(londirN=='E' || minLonp== 0) minLon = minLonp else minLon = as.numeric(minLonp)*-1
        } else{

          minLonp = minLonvs

          dirLonp = dirLonvs

          if(dirLonp=='E' || minLonp==0) minLon = minLonp else minLon = as.numeric(minLonp)*-1
        }
      }

      # #max longitude extraction

      lonmaxv<- unlist(rc$Easternmost)[which(unlist(rc$SpecCode) %in% codesp)]

      lonmaxdir<- unlist(rc$WestEastE)[which(unlist(rc$SpecCode) %in% codesp)]

      if(all(is.na(lonmaxv))) {

        if(isTRUE(verbose)==TRUE) message('No minimum longitude for ', sp1, '.')

        maxLon = NA
        dirLon <- NA
      }else{
        maxLonvs <- lonmaxv[!is.na(lonmaxv)]
        dirmaxLonvs <- lonmaxdir[!is.na(lonmaxdir)]

        if(length(maxLonvs)!=length(dirmaxLonvs)) if(isTRUE(warn)==TRUE) warning('Some max longitudes dont have direction which affects consversion')

        if(length(maxLonvs)>1) {

          maxLonp <- max(maxLonvs)

          dirmaxLonp <- dirmaxLonvs[which(maxLonvs==maxLonp )]

          if(length(dirmaxLonp)>1) londirNmx <- dirmaxLonp[1] else londirNmx <- dirmaxLonp

          if(londirNmx=='E' || maxLonp ==0) maxLon = maxLonp  else maxLon = as.numeric(maxLonp)*-1
        } else{

          maxLonp = maxLonvs

          dirmaxLonp = dirmaxLonvs

          if(dirmaxLonp=='E' || maxLonp == 0) maxLon = maxLonp  else maxLon = as.numeric(maxLonp)*-1
        }
      }

    }else{
      if(isTRUE(verbose)==TRUE) message('No minimum or maximum latitude or longitudes for ', sp1, '.')
      minLat = NA
      maxLat = NA
      minLon = NA
      maxLon = NA
    }

    minLatvalues[ispg] <- maxLat
    maxLatvalues[ispg] <- minLat
    minLonvalues[ispg] <- minLon
    maxLonvalues[ispg] <- maxLon
    species[ispg] <- sp1

    dfgeo <- data.frame(species = species, minLat= minLatvalues, maxLat= maxLatvalues,
                        minLon = minLonvalues, maxLon =maxLonvalues)
  }

  #clean records outside the ranges

  dflist <- list();trackloss <- c()
  #make sure the input data is a data frame to speed up the base filter function
  data <- as.data.frame(x)

  #check species names
  data <- check_names(data = data, colsp = colsp, verbose = F, merge = T, sn = sn,pct = pct)

  speciesx <- unique(unlist(data$speciescheck))# make it unique in case of synonyms

  for(ispv in seq_along(speciesx)){

    #cross check if the species exists in the dfgeo data set
    spx <- speciesx[ispv]

    if(spx %in% unlist(dfgeo$species)==TRUE){

      #if true, use the species names to filter data

      spdata <- data[data$speciescheck==spx,]

      #check if the longitudes and longitudes are not beyond the lat/lon limits

      latitudes <- unlist(spdata[, lat])

      if(max(latitudes)>90 | min(latitudes)<(-90)) warning('Records are outside latitude range or different cordinate reference system not deicmal degreesfor ', spx, '.')


      #Extract limit from the dfgeo data set generated above

      minLatitude <- unlist(dfgeo$minLat)[which(dfgeo$species==spx)]
      maxLatitude <- unlist(dfgeo$maxLat)[which(dfgeo$species==spx)]

      if(all(is.na(c(minLatitude, maxLatitude)))) {

        if(isTRUE(warn)==TRUE) warning('No latitudinal rnages for ', spx,'.')

        dflon <- spdata

      }else if(!is.na(minLatitude) && is.na(maxLatitude)){

        indLat <- which(latitudes>minLatitude)

        dflon <- switch(output, clean = spdata[indLat,], outlier= spdata[-indLat,])

        track <- nrow(spdata)- nrow(dflon)

        if(isTRUE(verbose)==TRUE) message(track, ' reocrds were discarded becuase they are outside the minimum latitude for ', spx, '.')

      }else if(is.na(minLatitude) && !is.na(maxLatitude)){

        indLat <- which(latitudes<maxLatitude)

        dflon <- switch(output, clean = spdata[indLat,], outlier= spdata[-indLat,])

        track <- nrow(spdata)-nrow(dflon)

        if(isTRUE(verbose)==TRUE) message(track, ' reocrds were discarded becuase they are outside the maximum latitude for ', spx, '.')
      }else{
        indLat <- which(latitudes>minLatitude | latitudes<maxLatitude)

        dflon <- switch(output, clean = spdata[indLat,], outlier= spdata[-indLat,])

        track <- nrow(spdata)-nrow(dflon)

        if(isTRUE(verbose)==TRUE) message(track, ' reocrds were discarded becuase they are outside the maximum and minimum latitude for ', spx, '.')

      }
      #check for longitude from latitude output

      longitudes <- unlist(dflon[, lon])

      if(max(longitudes)>180 | min(longitudes)<(-180)) warning('Records are outside longitude range or different cordinate reference system not deicmal degrees for ', spx, '.')

      minLongitude <- unlist(dfgeo$minLon)[which(dfgeo$species==spx)]
      maxLongitude <- unlist(dfgeo$maxLon)[which(dfgeo$species==spx)]

      if(all(is.na(c(minLatitude, maxLatitude)))) {

        if(isTRUE(warn)==TRUE) warning('No longitudinal ranges for ', spx, '.')

        dfdata <- dflon

      }else if(!is.na(minLongitude) && is.na(maxLongitude)){

        indLon <- which(longitudes>minLongitude)

        dfdata <- switch(output, clean = dflon[indLon,], outlier= dflon[-indLon,])

        track <- nrow(dflon)-nrow(dfdata)

        if(isTRUE(verbose)==TRUE) message(track, ' records were discarded becuase they are outside the minimum longitude for ', spx, '.')

      }else if(is.na(minLongitude) && !is.na(maxLongitude)){

        indLon <- which(longitudes<maxLongitude)

        dfdata <- switch(output, clean = dflon[indLon,], outlier= dflon[-indLon,])

        track <- nrow(dflon)-nrow(dfdata)

        if(isTRUE(verbose)==TRUE) message(track, ' records were discarded becuase they are outside the maximum longitude for ', spx, '.')

      }else{
        indLon <- which(longitudes>minLongitude | longitudes<maxLongitude)

        dfdata <- switch(output, clean = dflon[indLon,], outlier= dflon[-indLon,])

        track <- nrow(dflon)-nrow(dfdata)

        if(isTRUE(verbose)==TRUE) message(track, ' records were discarded becuase they are outside the maximum longitude for ', spx, '.')

      }

    }else{
      warning('Species name do not exist and the original data will be returned')
      dfdata <- data[data$speciescheck==spx,]
    }

    dflist[[ispv]] <- dfdata
    dflist[[ispv]]$trackloss[ispv] <- track
    dfinal <- do.call(rbind, dflist)
  }
  return(dfinal)
}


