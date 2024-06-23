
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
#' a global online network providing data, tools and resources for science and policy support.
#' Hydrobiologia 838, 1-11 (2019).
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
      Hydrobiologia 838, 1-11', '\n')

  return(dfinal)
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
      Hydrobiologia 838, 1-11 (2019)', '\n')


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
#' @param data Dataframe or vector to retrieve  ranges from FishBase.
#' @param colsp Column with species names from the data set.
#' @param verbose TRUE and messages will show. Default FALSE:
#' @param pct The percentage similarity of species names during standardization from FishBase.
#' @param sn TRUE and synonyms will be generated and not accepted ones. Default is FALSE, where species accepted names will be produced.
#' @param warn FALSE, not to generate warnings and TRUE for warnings. Default is FALSE:
#' @param ranges A standard database for ecological ranges from FishBase. See \href{https://www.fishbase.se/}{FishBase} for more information.
#' @param synonym A standard database for species synonym names from FishBase. See \href{https://www.fishbase.se/}{FishBase} for more information.
#'
#' @return Dataframe with geographical corrected ranges for species from FishBase.
#'
#' @export
#'
#' @examples
#'
#'
geo_ranges <- function(data, colsp =NULL, verbose=F, pct = 90,sn =FALSE, warn=FALSE,
                       synonym = fishbase(tables = 'synonym'),
                       ranges = fishbase(tables = 'ranges')){

  if(missing(data)) stop('Data is not provided', call. = FALSE)

  if(is(data, 'data.frame') && is.null(colsp)) {

    stop('Species column names is not provided', call. = FALSE)

  } else if(is(data, 'data.frame') && !is.null(colsp)){

    if(length((colnames(data)[colnames(data)==colsp]))<1){

      stop('Species column name ', colsp, ' is  not found in the ', deparse(substitute(data)), ' data provided')

    } else{

      spls <- as.list(unique(unlist(data[, colsp])))

      dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false
    }

  }else if(is(data, 'list')){

    spls <- unlist(data)

    dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false

  }else if(is(data, 'vector') || is(data, 'atomic')) {

    dsplist <- check_names(data = data, verbose = verbose, pct = pct, sn = sn)


  }else{
    stop('No data provided for species to check and merge')
  }

  #remove repeated species names

  if(is(dsplist, 'vector')){

    uqspeciesnames <- unique(dsplist)
  }else{
    uqspeciesnames <- unique(unlist(dsplist$speciescheck))

  }
  if(all(is.na(uqspeciesnames))==FALSE){

  speciesfinal <- uqspeciesnames[!is.na(uqspeciesnames)]#remove NA species from the vector data


  ranges_db <- ranges#dataset for ranges

  syndata <- synonym

  ld <- length(uqspeciesnames); lv <- length(speciesfinal)

  if(ld<lv) if(isTRUE(warn))warning(ld-lv, ' dropped because species names are not in FishBase', call. = FALSE)

  cordinates <- c("Northernmost", "Southermost","Westernmost", "Easternmost")

  cord <- sapply(speciesfinal, function(y){

    species  <- unlist(syndata$synonym)%in% y

    spcodes <-  unlist(syndata$SpecCode)[which( species==TRUE)]

    cord <- sapply(cordinates, function(x){

      coords <- x

      dir = switch(coords, Northernmost = "NorthSouthN",Southermost = "NorthSouthS",
                   Westernmost ="WestEastW", Easternmost = "WestEastE" )

      coordvalues<- unlist(ranges_db[,coords])[which(unlist(ranges_db$SpecCode) %in% spcodes)]

      if(length(coordvalues)>=1){

        cordvalues_no_na <- coordvalues[!is.na(coordvalues)]

        if(coords == "Southermost" | coords== "Westernmost"){
          cordvalues_final <- cordvalues_no_na[which.min(cordvalues_no_na)]
        }else{
          cordvalues_final <- cordvalues_no_na[which.max(cordvalues_no_na)]
        }
        direction_values<- unlist(ranges_db[,dir])[which(unlist(ranges_db$SpecCode) %in% spcodes)]

        dir_final <- direction_values[!is.na(direction_values)][which.max(cordvalues_no_na)]

        if(length(cordvalues_final) <1 & length(dir_final)<1){

          cordvalues_final = NA

        }else if(length(dir_final)<1){

          if(isTRUE(warn)) warning("The cordinates directions for ", y, " are not provided and cordinate coversation maybe erroneous.", call. = FALSE)

          cordvalues_final

        }else if(cordvalues_final==0 & is.na(dir_final)){

          cordvalues_final

        }else{

          if(dir_final =="S" | dir_final =="W" | dir_final =="s" | dir_final=='w') {
            if(cordvalues_final>0) cd = cordvalues_final*-1 else cd = cordvalues_final #some are already -ve
          } else{
            cd = cordvalues_final
          }
        }

      }else{

        if(isTRUE(warn))warning("No latitidunal ranges found in FishBase and the original data will be outputed.", call. = FALSE)
      }

    }, simplify = TRUE)
  }, simplify = TRUE)

  }else{

    warning("The species name is not found in FishBase. If the species  name is not a fish species, remove checkfishbase from optpar parameter.")
    cord = NA
  }

  return(unlist(cord))
}

