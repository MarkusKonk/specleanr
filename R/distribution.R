



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
  #ndata is standard database from the freshwater information platform with fish species traits
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
      error=function(){

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
      error=function(){

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

