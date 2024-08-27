
#' @title standardize species names.
#'
#' @param spp \code{string} species name provided by the user.
#' @param verbose \code{logical}. Default \code{TRUE} to show implementation messages.
#' @param accept \code{logical}. The user can reject or accept the suggested name by changing the default \code{TRUE} to \code{FALSE}
#' @param ... Other arguments are allowed. See \code{gnr_resolve} for details.
#'
#' @importFrom taxize gnr_resolve
#'
#' @return species name standardized under taxize package
#'
#'
#'
#' @export
#'
check_taxa_names <- function(spp, verbose = FALSE, accept =TRUE, ...){

  namesdf <- gnr_resolve(sci = spp,...)

 if(nrow(namesdf)>=1){

    nmmatch <- namesdf$matched_name

    scores <- namesdf$score

    cleannames <- sapply(X= strsplit(nmmatch, split = " "), function(x){

      xfinal <- paste0(x[1], " ", x[2])
    })

    if(mean(scores)>0.95 && length(unique(cleannames))==1){

      spfinal <- cleannames[1]

      if(isTRUE(verbose))message("The species name ", spfinal, " will be retained for ", spp, " provided.")
    }else{

      #check unique names in the match names
      uniqnames <- unique(cleannames)

      if(length(uniqnames)>1){

        unifreq <- sapply(uniqnames, function(x){

          tf <- cleannames%in%x

          uqlen <- length(tf[which(tf==TRUE)])
        })
        #maximum number of occurrences
        if(isTRUE(accept)){

          spfinal <- names(unifreq)[which.max(unifreq)]

          if(isTRUE(verbose)) message("Suggested name ", spfinal, " will used instead of ", spp)
        }else{
          spfinal <- spp

          if(isTRUE(verbose)) message("Suggested name ", spfinal, " rejected by the user and original ", spp, " used.")
        }


      }else{
        #since no unique names
        #user can reject the suggested names to continue with the
        if(isTRUE(accept)){

          spfinal <- cleannames[1] #select the first name

        }else{

          spfinal <- spp
        }

      }
    }
 }  else{

   warning("The species names ", spp, " provided does not have any match in the taxonomic databases. For the databases checked see taxize package.", call. = FALSE)
   spfinal <- spp

 }
  return(spfinal)
}


#' @title Download species records from online database.
#'
#' @param data \code{dataframe}, \code{list}, \code{vector}, \code{string}. data to retrieve records from online
#'      databases (GBIF, VertNET, and iNaturalist).
#' @param colsp \code{string}. A variable of species names. Provided if data is a data frame, so not
#'      required for lists and vector.
#' @param bbox \code{vector} or \code{sf}. Bounding box to limit the download of records within a particular area. Otherwise all
#'      records from the GBIF will be downloaded. These can be provided in two forms,
#'      either a shapefile \code{(sf)} class accepted or provide a list of
#'      \code{xmin}, \code{ymin}, \code{xmax}, and \code{ymax}.
#' @param db \code{vector}. The different databases allowed including \code{'gbif', 'vertnet', and 'inat'}.
#' @param gbiflim \code{integer}. Limits on the records from the Global Biodiversity Information Platform
#' @param vertlim \code{integer}. Limits on the records from VertNET.
#' @param inatlim \code{integer}. Limits on the records from iNaturalist database.
#' @param warn \code{logical}. To indicate if warning messages should be shown. Default \code{FALSE}.
#' @param isFish \code{logical}. To indicate if the occurrence records extracted are for fish taxa or not. This allows to clean the species names
#'      accordingly. For other taxa a different name checks is conducted. Default is \code{TRUE}.
#' @param verbose \code{logical}. \strong{TRUE} if detailed messages should be indicated and \strong{FALSE}
#'      if download messages are not needed. Default \strong{TRUE}.
#' @param ... More function for species data download can be used.
#'      See \code{rgbif::occ_data} for more information, \code{rinat::get_inat_obs}, and
#'      \code{rvertnet::searchbyterm}.
#' @inheritParams check_names
#'
#'
#' @details
#'        Note always check the validity of the species name with standard database FishBase or World Register of Marine Species.
#'        If the records are more than 50000 in GBIF, and extent can be provide to limit the download.
#'
#' @importFrom sf st_bbox
#' @importFrom methods new
#' @importFrom rvertnet searchbyterm
#' @importFrom rinat get_inat_obs
#' @importFrom rgbif occ_data occ_count
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
#'
#' }
#'
#'
getdata <- function(data, colsp = NULL, bbox=NULL, isFish= TRUE,
                     db = c("gbif", 'vertnet', 'inat'),
                     gbiflim = 1e6, vertlim = 1e3,
                     inatlim =3e3, verbose= FALSE, warn =FALSE, pct = 80, sn = F, ...){

 if(missing(data)){

    stop('Provide a list of species or at least one species.')
  }

  if(is(data, 'data.frame') && is.null(colsp)){

    stop('Provide the column list for species in the data to be downloaded.')

  } else if(is(data, 'data.frame') && !is.null(colsp)){

    data <- unique(unlist(data[, colsp]))

  }else if(is(data, 'vector') || is(data, 'atomic')){

    data <- unique(data)

  }else if(is(data, 'list')){

    data <- unique(unlist(data))

  } else{
    stop('Data either list or dataframe not provided for download.')
  }

  if (!curl::has_internet()) stop('No internet connection, connect and try again later.')

  sppdata <- sapply(data, function(spp){

    if(isTRUE(isFish)){

      checkFB<- check_names(data = spp, verbose = verbose, pct = pct, sn = sn)

      #check if a name is found in the FishBase

      if(is.na(checkFB)) {

        checksppx <- spp

        if(isTRUE(warn)) warning("The fish species name ", spp, " is not found in FishBase and the original name provider by the user will be used.",  call. = FALSE )

      } else{

        checksppx <- checkFB
      }

    }else{
      checksppx <- check_taxa_names(spp = spp, verbose = verbose)
    }

    sapply(db, FUN = function(x) {

      if(x=='gbif'){

        ndata <- occ_count(scientificName = spp)

        if(ndata==0){

          if(isTRUE(verbose)) message('No records for ', spp, ' in GBIF')

          gbifx <- NULL

        } else if(ndata<=50000){

          if(gbiflim<50000){

            gbifsp <-rgbif::occ_data(scientificName = spp, limit = gbiflim)

            if(isTRUE(verbose)) message(nrow(gbifsp$data) ,' records for ', spp,' in GBIF were downloaded based on the gbiflimit of ', gbiflim)

            gbifx <- gbifsp$data

          }else{

            gbifsp <- occ_data(scientificName = spp, limit = ndata)

            if(isTRUE(verbose)) message(nrow(gbifsp$data) ,' records for ', spp,' in GBIF were download as they were the maximum records found.')

            gbifx <- gbifsp$data
          }

        }else if (ndata>50000 && !is.null(bbox)){

          if(inherits(bbox, what = 'sf')){

            ext2 <- unname(sf::st_bbox(bbox))

          }  else{
            ext2 <- bbox

            stdbox <- c("xmin", "ymin", "xmax", "ymax")

            if(setequal(stdbox, names(ext2))==FALSE) stop("the labels provided in the bounding are not standard. Please use xmin, xmax, ymin, ymax")
          }

          if(gbiflim<=50000){

            gbifsp <- rgbif:: occ_data(scientificName = spp, limit = gbiflim,
                                       decimalLongitude = paste0(ext2[1],',' ,ext2[3]),
                                       decimalLatitude = paste0(ext2[2],',' ,ext2[4]), ...)

            if(isTRUE(verbose)) message(nrow(gbifsp$data) ,' records for ', spp,' in GBIF were downloaded based on the gbif limit of ', gbiflim)

          }else{

            gbifsp <- rgbif:: occ_data(scientificName = spp, limit = gbiflim,
                                       decimalLongitude = paste0(ext2[1],',' ,ext2[3]),
                                       decimalLatitude = paste0(ext2[2],',' ,ext2[4]), ...)

            if(isTRUE(verbose)) message('All ', nrow(gbifsp$data) ,' records for ', spp,' in GBIF were downloaded')
          }

          gbifx <- gbifsp$data
        }
        else if (ndata>50000 && is.null(bbox)){

          if(isTRUE(verbose)) message("Only ", gbiflim, " records will be downloaded.")

          gbifsp <- occ_data(scientificName = spp, limit = gbiflim,...)

          gbifx <- gbifsp$data

        }else{
          gbifx = "NO DATA FOUND"
        }

      }else if(x=='vertnet'){

        sptx <- scan(text = spp, what = ' ', quiet = T)

        vertx <- searchbyterm(genus= tolower( sptx[1]), specificepithet = tolower(sptx[2]),
                              limit = vertlim, messages = FALSE)
        if(is.null(vertx)){

          if(isTRUE(verbose)) message('No records for ', spp, ' in VertNet')

          vertxdf <- NULL

        }else{
          vertxdf  <- vertx$data

          if(isTRUE(verbose)) message(nrow(vertxdf), ' records for ', spp, ' in VertNet downloaded.')
          vertxdf
        }

        #handle iNaturalist data download using rinat::get_inat_obs
      }else if(x=='inat'){

        inatx <- tryCatch(
          expr = {
            sx <- get_inat_obs(taxon_name= spp, maxresults = inatlim)
          },
          error= function(e){

            if(isTRUE(verbose)) message('No data exist for species ', spp, ' in iNaturalist')

            return(0)
          })

        if(length(inatx) >1 ){

          inatx <-  sx

          if(isTRUE(verbose))message(nrow(inatx), ' records for ', spp, ' in iNaturalist downloaded.')

          inatx
        }else{
          inatx <- NULL
        }
      }else{
        stop('Database name not acceptable')
      }

    }, simplify = FALSE)

  }, simplify = FALSE)

  return(new('dataonline', output= sppdata))
}


#Main function for merging all files

#' @title Extracts data from lists of files obtained from online databases.
#'
#' @param online List of online data sets, Only dataonline class accepted.
#' @param verbose To show extraction messages. Default \code{FALSE}
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
extract_online <- function(online, verbose=FALSE){

  if(!is(online, 'dataonline')) stop('dataonline class created for online data accpeted')

  if(missing(online)) stop('Use function to process data from online databases and/or match with local data merged in match_df function')

  species <- list()

  output <- online@output

  for (bi in seq_along(output)) {

    dfdata <- output[[bi]]

    len <- sapply(dfdata, length)


    if(all(len==0)==FALSE) {

      if(any(len==0)) fdata <- dfdata[len !=0] else fdata <-  dfdata

      #check if the decimallatitude and decimallongitude exists in vertnet data for match_datasets to run
      if(("vertnet" %in% names(fdata))==TRUE){

        if(any(c("decimallatutide", "decimallongitude")%in%sapply(fdata, colnames)[["vertnet"]]) == FALSE){

          if(isTRUE(verbose)) message("Vertnet data for ", names(online@output)[bi], " has been removed because it lacks the coordinates columns.")

          fdata[["vertnet"]] <- NULL
        } else {
          fdata
        }

      }else{
        fdata
      }
      species[[bi]] <- match_datasets(datasets = fdata,
                                      lats = c('latitude','decimallatitude'),
                                      lons = c('decimallongitude', 'longitude'),
                                      species = c('scientificname','scientific_name'),
                                      date = c('datetime', 'year','eventdate','dates'),
                                      country = c('place_guess'))
      dfinal <- do.call(rbind, species)

    }else{

      #handles if no data is returned by all repositories
      if(isTRUE(verbose)) message("The species ", names(online@output)[bi], " has been removed because no data was returned from all repositories.")
    }

  }

  return(dfinal)
}

