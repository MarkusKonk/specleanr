
#' @title Check for outliers for multiple species using temperature ranges from FishBase.
#'
#' @param data Dataframe to check for outliers.
#' @param sp If dataframe is used, then sp is the column with species names.
#' @param optimal Dataframe with standard species optimal ranges retrieved from FIshBase.
#' @param species Species column name for the standard database with optimal parameters.
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param min Minimum temperature column from the standard optimal dataframe.
#' @param max Maximum temperature column from the standard optimal dataframe.
#' @param output output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers.
#' @param sciname Only used if only one species is considered
#' @param check.names Check names for the species and default is TRUE
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #' library(terra)
#'
#' #species data from online databases
#'
#' data(efidata)
#' data(jdsdata)
#'
#' multispecies <- merge_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                     lats = 'lat',
#'                     lons = 'lon',
#'                     species = c('speciesname','scientificName')
#'
#' multspchecked <- check_names(data = multispecies, colsp='species', pct=90, merge=TRUE)
#'
#' #preclean and extract
#'
#' danube <- system.file('extdata/danube/basinfinal.shp', package='specleanr')
#'
#' danubebasin <- sf::st_read(danube, quiet=TRUE)
#'
#' #Get environmental data
#'
#' #worldclim_bio <- env_download(var='bio', resolution = 10, basin = danube, folder='worlclimddata')
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', quiet=TRUE, package='specleanr'))
#'
#' optimal_df <- ecoranges(multspchecked, colsp = 'speciescheck', range=c('n', 'a'),
#'                               basin = 'Danu')
#'
#' precleaned <- precleaner(data = gbchecked,
#'                           raster= worldclim ,
#'                           lat = 'decimalLatitude',
#'                           lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = danubebasin,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#' #only retain species with optimal ranges
#'
#' finaldata <- precleaned %>% filter(species%in%optimal_df$Species)
#'
#' #outliers
#' multioultiers <- bulkopt(data = finaldata,
#'                         var = 'bio1',
#'                         sp = 'species',#preclenaed data
#'                         optimal = optimal_df,
#'                         min = 'TempMin',
#'                         max = 'TempMax',
#'                         species = 'Species', #optimal data
#'                         output='outlier')
#'
#' multiout_clean <- bulkopt(data = finaldata,
#'                         var = 'bio1',
#'                         sp = 'species',#preclenaed data
#'                         optimal = optimal_df,
#'                         min = 'TempMin',
#'                         max = 'TempMax',
#'                         species = 'Species', #optimal data
#'                         output='clean')
#'
#' }
#'
#'
#'
bulkopt <- function(data, sp=NULL, optimal, species, var, min, max,
                    output, sciname= NULL, check.names=TRUE){

  if(missing(data)) stop('Provide the species data')

  if(missing(optimal)) stop('Ecological optimal ranges from ecoranges function missing')

  #sp is the species column in species data

  if(!is.null(sp) && length((colnames(data)[colnames(data)==sp]))<1){

    stop(sp, ' variable is  not found in the ', deparse(substitute(data)), ' data provided')

  }
  #species is column in standard dataset

  if(length((colnames(optimal)[colnames(optimal)==species]))<1){

    stop(species, ' variable is  not found in the ', deparse(substitute(optimal)))

  }

  if(length((colnames(optimal)[colnames(optimal)==min]))<1){

    stop(min, ' is  not found in the ', deparse(substitute(optimal)))

  }
  if(length((colnames(optimal)[colnames(optimal)==max]))<1){

    stop(max, ' is  not found in the ', deparse(substitute(optimal)))

  }
  #sciname is species name when its only one data set

  match.arg(output, choices = c('clean','outliers'))

  spopt <-   unlist(optimal[,species])
  mincol <-  unlist(optimal[,min])
  maxcol <-  unlist(optimal[,max])

  datIn <- list(); datOut <- list()

  if(is(data,'data.frame') && !is.null(sp)){

    dflist <- split(data, f=data[,sp])

    sppnames <- names(dflist)

    for (isp in 1:length(sppnames)) {

      spd <- sppnames[isp]

      varc <- dflist[[spd]][,var]

      if(spd %in% spopt){

        idx<- which(spopt == spd)

        min <- mincol[idx]

        max <- maxcol[idx]

        idx_in <- which(varc>min & varc<max)

      }else{

        #Guess species due spelling errors in the input dataset

        if(!is.null(check.names)){

          simd <- adist(spd, spopt)

          spdn <- spopt[(which(simd==min(simd)))]

          if(length(spdn)>1){

            message(spdn,' approximated from optimal ranges list for ', spdn)

            spdn = spdn[1]

          }else{

            spdn

            message(spdn,' approximated from optimal ranges list for ', spdn)
          }
          idx<- which(spopt == spdn)

          min <- mincol[idx]

          max <- maxcol[idx]

          idx_in <- which(varc>min & varc< max)

        }else{

          message('No species ', spd, ' found in the optimal ranges')
        }
      }

      datIn[[isp]] <- dflist[[isp]][idx_in,]
      datOut[[isp]]<- dflist[[isp]][-idx_in,]
    }
  } else if (is(data, 'data.frame') && !is.null(sciname)){

    #Data for one species if more than one provide the data must have a column for species names OR provide names lists species data sets
    if(sciname %in%spopt){

      varc <- unlist(data[,var])

      idx<- which(spopt == sciname)

      min <- mincol[idx]

      max <- maxcol[idx]

      idx_in <- which(varc>min & varc<max)

    }else{
      #Guess species due spelling errors in the input dataset
      if(!is.null(check.names)){

        simd <- adist(sciname, spopt)

        spdn <- spopt[(which(simd==min(simd)))]

        if(length(spdn)>1){
          message(spdn,' approximated from optimal ranges list for ', spdn)
          spdn = spdn[1]
        }else{
          spdn
          message(spdn,' approximated from optimal ranges list for ', spdn)
        }
        varc <- unlist(data[,var])

        idx<- which(spopt == spdn)

        min <- mincol[idx]

        max <- maxcol[idx]

        idx_in <- which(varc>min & varc<max)

      }else{

        message('No species ', isp, ' found in the optimal ranges')
      }
    }

  }else if(is(data, 'list')){ #list of species named data sets

    for (isp in names(data)) {

      varc <- data[[isp]][,var]

      if(isp %in%spopt){


        idx<- which(spopt == isp)

        min <- mincol[idx]

        max <- maxcol[idx]

        idx_in <- which(varc>min & varc<max)

      }else{
        #Guess species due spelling errors in the input dataset
        if(!is.null(check.names)){

          simd <- adist(isp, spopt)

          spdn <- spopt[(which(simd==min(simd)))]

          if(length(spdn)>1){
            message(spdn,' approximated from optimal ranges list for ', spdn)
            spdn = spdn[1]
          }else{
            spdn
            message(spdn,' approximated from optimal ranges list for ', spdn)
          }
          idx<- which(spopt == spdn)

          min <- mincol[idx]

          max <- maxcol[idx]

          idx_in <- which(varc>min & varc<max)

        }else{

          message('No species ', isp, ' found in the optimal ranges')
        }
      }

      datIn[[isp]] <- data[[isp]][idx_in,]
      datOut[[isp]]<- data[[isp]][-idx_in,]
    }

  }else{
    stop('Data input not a dataframe with a column for species or list of species datasets')
  }

  if(is(data, 'list')){
    switch (output, clean=return(datIn), outlier=return(datOut))

  }else if(is(data, 'data.frame') && !is.null(sciname)){

    switch (output, clean=return(data[idx_in,]), outlier=return(data[-idx_in,]))
  }else{
    switch (output, clean=return(do.call(rbind, datIn)), outlier=return(do.call(rbind, datOut)))
  }
}


#' @title Detect outliers using predefined optimal ranges such as annual mean temperature from WORLDCLIM
#'
#' @param data Dataframe of species records with environmental data
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for a dataset with no outliers or outlier: to output a dataframe with outliers.
#' @param min Minimum temperature column from the standard optimal dataframe.
#' @param max Maximum temperature column from the standard optimal dataframe.
#' @param ecolimit If used then a single value is used and tgether with direction can be used to flag set otpimal conditions.
#' For example, if a mean of 10 is used, then ecolimit = 10 and direction can equal, less, greater or lesseqaul than
#' the stipulated value.
#' @param direction Indicates which direction takes for the ecolimit. For example, >ecolimit
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #' library(terra)
#'
#' #species data from online databases
#'
#' gbdata <- df_retronline(data='Gymnocephalus baloni', gbiflim = 100, inatlim = 100, vertlim = 100)
#'
#' gbfinal <- merge_all(online = gbdata)
#'
#' gbchecked <- check_names(data = gbfinal, colsp='species', pct=90, merge=TRUE)
#'
#' #preclean and extract
#'
#' danube <- system.file('extdata/danube/basinfinal.shp', quiet=TRUE, package='specleanr')
#'
#' danubebasin <- sf::st_read(danube)
#'
#' #Get environmental data
#'
#' #worldclim_bio <- env_download(var='bio', resolution = 10, basin = danube, folder='worlclimddata')
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', quiet=TRUE, package='specleanr'))
#'
#' precleaned <- precleaner(data = gbchecked,
#'                           raster= worldclim ,
#'                           lat = 'decimalLatitude',
#'                           lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = danubebasin,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#' #outliers
#' spoptimal_df <- optimal(data = precleaned, var = 'bio1', output='outlier', min = 4, max =20)
#'
#' #clean
#' spoptimal_df <- optimal(data = precleaned, var = 'bio1', output='clean', min = 4, max =20)
#'
#' }
#'
#'
optimal <- function(data, var = NULL, output, min=NULL,
                    max=NULL, ecolimit = NULL, direction = NULL){

  match.arg(output, choices = c('both', 'clean', 'outlier'))

  match.arg(direction, choices = c('equal','less','greater','le','ge'))

  if(length(var)>1) stop('One variable should be considered', call. = FALSE)

  var <- unlist(data[,var])

  if(is.null(ecolimit)){

    datIn  <-  which(var>=min & var <=max)

  }else{
    datIn = switch(direction, equal=which(var==ecolimit),
                   greater = which(var>ecolimit),
                   less = which(var<ecolimit),
                   ge = which(var>=ecolimit),
                   le = which(var<=ecolimit))
  }
  switch(output, clean = return(data[datIn,]),
         outlier = return(data[-datIn,]),
         both    = return(list(data[datIn,], data[-datIn,])))
}




