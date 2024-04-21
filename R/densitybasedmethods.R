
#' @title Flags suspicious using the local outlier factor or Density-Based Spatial Clustering of Applications with Noise
#'
#' @param data Data frame of species records with environmental data
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for data frame with no suspicious outliers or outlier: to return dataframe with only outliers.
#' @param minPts Minimum neighbors around the records.
#' @param metric Distance-based measure to examine the distance between variables. Default \code{manhattan}.
#' @param mode Either \code{soft} if mean is used or \code{robust} if mad is used. Defualt \code{soft}.
#'
#'
#' @importFrom dbscan lof kNN glosh
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(terra)
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
#' lof_outliers <- xlof(data = precleaned,
#'                          exclude = c('x','y'),
#'                          output='outlier')
#'
#' #clean data
#' lof_clean <- xlof(data = precleaned,
#'                          exclude = c('x','y'),
#'                          output='clean')
#'
#'
#'
#' }
#'
xlof <- function(data, output, minPts, exclude = NULL, metric = 'manhattan', mode='soft'){

  match.arg(mode, choices = c('robust', 'soft'))

  match.arg(metric, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary"))

  if(!is.null(exclude)) df<- data[!colnames(data)%in%exclude] else df <- data

  dxl <- dist(x=df, method = metric)

  lscores <- dbscan::lof(dxl, minPts = minPts)

  #remove infinity values or NA values

  lclean <- lscores[!is.infinite(lscores) & !is.na(lscores)]

  lmscores <- mean(lclean)

  lsdscores <- sd(lclean)

  lmedscores <- stats::median(lclean)

  lmadscores <- stats::mad(lclean)

  lzval <- c()

  for(isl in 1:length(lscores)){

    lzval[isl] <- switch(mode, soft=(lscores[isl]-lmscores)/lsdscores,
                         robust=(lscores[isl]-lmedscores)/lmadscores)
  }

  datIn <- which(lzval<2 & lzval> (-2))

  switch (output, clean= return(data[datIn,]), outlier= return(data[-datIn,]))
}


#' Title
#'
#' @param data zzz
#' @param output zztt
#' @param exclude zzuu
#' @param metric ttt
#' @param mode uiii
#'
#' @return
#' @export
#'
#' @examples
xknn <- function(data, output, exclude = NULL, metric = 'manhattan', mode='soft'){

  match.arg(mode, choices = c('robust', 'soft'))

  match.arg(metric, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary"))

  if(!is.null(exclude)) df<- data[!colnames(data)%in%exclude] else df <- data

  dx <- dist(x=df, method = metric)

  knx <- dbscan::kNN(dx, k=3)

  kscores <- c()

  for (iknn in 1:nrow(knx$dist)) {

    kscores[iknn] <- mean(knx$dist[iknn,])
  }

  mscores <- base::mean(kscores)

  sdscores <- stats::sd(kscores)


  medscores <- stats::median(kscores)

  madscores <- stats::mad(kscores)

  zval <- c()

  for(isc in 1:length(kscores)){

    zval[isc] <- switch(mode, soft=(kscores[isc]-mscores)/sdscores, robust=(kscores[isc]-medscores)/madscores)
  }

  datIn <- which(zval<2 & zval> (-2))

  switch (output, clean= return(data[datIn,]), outlier= return(data[-datIn,]))
}


#' Title
#'
#' @param data uuu
#' @param k uuu
#' @param output uuu
#' @param exclude uzz
#' @param metric uuu
#' @param mode zzz
#'
#' @return
#' @export
#'
#' @examples
xglosh <- function(data, k, output, exclude = NULL, metric = 'manhattan', mode='soft'){

  match.arg(mode, choices = c('robust', 'soft'))

  match.arg(metric, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary"))

  if(!is.null(exclude)) df<- data[!colnames(data)%in%exclude] else df <- data

  dx <- dist(x=df, method = metric)

  gscores <-dbscan::glosh(dx, k = k)

  gmscores <- mean(gscores)

  gsdscores <- stats::sd(gscores)


  gmedscores <- stats::median(gscores)

  gmadscores <- stats::mad(gscores)

  gzval <- c()

  for(isg in 1:length(gscores)){

    gzval[isg] <- switch(mode, soft=(gscores[isg]-gmscores)/gsdscores, robust=(gscores[isg]-gmedscores)/gmadscores)
  }

  datIn <- which(gzval<2 & gzval> (-2))

  switch (output, clean= return(data[datIn,]), outlier= return(data[-datIn,]))
}



