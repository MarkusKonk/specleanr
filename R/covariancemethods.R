#Malahanobis
#' @title Flags outliers based on Mahalanobis distance matrix for all records.
#'
#' @param data Dataframe to check for outliers
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for a data set with no outliers or outlier: to output a data frame with outliers.
#' @param mode Either robust, if a robust mode is used which uses mcd() instead of mean. Default mode is soft.
#'
#' @importFrom stats mahalanobis qchisq cov
#' @importFrom usdm vifcor vifstep exclude
#' @importFrom robust covRob
#'
#' @return Either clean or oultiers data set
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
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
#' #worldclim_bio <- env_download(var='bio', resolution = 10, basin = danube, folder='worldclimdata')
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
#' mahal_outliers <- mahal(data = precleaned,
#'                          exclude = c('x','y'),
#'                          output='outlier')
#'
#' #clean data
#' mahal_clean <- mahal(data = precleaned,
#'                          exclude = c('x','y'),
#'                          output='clean')
#'
#'
#' }
#'
#'
mahal <- function(data, exclude, output, mode){

  if(missing(data)) stop('Data not provided.')

  match.arg(mode, choices = c('soft', 'robust'))

  match.arg(output, choices = c('clean', 'outlier'))

  dfna <- data[complete.cases(data),]

  df <- dfna[,!colnames(dfna) %in% exclude]

  #check for multicolinearity in the data

  df2<- usdm::exclude(df, suppressWarnings(usdm::vifstep(df)))


  if(nrow(df2)<ncol(df2)){
    stop('Inverse matrix cannot be computed if number of variables are equal or greater than observations and NULL output generated')

  }else if(det(cov(df2))==0){
    stop('The dterminant of the convariance matrix is zero and solve function failed.')

  }else{
    if(mode=='soft'){

      mdist <- stats::mahalanobis(df2 , center =  colMeans(df2), cov =  stats::cov(df2))

    }else{

      cobj =  robust::covRob(df2, estim='mcd', alpha=0.7)

      covmatrix <- cobj$cov

      centermatrix <- cobj$center

      mdist <- stats::mahalanobis(df2, center = centermatrix, cov = covmatrix)
    }

    cutoff <- qchisq(p=0.95, df = ncol(df2))

    datIn <- which(mdist<cutoff)

    switch(output, clean=return(data[datIn,]), outlier=return(data[-datIn,]))
  }
}
