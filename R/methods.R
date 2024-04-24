
#Adjusted boxplots

#' @title Adjust the boxplots bounding fences using medcouple to flag suspicious outliers (Hubert & Vandervieren 2008).
#'
#' @param data Dataframe to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers
#' @param a Constant for adjusted boxplots
#' @param b Constant for adjusted boxplots
#' @param coef Constant for adjusted boxplots
#'
#' @importFrom robustbase adjboxStats
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  adout <- adjustboxplots(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#'
#' }
#'
#' @references Hubert M, Vandervieren E. 2008. An adjusted boxplot for skewed distributions.
#' Computational Statistics and Data Analysis 52:5186–5201.
#'
#' @author Anthony Basooma (anthony.basooma@boku.ac.at)
#'
adjustboxplots <- function(data, var, output, a=-4, b=3, coef=1.5){

  options(mc_doScale_quiet=TRUE)

  if(is(data, 'atomic') || is(data,'vector')|| is(data,'list')){

    data <- unlist(data)

    if(!is.null(var)) message(var, ' is not required for vector or list data')

    if(!is(data, 'numeric')) stop('Only numeric data accepeted')

    outdata <- robustbase::adjboxStats(var, coef = coef,a = a, b = b, do.conf = TRUE,
                                       do.out = TRUE)$out
    datIn <- which(!var%in%outdata)

    switch(output, clean=return(data[-datIn]), outlier= return(data[datIn]))

  }else if(is(data, 'data.frame') && !is.null(var)){

    var <- unlist(data[, var])

    outdata <- robustbase::adjboxStats(var, coef = coef,a = a, b = b, do.conf = TRUE,
                                       do.out = TRUE)$out
    datIn <- which(!var%in%outdata)

    switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))

  }else{

    stop('Data input is neither a dataframe neither a vector or list')

  }
}

#
#' @title Computes interquartile range to flag environmental outliers
#'
#' @param data Dataframe to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers.
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#' @importFrom stats quantile IQR
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  iqrout <- interquartile(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#'
#' }
#'
interquartile <- function(data, var, output, x=1.5){

  var <- unlist(data[, var])

  iqrcal <- stats::IQR(var)

  lowbound = quantile(var, 0.25) - x * iqrcal

  upbound = quantile(var, 0.75) + x * iqrcal

  datIn <- which(var<upbound & var>lowbound)

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#Semi interquartile range

#' @title  Computes semi-interquantile range to flag suspicious outliers
#'
#' @param data Dataframe to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  semiout <- semiIQR(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#'
#' }
#'
#'
#'

semiIQR <- function(data, var, output, x=3){

  var <- unlist(data[,var])

  semiQL=  stats::quantile(var, 0.5)- stats::quantile(var, 0.25)

  #SIQRU = Q3 − Q2
  semiQU=  stats::quantile(var, 0.75) - stats::quantile(var, 0.5)

  #[Q1 − 3SIQRL ;
  lowbound  = stats::quantile(var, 0.25) - x*semiQL

  #Q3 + 3SIQRU]
  upbound = quantile(var, 0.75) + x*semiQU

  datIn <- which(var<upbound & var>lowbound)

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#=
#Hampel filters

#' @title Flag suspicious outliers based on Hampel method (Pearson et al. 2016).
#'
#' @param data Data frame to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#' @importFrom stats median mad
#'
#' @return Data frame with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  hampout <- hampel(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#'
#' }
#'
#' @references Pearson Ronald, Neuvo Y, Astola J, Gabbouj M. 2016. The Class of Generalized Hampel Filters.
#' Pages 2546–2550 2015 23rd European Signal Processing Conference (EUSIPCO).
#'
#' @author Anthony Basooma (anthony.basooma@boku.ac.at)
#'
#'
hampel <- function(data, var=NULL, output, x=3){

  if(is(data, 'atomic') || is(data,'vector')|| is(data,'list')){

    data <- unlist(data)

    if(!is.null(var)) message(var, ' is not required for vector or list data')

    if(!is(data, 'numeric')) stop('Only numeric data accepeted')

    lowbound = stats::median(data) - x * stats::mad(data)

    upbound = stats::median(data) + x * stats::mad(data)

    datIn <- which(data<upbound & data>lowbound)

    switch(output, clean=return(data[datIn]), outlier= return(data[-datIn]))

  }else if(is(data, 'data.frame') & !is.null(var)){

    var <- unlist(data[,var])

    lowbound = stats::median(var) - x * stats::mad(var)

    upbound = stats::median(var) + x * stats::mad(var)

    datIn <- which(var<upbound & var>lowbound)

    switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))

  }else{

    stop('Data input is neither a dataframe neither a vector or list')

  }
}

#Reverse jackknifing

#' @title Ideintfies outliers using Reverse Jackknifing method based on Chapman et al., (2005).
#'
#' @param data Data frame to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers.
#' @param output Either clean: for data frame with no suspicious outliers or outlier: to return data frame with only outliers
#' @param mode Either robust, if a robust mode is used which uses median instead of mean and median absolute deviation from median
#' or mad instead of standard deviation
#'
#' @return Data frame with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  jkout <- jknife(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#'
#' }
#'
#'
jknife <- function(data, var, output, mode='soft'){

  match.arg(mode, choices = c('robust', 'soft'))

  match.arg(output, choices = c('outlier', 'clean'))


  if(missing(data)) stop('Provide the enviromental data', call. = FALSE)

  if(missing(var)) stop('Provide the variable to use for calcualting outliers', call. = FALSE)

  if(length(var)>1 || length(var)<1) stop('Provide only one parameters for detecting outleirs', call. = FALSE)

  var <- unlist(data[,var])

  sort_ls <- sort(unique(var))

  threshold <- 0.95*sqrt(length(var))*0.2

  totnum <- length(sort_ls)-1

  if(mode=='soft'){
    xbar <- base::mean(var)

    y <- c()
    xc <- c()
    for (oii in 1:totnum) {

      xi <- sort_ls [oii]

      xii <- sort_ls [oii+1]

      y[oii] <- ifelse(xi<xbar, (xii-xi)*(xbar-xi), (xii-xi)*(xii-xbar))
      xc[oii] <- xi

    }
    cvals <- y/sd(y)

    idx <- which(cvals  <threshold)

    xcv <- xc[idx]

    xvar <- var[var%in%xcv]

    datIn <- which(var%in%xvar)

  }else if(mode=='robust'){
    y <- c()
    xc <- c()
    xbar <- stats::median(var)

    for (oii in 1:totnum) {

      xi <- sort_ls[oii]
      xii <- sort_ls[oii+1]

      y[oii] <- ifelse(xi<xbar, (xii-xi)*(xbar-xi), (xii-xi)*(xii-xbar))
      xc[oii] <- xi

    }
    cvals <- y/mad(y)

    idx <- which(cvals < threshold)

    xcv <- xc[idx]

    xvar <- var[var%in%xcv]

    datIn <- which(var%in%xvar)
  }else{
    stop('No mode selected. Use either robust or soft')
  }
  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title Computes z-scores to flag environmental outliers.
#'
#' @param data Dataframe to check for outliers.
#' @param var Environmental parameter considered in flagging suspicious outliers.
#' @param output Either \strong{clean}: for data frame with no suspicious outliers or outlier: to retrun dataframe with only outliers.
#' @param type Either \strong{mild} if zscore cut off is 2.5 or \strong{extreme} if zscore is >3.
#' @param mode Either robust, if a \strong{robust} mode is used which uses median instead of
#' mean and median absolute deviation from median.
#'
#' @return Data frame with or with no outliers.
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  zout <- zscore(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#'
#' }
#'
zscore <- function(data, var, output ='outlier', type = 'mild', mode = 'soft'){

  match.arg(output, choices = c('clean','outlier'))
  match.arg(type, choices = c('extreme','mild'))
  match.arg(mode, choices = c('robust', 'soft'))

  var <- unlist(data[,var])

  if(mode=='robust'){

    zscores <- (var- stats::median(var))/stats::mad(var)

    datIn = switch(type, mild = which(zscores< 2.5 & zscores > (-2.5)),
                   extreme= which(zscores< 3 & zscores > (-3)))
  }else if (mode=='soft') {
    zscores <- (var - base::mean(var))/stats::sd(var)

    datIn = switch(type, mild = which(zscores< 2.5 & zscores > (-2.5)),
                   extreme= which(zscores< 3 & zscores > (-3)))
  }else{
    stop('Select mode of zscore whether robust or soft')
  }

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#Boxplot modifications

#' @title Log boxplot based for outlier detection.
#'
#' @param data Dataframe or vector where to check outliers.
#' @param var Variable to be used for outlier detection if \strong{data} is not a vector file.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param x The constant for creating lower and upper fences. Extreme is 3, but default is 1.5.
#'
#' @details
#' The loxplot for outlier detection \strong{Barbato et al. (2011)} modifies the
#' the interquartile range method to detect outlier but considering the sample sizes while indicating
#' the fences (lower and upper fences).
#'
#' \deqn{ lowerfence = [Q1 -1.5*IQR[1+0.1log(n/10)]}
#'
#' \deqn{upperfence = [Q3 +1.5*IQR[1+0.1log(n/10)]}
#'
#'      Where; Q1 is the lower quantile and Q3 is the upper quantile. The method consider the sample
#'      size in setting the fences, to address the weakness of the interquartile range method \emph{(Tukey, 1977)}.
#'      However. similar to IQR method for flagging outlier, log boxplot modification is affected by
#'      data skewness and which can be address using \code{seqfences}, \code{distboxplot},
#'      \code{semiinterquatile}, and \code{mixediqr} .
#'
#' @return Dataframe with our without outliers depending on the output.
#' \describe{
#' \item{clean}{Data without outliers.}
#' \item{outlier}{Data with outliers.}
#' }
#' @export
#'
#'
#' @references Barbato G, Barini EM, Genta G, Levi R. 2011. Features and performance of
#' some outlier detection methods. Journal of Applied Statistics 38:2133–2149
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  logout <- logboxplot(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#'
#' }
#'
#'
#' @author Anthony Basooma (anthony.basooma@boku.ac.at)
#'
logboxplot <- function(data, var, output, x=1.5){

  var <- unlist(data[, var])

  iqrcal <- stats::IQR(var)

  correction <- 1+0.1*log(length(var)/10)

  lowbound = quantile(var, 0.25) - x * iqrcal*correction

  upbound = quantile(var, 0.75) + x * iqrcal*correction

  datIn <- which(var<upbound & var>lowbound)

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#Walker et al., 2018
#' Title
#'
#' @param data kk
#' @param var kk
#' @param output jj
#' @param x jj
#'
#' @return kkk
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  logout <- mixediqr(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#' }
#'
mixediqr <- function(data, var, output, x=3){

  var <- unlist(data[,var])

  #semiintequantile range component
  semiQRl=  stats::quantile(var, 0.5)- stats::quantile(var, 0.25)

  #SIQRU = Q3 − Q2
  semiQRu=  stats::quantile(var, 0.75) - stats::quantile(var, 0.5)

  #Bowley’s Coefficient
  #For symmetrical data, BC is zero and the returns to IQR, which is similar to SEMIIQR
  bc <- (semiQRu-semiQRl)/(semiQRl+semiQRu)#

  numerator <- 1-bc

  denominator <- 1+bc

  frac <- numerator/denominator

  #interquantile range
  iqr <- IQR(var)


  lowbound  = stats::quantile(var, 0.25) - x*(iqr*frac)


  upbound = quantile(var, 0.75) + x*(iqr*frac)

  datIn <- which(var<upbound & var>lowbound)

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' Title
#'
#' @param data hh
#' @param var uu
#' @param output kk
#' @param x kk
#'
#' @return kk
#' @export
#'
#' @examples
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  medout <- medianrule(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#' }
#'
medianrule <- function(data, var, output, x=2.3){

  var <- unlist(data[, var])

  iqrcal <- stats::IQR(var)

  lowbound = quantile(var, 0.5) - x * iqrcal

  upbound = quantile(var, 0.5) + x * iqrcal

  datIn <- which(var<upbound & var>lowbound)

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#' Title
#'
#' @param data hh
#' @param var hh
#' @param output hh
#' @param p1 hh
#' @param p2 hhh
#'
#' @return h hh
#'
#' @export
#'
#' @examples
#'\dontrun{
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  bxout <- distboxplot(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#' }

distboxplot <- function(data, var, output, p1=0.025, p2 = 0.975){

  kndata <- specleanr::kdat

  if(is(data, 'data.frame')){
    nd <- nrow(data)
    var <- unlist(data[,var])
  } else if(is(data, 'vector') || is(data, 'atomic')){
    nd <- length(data)

    var <- data
  } else{
    stop('Only data frame or vector of values are accepted.')
  }

  if(nd>400){
    nd = 0
  }else if(nd>=100 & nd <200){
    nd = 100
  }else if (nd>=200 && nd<300){
    nd = 200
  }else if(nd>=300 & nd<400){
    nd = 300
  }else{
    nd
  }

  kn <- kndata$kn[which(as.numeric(kndata$n) == nd)]

  #semi intequantile range component
  semiQRl=  unname(stats::quantile(var, 0.5))- unname(stats::quantile(var, 0.25))

  semiQRu=  unname(stats::quantile(var, 0.75)) - unname(stats::quantile(var, 0.5))

  alphaL <- (2*semiQRl)/kn

  alphaU <- (2*semiQRu)/kn

  stdalphaL <- qnorm(p1, lower.tail = F)

  stdalphaU <- qnorm(p2, lower.tail = T)

  lowbound  = stats::quantile(var, 0.5) - stdalphaL * alphaL

  upbound = quantile(var, 0.5) + stdalphaU * alphaU

  datIn <- which(var<upbound & var>lowbound)

  if(is(data, 'data.frame')){
    switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
  }else{
    switch(output, clean=return(data[datIn]), outlier= return(data[-datIn]))
  }
}

#(Schwertman et al. 2007)
#' Title
#'
#' @param data ll
#' @param var lll
#' @param output lll
#' @param gamma lll
#' @param mode lll
#'
#' @return Dataframe or vector with or without outliers
#'
#' @importFrom stats qnorm qt
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#'
#' data("efidata")
#'
#' gbd <- check_names(data = efidata, colsp='scientificName', pct=90, merge=TRUE)
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'))
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  sqout <- seqfences(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#' }
#'
#'
#' @references
#'
#' \enumerate{
#'
#' \item Schwertman NC, de Silva R. 2007. Identifying outliers with sequential fences.
#' Computational Statistics and Data Analysis 51:3800–3810.
#' \item Schwertman NC, Owens MA, Adnan R. 2004. A simple more general boxplot method for identifying outliers.
#' Computational Statistics and Data Analysis 47:165–174.
#'
#' }
#'

seqfences <- function(data, var, output, gamma=0.95, mode='eo'){

  if(!gamma%in%c(0.75, 0.80, 0.90, 0.95, 0.975, 0.99, 0.995)) stop('Only 0.75, 0.80, 0.90, 0.95, 0.975, 0.99, 0.995 are allowed values for gamma.')

  kndata <- specleanr::kdat

  mth <- specleanr::mth

  if(is(data, 'data.frame')){
    nd <- nrow(data)

    var <- unlist(data[,var])
  } else if(is(data, 'vector') || is(data, 'atomic')){
    nd <- length(data)


    var <- data
  } else{
    stop('Only data frame or vector of values are accepted.')
  }

  if(nd>400){
    nd = 0
  }else if(nd>=100 & nd <200){
    nd = 100
  }else if (nd>=200 && nd<300){
    nd = 200
  }else if(nd>=300 & nd<400){
    nd = 300
  }else{
    nd
  }

  kn <- kndata$kn[which(as.numeric(kndata$n) == nd)]

  #get confidence coefficients
  df <- as.integer(7.6809524 + .5294156*nd - .00237*nd^2)

  confs <- c('m1', 'm2', 'm3', 'm4', 'm5', 'm6')

  lf <- c(); uf <- c(); tv <- c()
  for (si in seq_along(confs)) {
    m <- confs[si]

    mc <- mth[,m][which(mth$p== gamma)]

    tv <- qt(mc/length(var), df, lower.tail = F)
    lf[si] <- unname(quantile(var, 0.5)) - (tv/kn)*IQR(var)
    uf[si] <- unname(quantile(var, 0.5)) + (tv/kn)*IQR(var)
  }

  indx <- c()
  for (sii in seq_along(var)){
    vals <- var[sii]
    if(mode=='eo'){
      ev <- 1
    }else if(mode=='meo'){
      ev <- 2
    }else if(mode=='leo'){
      ev <- 3
    }else if (mode == 'ao'){
      ev <- 4
    }else if(mode=='wo'){
      ev <- 5
    }else{
      ev <- 6
    }
    lv <- vals < lf[ev] | vals > uf[ev]
    if(any(lv)==TRUE)  tf <- TRUE else tf <- FALSE
    indx[sii] <- tf
  }

  datIn <- which(indx==FALSE)

  if(is(data, 'data.frame')){
    switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
  }else{
    switch(output, clean=return(data[datIn]), outlier= return(data[-datIn]))
  }
}

#' @title Identify outliers using Isolation forest mode (..).
#'
#' @param data Dataframe of environmental variables extracted from where the species was recorded present or absent.
#' @param size Proportion of data to be used in training isolation forest n´model. It ranges form 0.1 (fewer data  selected ) to 1 to all data used in
#' traing isolation model.
#' @param cutoff Cut to select where the record was an outlier or not.
#' @param output Either clean: for a data set with no outliers or outlier: to output a dataframe with outliers. Default is 0.5.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#'
#' @importFrom isotree isolation.forest
#' @importFrom stats predict
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'
#'
isoforest <- function(data, size, cutoff =0.5, output, exclude){

  if(cutoff<0 |cutoff>1)stop('cutoff should range from 0 to 1')

  if(size<0 | size>1)stop('size should range from 0 to 1')

  df<- data[!colnames(data)%in%exclude]

  isomodel <- isolation.forest(data= df, sample_size = size, ntrees = 100, 1)

  isopred <- predict(isomodel,df)

  datIn <- which(isopred< cutoff)

  switch (output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#===========================================
#One class support vector machine
#=========================================

#' @title Identify outliers using One Class Support Vector Machines
#'
#' @param data Dataframe of environmental variables extracted from where the species was recorded present or absent.
#' @param kernel Either radial, linear
#' @param tune To performed a tuned version of one-class svm. High computation requirements needed.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesnot want to consider.
#' @param output Either clean: for a dataset with no outliers or outlier: to output a dataframe with outliers.
#' @param tpar A list of parameters to be varied during tunning from the normal model.
#'
#' @importFrom  e1071 svm tune.svm
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'
onesvm <- function(data, kernel='radial', tune=NULL, exclude, output,
                   tpar = list(gamma = 1^(-1:1), epislon =seq(0, 1, 0.1),
                               cost =2^2:4, nu = seq(0.05, 1, 0.1))){

  match.arg(kernel, choices = c('radial', 'linear')) #radial set to defualt, works well for high dimensional data

  df<- data[!colnames(data)%in%exclude]


  if(is.null(tune)){

    svm_model <- svm(df, type = 'one-classification', nu=0.1, scale = T, kernel = kernel)

    prediction <- predict(svm_model, df)


  }else if(tune==TRUE){

    tunemodel =  tune.svm(x=df, y=rep(T,nrow(df)), type='one-classification', kernel= kernel,
                          nu=tpar$nu, gamma = tpar$gamma)#create TRUE class to validate the model

    modnew <- tunemodel$best.model

    prediction <- predict(modnew, df)

  }else{
    message('Default untuned model executed')
  }
  datIn <- which(prediction==TRUE)

  switch (output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


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


#' @title Flags outliers using kmeans clustering method
#'
#' @param data Dataframe to check for outliers
#' @param k The number of clusters to be used for optimization. It should be greater than 1. For many species k should be be greater 10
#' to ably cater for each species search for optimal k using the different optimization methods in kmethod
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for a data set with no outliers or outlier: to output a data frame with outliers.
#' @param mode Either robust, if a robust mode is used which uses median instead of mean and median absolute deviation from median.
#' @param verbose To indicate messages and the default is FALSE.
#' @param method The method to be used for the kmeans clustering. Default is \code{silhouette}.
#' \code{Elbow method} can be used but user input is required, and therefore multiple outlier detection method
#' is not possible.
#'
#' @importFrom stats kmeans sd dist
#' @importFrom cluster silhouette
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
#' danube <- system.file('extdata/danube/basinfinal.shp', quiet=TRUE, package='spcleanr')
#'
#' danubebasin <- sf::st_read(danube)
#'
#' #Get environmental data
#'
#' #worldclim_bio <- env_download(var='bio', resolution = 10, basin = danube, folder='worlclimddata')
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', quiet=TRUE, package='spcleanr'))
#'
#' precleaned <- pre_cleaner(data = gbchecked,
#'                           raster= worldclim ,
#'                           colsp = 'speciescheck',
#'                           basin = danubebasin,
#'                           multiple = FALSE,
#'                           maxpts = 10)
#'
#' #outliers
#' kmeanoutliers <- xkmeans(data = precleaned,
#'                          k=6, exclude = c('x','y'),
#'                          kmethod ='silhouette',
#'                          output='outlier')
#'
#' #clean data
#' kmeanclean <- xkmeans(data = precleaned,
#'                          k=6, exclude = c('x','y'),
#'                          kmethod ='silhouette',
#'                          output='clean')
#' }
#'
#'
xkmeans <- function(data, k, exclude, output, mode, method=NULL, verbose=FALSE){

  match.arg(method, choices = c('silhouette','elbow'))

  match.arg(mode, choices = c('soft', 'robust'))

  df2 <- data[,!colnames(data) %in% exclude]

  df_scaled <- scale(df2)
  if(k<1 || k==1) stop('Increase k to atleast 2 to form clusters in the data.')

  wcss <- c(); kcenters <- c(); sil_val <- list()

  if(method=='elbow' && !is.null(method)){

    if(nrow(df_scaled)<10) k = 3 else k = k # to avoid errors for few data.

    for (ik in 2:k) {

      km <- stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4)

      wcss[ik-1] <- km$tot.withinss

      kcenters[ik-1] <- ik

      plot(x=kcenters, y=wcss, xlab='Number of clusters', ylab='Total within sum of squares',
           type = "b", main='Choose cluster number where the line forms an bend')

    }
    opt_k <- as.integer(readline(prompt = 'Enter the value of k with a bend: '))

    if(opt_k>length(kcenters)) stop('Optimal cluster centers should be within the
                                   number of cluster iteration number: ', k, call. = FALSE)
  }
  else if(method=='silhouette' && !is.null(method)){

    if(nrow(df_scaled)<10) k = 3 else k = k # to avoid errors for few data.

    for (ik in 1:k) {

      km <- stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4)

      sil <- cluster::silhouette(km$cluster, dist(df_scaled))

      if(length(sil)>1){

        sil_val[[ik]] <- sil[,3]

      }else{
        if(isTRUE(verbose)) message('The k centers should be greater than 1 to compute silhouette neighbour clusters')

      }
    }

    meanlst <- sapply(sil_val[-which(sapply(sil_val, is.null))], mean)

    opt_k <- which(meanlst==max(meanlst))+1 #to cater for 1-k means

    if(isTRUE(verbose))message('The optimal k value of ', opt_k, ' has been selected')

  }else{
    if(isTRUE(verbose)) message('Optimal k optimal method not selected and a default value of 3 will be used')
    opt_k = 3
  }

  km_new <- stats::kmeans(df_scaled, centers = opt_k, iter.max = 100, nstart = 4)

  eu_dist<- sqrt(rowSums(df_scaled - km_new$centers[km_new$cluster,])**2)#euclidean distances

  meaneu <- base::mean(eu_dist)
  sdeu <- stats::sd(eu_dist)

  medianeu <- stats::median(eu_dist)

  madeu <- stats::mad(eu_dist)

  zscores <- c()

  for(iik in 1:length(eu_dist)){

    zscores[iik] <- switch(mode, soft=(eu_dist[iik]-meaneu)/sdeu, robust=(eu_dist[iik]-medianeu)/madeu)
  }

  datIn <- which(zscores<2 & zscores> (-2))

  switch(output, clean= return(data[datIn,]), outlier=return(data[-datIn,]))
}



#' @title Using kmedian outlier detection methods
#'
#' @param data Input data for for checking outliers
#' @param k Number of clusters
#'
#' @return clean data set after kmedian outlier cleaning.
#'
#' @export
#'
#' @examples
#'
xkmedian <- function(data, k=3){

  set.seed(123)

  #select random starting rows
  set.seed(123)
  rstart <- sample(x=nrow(data), size = k)
  dfx <- data[rstart,]
  cl <- matrix(NA, nrow = nrow(data), ncol = nrow(dfx))
  for (i in 1:nrow(data)) {
    dr <- as.vector(t(data)[, i])
    for (ii in 1:nrow(dfx)) {
      cc <- as.vector(t(dfx[ii,]))
      cl[i,ii] <- sqrt(Reduce(abs(dr-cc)^2, f='+'))
    }
  }
  #get cluster labels
  label <- apply(cl, 1, which.min)
  #recalculate the centroids
  crcal <- list()
  for (iii in seq_along(unique(label))) {
    clust <- data[which(label==iii),]
    crcal[[iii]] <- apply(clust, 2, median)
  }
  #recompute distances
  cshf <- matrix(NA, nrow = nrow(data), ncol = length(crcal))

  for (iv in 1:nrow(data)) {
    dr2 <- as.vector(t(data)[, iv])
    for (v in seq_along(crcal)) {
      c2 <- as.vector(t(crcal[[v]]))
      cshf[iv,v] <- sqrt(Reduce(abs(dr2-c2)^2, f='+'))
    }
  }
  label2 <- apply(cshf, 1, which.min)


  if(all(label==label2)==FALSE){
    #get the centroids
    crcal2 <- list()
    for (vi in seq_along(unique(label2))) {
      clust2 <- data[which(label2==vi),]
      crcal2[[vi]] <- apply(clust2, 2, median)
    }

    #recompute distances
    cshf2 <- matrix(NA, nrow = nrow(data), ncol = length(crcal2))

    for (vii in 1:nrow(data)) {
      dr3 <- as.vector(t(data)[, vii])
      for (viii in seq_along(crcal2)) {
        c3 <- as.vector(t(crcal2[[viii]]))
        cshf2[vii,viii] <- sqrt(Reduce(abs(dr3-c3)^2, f='+'))
      }
    }
    label3 <- apply(cshf2, 1, which.min)
  }else{
    message('converged')
  }

  return(list(label, label2, label3))
}

#' Title
#'
#' @param data kk
#' @param k kk
#' @param metric kk
#' @param output kk
#' @param exclude kk
#' @param x a constant to detemrine outliers.
#'
#' @return
#' @export
#'
#' @examples
xkmedoid <- function(data, k = 2, metric = 'manhattan', output='clean', exclude, x=1.5){

  if(missing(data)) stop('Data missing')

  if(k==1) warning('All data willl put in one cluster with one medoid.')

  match.arg(metric, choices = c('euclidean', 'manhattan'))

  match.arg(output, choices = c('clean', 'outlier'))

  #run the partitioning around the medoid (pam function)

  dfd <- scale(data[,!colnames(data) %in% exclude])

  p = switch(metric, euclidean = cluster::pam(x= dfd, k= k, metric = 'euclidean'),
             manhattan = cluster::pam(x= dfd, k= k, metric = 'manhattan'))

  admp <- c()
  fd <- list()

  for(i in seq_along(unique(p$clustering))){

    df <- dfd[which(p$clustering==i),]

    for (ii in 1:nrow(df) ) {

      rd <- as.vector(t(df)[, ii])

      md <- as.vector(t(p$medoids)[, i])

      if(metric=='euclidean'){
        admp[ii] <- sqrt(Reduce(abs(rd-md)^2, f='+')) #compute the absolute distance between medoids
      }else{
        admp[ii] <- Reduce(abs(rd-md), f='+') #compute the absolute distance between medoids
      }

    }

    tr <- mean(admp)*x #compute threshold for determining outliers

    datIn <- which(admp<tr) #identify outliers

    fd[[i]] <- switch(output, clean= df[datIn,], outlier=df[-datIn,])

    out <- do.call(rbind, fd)
  }
  return(out)
}




