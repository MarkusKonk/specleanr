
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


#to add system files
# sysfile <- 'mth'
# sysdata_filenames <- load("R/sysdata.rda")
# save(list = c(sysdata_filenames, 'mth'), file = "R/sysdata.rda")








