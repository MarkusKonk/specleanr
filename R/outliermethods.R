
#Adjusted boxplots

#' @title Adjust the boxplots bounding fences using medcouple to flag suspicious outliers (Hubert & Vandervieren 2008).
#'
#' @param data Dataframe to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to return dataframe with only outliers
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

#' @title Computes interquartile range to flag environmental outliers
#'
#' @param data Dataframe to check for outliers
#' @param var Variable considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers.
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#' @details
#' Interquartile range (IQR) uses quantiles that are resistant to outliers compared
#'      to mean and standard deviation (Seo 2006). Records were considered as mild outliers
#'      if they fell outside the lower and upper bounding fences
#'      [Q1 (lower quantile) -1.5*IQR (Interquartile range); Q3 (upper quantile) +1.5*IQR]
#'      respectively \code{(Rousseeuw & Hubert 2011)}.
#'      Extreme outliers were also considered if they
#'      fell outside \code{\[Q1-3*IQR, Q3+3*IQR\]} \code{(García-Roselló et al. 2014)}.
#'      However, using the interquartile range assumes uniform lower and
#'      upper bounding fences, which is not robust to highly skewed data
#'      (Hubert & Vandervieren 2008).
#'
#'
#' @importFrom stats quantile IQR
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
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
#' refdata <- pred_extract(data = gbd, raster= wcd , lat = 'decimalLatitude',
#'                           lon= 'decimalLongitude',
#'                           colsp = 'speciescheck',
#'                           basin = db,
#'                           multiple = FALSE,
#'                           minpts = 10)
#'
#'  iqrout <- interquartile(data = refdata[['Salmo trutta']], var = 'bio6', output='outlier')
#' }
#' @references
#'  Rousseeuw PJ, Hubert M. 2011. Robust statistics for outlier detection. Wiley Interdisciplinary Reviews
#'  Data Mining and Knowledge Discovery 1:73–79. Wiley-Blackwell.
#'
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
#' @details
#' SemiInterquantile Ranges introduced adjusts for whiskers on either
#' side to flag suspicious outliers [Q1 – 3(Q2 (median) − Q1); Q3 + 3(Q3 − Q2)] \code{((Kimber 1990))}.
#' However, SIQR introduced the same constant values for bounding fences
#' for the lower and upper quartiles \code{(Rousseeuw & Hubert 2011)}, which leads to
#' outlier swamping and masking.
#'
#'
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#'
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
#' @references
#' Kimber AC. 1990. Exploratory Data Analysis for Possibly Censored Data From Skewed Distributions.
#' Page Source: Journal of the Royal Statistical Society. Series C (Applied Statistics).
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

#' @title Flag suspicious outliers based on the Hampel filter method..
#'
#' @param data Data frame to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#' @details
#' The Hampel filter method is a robust decision-based filter that considers
#' the median and MAD. Outliers lies beyond \deqn{[x-* λ*MAD; x+ λ*MAD]} and
#' λ of 3 was considered (Pearson et al. 2016).
#'
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
#' @references
#' Pearson Ronald, Neuvo Y, Astola J, Gabbouj M. 2016. The Class of Generalized Hampel Filters.
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

#' @title Identifies outliers using Reverse Jackknifing method based on Chapman et al., (2005).
#'
#' @param data Dataframe to check for outliers
#' @param var Variable considered in flagging suspicious outliers.
#' @param output Either clean: for data frame with no suspicious outliers or outlier: to return data frame with only outliers
#' @param mode Either robust, if a robust mode is used which uses median instead of mean and median absolute deviation from median
#' or mad instead of standard deviation.
#'
#' @details
#' Reverse jackknifing was specifically developed to detect error climate profiles \code{(Chapman 1991, 1999)}.
#' The method has been applied in detecting outliers in environmental data \code{(García-Roselló et al. 2014; Robertson et al. 2016)}
#'  and incorporated in the DIVAS-GIS software \code{(Hijmans et al. 2001)}.
#'
#'
#' @return Data frame with or with no outliers.
#' @export
#'
#' @examples
#'
#' @references
#' \enumerate{
#'
#'   \item Chapman AD. 1991. Quality control and validation of environmental resource data in
#'   Data Quality and Standards. Pages 1–23. Canberra. Available from
#'   https://www.researchgate.net/publication/332537824.
#'   \item Chapman AD. 1999. Quality Control and Validation of Point-Sourced Environmental Resource Data. eds. .
#'   Chelsea,. Pages 409–418 in Lowell K, Jaton A, editors. Spatial accuracy assessment:
#'   Land information uncertainty in natural resources, 1st edition. MI: Ann Arbor Press., Chelsea.
#' }
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
#' @param data Dataframe or vector to check for outliers.
#' @param var Variable considered in flagging suspicious outliers.
#' @param output Either \strong{clean}: for data frame with no suspicious outliers or outlier: to return dataframe with only outliers.
#' @param type Either \strong{mild} if zscore cut off is 2.5 or \strong{extreme} if zscore is >3.
#' @param mode Either robust, if a \strong{robust} mode is used which uses median instead of
#' mean and median absolute deviation from median.
#'
#' @details
#' The method uses mean as an estimator of location and standard deviation for scale
#'     \code{(Rousseeuw & Hubert 2011)}, which both have zero breakdown point,
#'     and their influence function is unbounded (robustness of an estimator to outliers)
#'     \code{(Seo 2006; Rousseeuw & Hubert 2011)}. Because both parameters are not
#'     robust to outliers, it leads to outlier masking and swamping
#'     \code{(Rousseeuw & Hubert 2011)}. Records are flagged as outliers
#'     if their Z-score exceeds 2.5 \code{(Rousseeuw & Hubert 2011)}.
#'
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
#' @param var Variable to be used for outlier detection if \strong{data} is not in a vector format.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param x The constant for creating lower and upper fences. Extreme is 3, but default is 1.5.
#'
#' @details
#' The loxplot for outlier detection \strong{Barbato et al. (2011)} modifies the
#' the interquartile range method to detect outlier but considering the sample sizes while indicating
#' the fences (lower and upper fences).
#'
#' \deqn{ lowerfence = [Q1 -1.5*IQR[1+0.1 * log(n/10)]}
#'
#' \deqn{upperfence = [Q3 +1.5*IQR[1+0.1 *log(n/10)]}
#'
#'      Where; Q1 is the lower quantile and Q3 is the upper quantile. The method consider the sample
#'      size in setting the fences, to address the weakness of the interquartile range method \emph{(Tukey, 1977)}.
#'      However. similar to IQR method for flagging outlier, log boxplot modification is affected by
#'      data skewness and which can be address using \link[specleanr]{distboxplot}, \link[specleanr]{seqfences},\link[specleanr]{mixediqr} and
#'      \link[specleanr]{semiIQR}.
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


#' Mixed Interquartile range and semiInterquartile range \code{Walker et al., 2018}
#'
#' @param data Dataframe or vector where to check outliers.
#' @param var Variable to be used for outlier detection if \strong{data} is not a vector file.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param x A constant for flagging outliers \code{Walker et al., 2018)}.
#'
#' @return Either clean our outliers
#' @export
#'
#' @examples
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
#' @author Anthony Basooma \email{anthony.basooma@@boku.ac.at}
#'
#' @references
#' Walker ML, Dovoedo YH, Chakraborti S, Hilton CW. 2018. An Improved Boxplot for Univariate Data. American Statistician 72:348–353. American Statistical Association.
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
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'), quiet = TRUE)
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

#' @title Identify outliers using isolation forest model.
#'
#' @param data Dataframe of environmental variables extracted from where the species was recorded present or absent.
#' @param size Proportion of data to be used in training isolation forest n´model. It ranges form 0.1 (fewer data  selected ) to 1 to all data used in
#' training isolation model.
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
#'
#'
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
#' @param species Species of interest.
#' @param species Species column name for the standard database with optimal parameters.
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param minval Minimum temperature column from the standard optimal dataframe.
#' @param maxval Maximum temperature column from the standard optimal dataframe.
#' @param optimumSettings o
#' @param lat o
#' @param lon o
#' @param ecoparam o
#' @param direction o
#' @param pct o
#' @param checkfishbase o
#' @param mode o
#' @param warn o
#' @param output output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers.
#'
#' @return Dataframe with or with no outliers.
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
#' #data(efidata)
#' #data(jdsdata)
#'
#' #multispecies <- merge_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                     #lats = 'lat',
#'                    # lons = 'lon',
#'                     #species = c('speciesname','scientificName')
#'
#' #multspchecked <- check_names(data = multispecies, colsp='species', pct=90, merge=TRUE)
#'
#' #preclean and extract
#'
#' #danube <- system.file('extdata/danube/basinfinal.shp', package='specleanr')
#'
#' #danubebasin <- sf::st_read(danube, quiet=TRUE)
#'
#' #Get environmental data
#'
#' #worldclim_bio <- env_download(var='bio', resolution = 10, basin = danube, folder='worlclimddata')
#'
#' #worldclim <- terra::rast(system.file('extdata/worldclim.tiff', quiet=TRUE, package='specleanr'))
#'
#' #optimal_df <- ecoranges(multspchecked, colsp = 'speciescheck', range=c('n', 'a'),
#'                               basin = 'Danu')
#'
#' #precleaned <- precleaner(data = gbchecked,
#'                           #raster= worldclim ,
#'                           #lat = 'decimalLatitude',
#'                           #lon= 'decimalLongitude',
#'                           #colsp = 'speciescheck',
#'                           #basin = danubebasin,
#'                           #multiple = FALSE,
#'                           #minpts = 10)
#'
#' #only retain species with optimal ranges
#'
#' #finaldata <- precleaned %>% filter(species%in%optimal_df$Species)
#'
#' #outliers
#' #multioultiers <- ecological_ranges(data = finaldata,
#'                         #var = 'bio1',
#'                         #sp = 'species',#preclenaed data
#'                         #optimal = optimal_df,
#'                         #min = 'TempMin',
#'                         #max = 'TempMax',
#'                         #species = 'Species', #optimal data
#'                         #output='outlier')
#'
#' #multiout_clean <- ecological_ranges(data = finaldata,
#'                         #var = 'bio1',
#'                         #sp = 'species',#preclenaed data
#'                         #optimal = optimal_df,
#'                         #min = 'TempMin',
#'                         #max = 'TempMax',
#'                         #species = 'Species', #optimal data
#'                         #output='clean')
#'
#' }
#'
ecological_ranges <- function(data, var = NULL, output= "outlier", species = NULL,
                              optimumSettings = list(optdf =NULL, optspcol = NULL,
                                                     mincol = NULL, maxcol = NULL,
                                                     ecoparam = NULL, direction= NULL),
                              minval=NULL, maxval=NULL, lat = NULL, lon = NULL,
                              ecoparam=NULL, direction = NULL,
                              pct= 80,
                              checkfishbase = FALSE, mode='temp', warn=TRUE){

  match.arg(output, choices = c('clean', 'outlier'))

  match.arg(mode, choices = c('geo', 'temp'))

  match.arg(direction, choices = c('equal','less','greater','le','ge'))

  if(is.null(data))stop("Environmental data for the species should be provided.")

  if(!is.null(optimumSettings$optdf)){

    if(!is(optimumSettings$optdf, "data.frame")){

      stop("A dataframe is expected for species optimal ecological parameters.")

    } else{

      if(is.null(optimumSettings$optspcol))stop('Provide column with species names in optimal data.')

      spname_opt <- unlist(optimumSettings$optdf[, optimumSettings$optspcol])

      #check if the species in the provided data has optimal ranges

      if((species%in%spname_opt)==TRUE){ #check if the species is in the dataframe for optimal parameters.

        spp <-  species

      }else if((species%in%spname_opt)==FALSE){

        xdist <- adist(species, spname_opt)

        spsel <- spname_opt[which(xdist==min(xdist))]

        if(length(spsel)>1) spfinal <- spsel[1] else spfinal <- spsel

        #use percentage similarity
        pc = (100-(min(xdist)/nchar(spfinal))*100)
        if(pc>pct) {
          spp <- spfinal
        }
        else{
          message(species, " does not have min,  max or optimal ranges and the original data will be outputted for clean data and NA for outliers.")
          switch(output, clean= return(data) , outlier = return(NA))
        }
      }else{
        message(species, " does not have min,  max or optimal ranges and the original data will be outputted and NA for outliers.")
        switch(output, clean= return(data) , outlier = return(NA))
      }
    }

    #when the multiple species and maximum(maxcol)/minimum(mincol) are provided

    if(is.null(optimumSettings$ecoparam)){
      #get min and max values

      if(!optimumSettings$maxcol%in%colnames(optimumSettings$optdf)) stop("Parameter names for species, `maxcol`, not in the optimal data.")

      if(!optimumSettings$mincol%in%colnames(optimumSettings$optdf)) stop("Parameter names for species, `mincol`, not in the optimal data.")

      parmin = optimumSettings$optdf[, optimumSettings$mincol]

      parmax = optimumSettings$optdf[, optimumSettings$maxcol]

      minv = parmin[which(optimumSettings$optdf[, optimumSettings$optspcol] ==spp)]

      maxv = parmax[which(optimumSettings$optdf[, optimumSettings$optspcol] ==spp)]


      dx = sprange(data = data, var = var, minval = minv, maxval = maxv)

      switch(output, clean = return(data[dx,]), outlier = return(data[-dx,]))

      #1. when the ecoparam is provided in the optimal dataset, for example if only mean temperature is provided then we can use

      #2. algebra as greater, less, equal, less than (le), or greater than (ge) MUST BE INDICATED

    }else if(!is.null(optimumSettings$ecoparam)){

      #when only one parameter is provided e.g., mean temperature, spi etc

      parv = optimumSettings$optdf[, optimumSettings$ecoparam]

      dirv = optimumSettings$optdf[, optimumSettings$direction]

      pvalue = parv[which(optimumSettings$optdf[, optimumSettings$optspcol] == spp)]

      direction = dirv[which(optimumSettings$optdf[, optimumSettings$optspcol] == spp)]

      dx = sprange(data = data, var = var, ecoparam = pvalue, direction = direction)

      switch(output, clean = return(data[dx,]), outlier = return(data[-dx,]))
    }else{
      stop("Provide either min/max or ecological preference value such as mean temperate.")
    }
  }else if(isTRUE(checkfishbase)){

    if(mode=="temp"){

      ranges <- thermal_ranges(x = species)

      if(nrow(ranges)>=1){
        minv <- ranges$tempmin
        maxv <- ranges$tempmax

        dx = sprange(data = data, var = var, minval = minv, maxval = maxv)

      }else{

        message("No temperature ranges for ", species ," from FishBase and orginal data will be output fro clean data.")

        switch(output, outlier= return(NA), clean = return(data))

      }
    }else if(mode=="geo"){

      geox <- geo_ranges(data = species, warn = warn )

      if(any(is.na(geox))== FALSE){

        dx = sprange(data = data, lat = lat, lon = lon, geo = geox)
      }else{
        message("No latitudinal/longitudinal ranges for the species from FishBase and orginal data will be output fro clean data.")

        switch(output, outlier= return(NA), clean = return(data))
      }

    }else{
      message('set either georanges or tempranges')
    }


  }else{
    #if it is one species, and manual input of optimal parameters: min and max or ecoparam (when only one is provided.)

    dx <- sprange(data = data, var = var, minval = minval, maxval = maxval,
                  ecoparam = ecoparam , direction = direction)
  }

  switch(output, clean = return(data[dx,]), outlier = return(data[-dx,]))
}



#' @title Detect outliers using predefined optimal ranges such as annual mean temperature from WORLDCLIM
#'
#' @param data Dataframe of species records with environmental data
#' @param var Environmental parameter considered in flagging suspicious outliers. The parameter directly
#'        influences the species distribution such as such as annual mean temperature,
#'        minimum temperature of the coldest month, maximum temperature of the
#'        warmest month, and spring precipitation (IUCN Standards and Petitions Committee 2022).
#' @param minval Minimum temperature column from the standard optimal dataframe. This can be obtained
#'      from literature or standard databases such as FishBase (www.fishbase.org),
#'      the Freshwater Information platform (http://www.freshwaterplatform.eu/),
#'      and the IUCN red list (https://www.iucnredlist.org/).
#' @param maxval Maximum temperature column from the standard optimal dataframe.
#' @param ecoparam If used then a single value is used and tgether with direction can be used to flag set otpimal conditions.
#'      For example, if a mean of 10 is used, then ecoparam = 10 and direction can equal, less, greater or lesseqaul than
#'      the stipulated value.
#' @param direction Indicates which direction takes for the ecoparam. For example, less than ecoparam.
#' @param lat,lon string columns with latitudes and longitudes.
#' @param geo logical, to aid in switching between temperature parameter \code{temp} latitudinal/longitudinal ranges.
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
#' gbdata <- getdata(data='Gymnocephalus baloni', gbiflim = 100, inatlim = 100, vertlim = 100)
#'
#' gbfinal <- extract_online(online = gbdata)
#'
#' gbchecked <- check_names(data = gbfinal, colsp='species', pct=90, merge=TRUE)
#'
#' #preclean and extract
#'
#' danube <- system.file('extdata/danube/basinfinal.shp', package='specleanr')
#'
#' danubebasin <- sf::st_read(danube)
#'
#' #Get environmental data
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'), quiet=TRUE)
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
#' @references
#' García-Roselló E, Guisande C, Heine J, Pelayo-Villamil P, Manjarrés-Hernández A, González Vilas L, González-Dacosta J, Vaamonde A, Granado-Lorencio C. 2014. Using modestr to download, import and clean species distribution records. Methods in Ecology and Evolution 5:708–713. British Ecological Society.
#' IUCN Standards and Petitions Committee. 2022. THE IUCN RED LIST OF THREATENED SPECIESTM Guidelines for Using the IUCN Red List Categories and Criteria Prepared by the Standards and Petitions Committee of the IUCN Species Survival Commission. Available from https://www.iucnredlist.org/documents/RedListGuidelines.pdf.


sprange <- function(data, var = NULL, minval = NULL, maxval = NULL, ecoparam = NULL, lat =NULL, lon=NULL,
                    direction = NULL, geo=NULL){

  if(!is.null(geo)){
    lat = unlist(data[, lat])
    lon = unlist(data[, lon])
  }else{
    if(is.null(var))stop("Provide the column for variable of concern in ´var´ parameter")
    var = unlist(data[, var])
  }
  #converting the latitudinal ranges to a vector
  geocodes <- c(geo)

  #
  if(is.null(ecoparam)){

    if(is.null(geo)){

      if(any(sapply(list(maxval, minval), is.null))==TRUE) stop("Can not proceeed without both maxval and minval ranges.'\n' If one is available, use ecoparam parameter and provide direction.")

      datIn  <-  which(var<=maxval & var >=minval)


    }else{
      if(any(sapply(list(lat, lon), is.null))==TRUE) stop("Can not proceeed without both latitudes and longitudes columns.'\n' If one is available, use ecoparam parameter and provide direction.")

      gcheck <- mapply(function(x, y){
        tf <- c(y<=geocodes[1], y>=geocodes[2], x>=geocodes[3], x<=geocodes[4])

        if(any(is.na(tf))){
          datIn <- NA
        } else if(all(tf==TRUE)){
          datIn <-  x
        } else{
          datIn <- NA
        }
        return(datIn)
      }, x=lon, y= lat)

      datIn <- which(lon %in%gcheck[!is.na(gcheck)])
    }
  }else{
    if(is.null(direction)) stop("For ecoparam indicate if the value is equal, less, or greater.")

    datIn = switch(direction,
                   equal=which(var==ecoparam),
                   greater = which(var>ecoparam),
                   less = which(var<ecoparam),
                   ge = which(var>=ecoparam),
                   le = which(var<=ecoparam))
  }
  return(datIn)
}


#' @title Flags outliers based on Mahalanobis distance matrix for all records.
#'
#' @param data Dataframe to check for outliers
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for a data set with no outliers or outlier: to output a data frame with outliers.
#' @param mode Either robust, if a robust mode is used which uses mcd() instead of mean. Default mode is soft.
#' @param pdf Chisqure probability distribution used for flagging outliers \code{Leys et al. 2018}.
#'
#' @importFrom stats mahalanobis qchisq cov
#' @importFrom usdm vifcor vifstep exclude
#' @importFrom robust covRob
#'
#' @return Either clean or outliers dataset
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
#' gbdata <- getdata(data='Gymnocephalus baloni', gbiflim = 100, inatlim = 100, vertlim = 100)
#'
#' gbfinal <- extract_online(online = gbdata)
#'
#' gbchecked <- check_names(data = gbfinal, colsp='species', pct=90, merge=TRUE)
#'
#' #preclean and extract
#'
#' danube <- system.file('extdata/danube/basinfinal.shp', package='specleanr')
#'
#' danubebasin <- sf::st_read(danube)
#'
#' #Get environmental data
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'), quiet=TRUE)
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
#' @references
#' Leys C, Klein O, Dominicy Y, Ley C. 2018. Detecting multivariate outliers:
#' Use a robust variant of the Mahalanobis distance. Journal of Experimental
#' Social Psychology 74:150–156. Academic Press Inc.
#'
mahal <- function(data, exclude, output, mode, pdf = 0.95){

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

    cutoff <- qchisq(p=pdf, df = ncol(df2))

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
#' danube <- system.file('extdata/danube/basinfinal.shp',  package='specleanr')
#'
#' danubebasin <- sf::st_read(danube)
#'
#' #Get environmental data
#'
#' #worldclim_bio <- env_download(var='bio', resolution = 10, basin = danube, folder='worlclimddata')
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'), quiet=TRUE)
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

    #lower the k value for low samples but that can not looped through the different values

     for (ik in 1:k) {

      #Check if an error is raised during the loop but when at low k values like 3, the iteration was successfully, choose a default automatically

      kx <- tryCatch(expr = stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4), error = function(e) e)

      if(inherits(kx, 'error') && nrow(df_scaled)>=10) {

        km = stats::kmeans(df_scaled, centers = 3, iter.max = 100, nstart = 4)

      }else{
        km = stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4)
      }

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
#' @return clean or outlier data set after outlier detection.
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
#' @param data the environmental data where outliers are examined from.
#' @param k the number of cluster centers to form cluster around it. Since kmedoid using the raw valaues from thr dataset, it is not insesetive to outliers.
#' @param metric Different distance based matrics including the Euclidean and Mahattan are implemented.
#' @param output Either clean or outliers dataset. Defualt \code{outlier} to output outliers dataset.
#' @param exclude columns to remove in implementtimg kmedoid algorithms, for example, the cordinates. This becuase kmediod is a multivariate algorithm which use all the data.
#' @param x a constant to detemrine outliers.
#'
#' @return clean or outlier data set after outlier detection
#'
#' @export
#'
#' @examples
#'
xkmedoid <- function(data, k = 2, metric = 'manhattan', output='outlier', exclude, x=1.5){

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




