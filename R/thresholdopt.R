
#' @title Determine the threshold using Locally estimated or weighted Scatterplot Smoothing.
#'
#' @inheritParams clean_data
#' @param plot \code{logical}. to show plot of loess fitted function with minima and maxima (optimal threshold and clean data).
#' @param colors \code{vector}. Colors for both the true data and the loess fitted data lines.
#' @param tuneLoess \code{vector}. Values to tune the span of the loess modeling. The best is obtained when RMSE
#'       is minimum.
#' @importFrom stats loess
#' @return Returns \code{numeric} of most suitable threshold at maxima of the loess smoothing.
#'
#' @export
#'
#'
thresh_search <- function(data, outliers,  sp = NULL, plot=FALSE, var_col = NULL, warn=FALSE,
                          verbose=FALSE,
                          colors = c('darkblue', 'orange'),
                          tuneLoess = seq(0.75, 1, 0.1)){

  #Extract the variable used from the datacleaner class for outliers.


  var <- outliers@varused
  if(length(var)>1) var <- sp else var

  seqlen <- seq(0.1, 0.9, 0.1)

  dataretained <- c()

  thresholdvalues <- c()

  #extract data at the different thresholds

  #the output data reaches a constant at higher thresholds are absolute outliers are exhausted

  for (ii in 1: length(seqlen)) {

    #try and catch threshold which returns no outliers and get return the original species data.

    absoutliersvec <- tryCatch(expr = ocindex(x= outliers, sp = sp, absolute = TRUE, threshold = seqlen[ii], warn = warn),
                              error = function(e) return(NULL))

    if(!is.null(absoutliersvec)) {

      varc <- unlist(data[, var])

      indx <- which(!varc %in% absoutliersvec)

      datIn <- data[indx,]

      dataretained[ii] <- nrow(datIn)

      }else {
        dataretained[ii] <- nrow(data)
      }

    thresholdvalues[ii] <- seqlen[ii]

    absthreshold_df <- data.frame(thresholdvalues = thresholdvalues, dataretained =dataretained)
  }

  #fit a local weighted running smoother
  #optimize span-- the smoothing value
  #tuneLoess <- seq(0.75, 1, 0.1)
  rmse <- c()
  spans <- c()
  for (ip in seq_along(tuneLoess)) {

    lwrs <-tryCatch(

      expr = loess(dataretained~thresholdvalues, data= absthreshold_df, span = tuneLoess[ip]),

      error= function(e){

        return(NULL)
      },
      warning=function(w){
        return(NULL)
      })
    #skip the NULL values from the vector data
    if(!is.null(lwrs)){

      rmse[ip] <- sqrt(mean((predict(lwrs)-unlist(absthreshold_df$dataretained))^2))
      spans[ip] <- tuneLoess[ip]
    }else{
      next
    }
  }
  bestspan <- spans[which.min(rmse)]

  lwrsout <- loess(dataretained~thresholdvalues, data= absthreshold_df, span = bestspan)#optimise low rmse

  #Get the maxima and minima ##thresholds when clean extracted data has reached highest.
  #Flat curve or where the slope of the line is the highest.: identify the first derivative
  #For the fitted data
  #completely, the f(x) = 0
  #maxima: a maximum where any shift the gradient decreases
  #https://stackoverflow.com/questions/12183137/calculate-min-max-slope-of-loess-fitted-curve-with-r

  firstdirev <- diff(lwrsout$fitted)

  maximaval <- lwrsout$fitted[which.min(firstdirev)]


  minimalval <- lwrsout$fitted[which.max(firstdirev)]

  optimalmax <- absthreshold_df$thresholdvalues[which.min(firstdirev)]

  optimalmin<- absthreshold_df$thresholdvalues[which.max(firstdirev)]



  if(isTRUE(plot)){
    #get values to dodge the curves
    rangeval <- maximaval-minimalval

    if(rangeval<=10) {
      dodge = 1
    }else if(rangeval>=10 && rangeval<50){
      dodge = 2.5
    }else if(rangeval>=50 && rangeval<100){
      dodge = 10
    }else if(rangeval>=100 && rangeval<200){
      dodge = 7.5
    }else if(rangeval>=200 && rangeval<500){
      dodge = 10
    }else if(rangeval>=500 && rangeval<1000){
      dodge = 25
    }else if(rangeval>=1000 && rangeval<2000){
      dodge = 50
    }else{
      dodge = 100
    }
    #install suggested packages if not yet inatalled on user computer
    if(!requireNamespace("ggplot2", quietly = TRUE))stop('Please install ggplot2 to continue.')

    thresholdvalues <- NULL; dataretained <- NULL

    gplot <- ggplot2::ggplot(data = absthreshold_df, ggplot2::aes(x = thresholdvalues, y = dataretained))+

      ggplot2::geom_point(shape=20, size=3)+

      ggplot2::geom_line(ggplot2::aes(color = "Actual values"),linetype='dotted', linewidth = 1.2)+

      ggplot2::theme_classic()+

      #stat_smooth(method = 'lm', se=FALSE, formula=y~poly(x,2))+
      ggplot2::geom_smooth(method = 'loess', se=FALSE, ggplot2::aes(color='loess values'), formula = 'y ~ x')+

      ggplot2::scale_color_manual(values = colors)+

      ggplot2::geom_vline(xintercept = optimalmin, linetype ='dotted')+

      ggplot2::geom_vline(xintercept = optimalmax, color='red', linewidth=1.2)+

      ggplot2::geom_hline(yintercept = maximaval, linetype ='dotted', color='red')+

      ggplot2::annotate("text", x = optimalmax+0.07, y = maximaval-dodge, label = paste0("Maxima: ", optimalmax)) +

      ggplot2::annotate("text", x = optimalmin+0.07, y = minimalval-dodge, label = paste0("Minima: ", optimalmin))+

      ggplot2::scale_x_continuous(breaks=seq(0,1,by=0.1))+

      ggplot2::theme(legend.position = 'bottom',
                     legend.title = ggplot2::element_blank())+

      ggplot2::labs(y="Data retained after outlier removal", x="Thresholds")

    print(gplot)
  }

  return(c(minima = optimalmin, maxima = optimalmax))
}


#' @title Optimize threshold for clean data extraction.
#'
#' @inheritParams thresh_search
#' @param refdata \code{dataframe}. Species data frame from precleaned analysis.
#' @return Either a \code{list} or \code{dataframe} of cleaned records for multiple species.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(jdsdata)
#' data(efidata)
#' matchdata <- match_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                             lats = 'lat',
#'                             lons = 'lon',
#'                             species = c('speciesname','scientificName'),
#'                             country= c('JDS4_site_ID'),
#'                             date=c('sampling_date', 'Date'))
#'
#' datacheck <- check_names(matchdata, colsp= 'species', pct = 90, merge =TRUE)
#'
#'
#' db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)
#'
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' rdata <- pred_extract(data = datacheck,
#'                       raster= worldclim ,
#'                       lat = 'decimalLatitude',
#'                       lon= 'decimalLongitude',
#'                       colsp = 'speciescheck',
#'                       bbox = db,
#'                       multiple = TRUE,
#'                       minpts = 10,
#'                       list=TRUE,
#'                       merge=F)
#'
#'
#' out_df <- multidetect(data = rdata, multiple = TRUE,
#'                       var = 'bio6',
#'                       output = 'outlier',
#'                       exclude = c('x','y'),
#'                       methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))
#'
#' #extracting optimal threshold for each species
#'
#' threshopt <- optimal_threshold(refdata = rdata, outliers = out_df)
#' }
#'
#'
#'

optimal_threshold <- function(refdata, outliers, var_col = NULL, warn=FALSE, verbose=FALSE,
                          plot =FALSE){

  #for a single species: clean data extraction

  if(deparse(substitute(refdata))!= outliers@dfname)stop('The reference dataset used in outlier detection and the output of outlier detection are different.')

  if(outliers@mode==FALSE){

    dfdata <- thresh_search(data = refdata, outliers = outliers, plot = plot, warn=warn, verbose=verbose)

  }else{

    if(is(refdata, 'list')){

      if(length(refdata)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

      splist <- refdata

    } else if(is(refdata, 'data.frame')){

      if(is.null(var_col)) stop('Provide the column with species names in parameter, var_col .')

      splist <- split(refdata, f= refdata[,var_col])

      if(length(splist)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

    }else{
      stop('Only list or dataframe of species occurences accepted or set the `var_col parameter`.')
    }
    spdata <- list()

    for (opt in seq_along(splist)) {

      spnames <- names(splist)[opt]

      if(is(refdata, 'data.frame'))  data2<- refdata[refdata[,var_col] == spnames, ] else  data2<- refdata[[spnames]]

      minmax <- thresh_search(data = data2, outliers = outliers, sp = spnames, plot = FALSE, var_col = var_col,
                              warn=warn, verbose=verbose)

      spdata[[opt]] <- data.frame(minima = unname(minmax[1]), maxima= unname(minmax[2]))

      spdata[[opt]]['species'] <- spnames

      dfdata <- do.call(rbind, spdata)

    }
  }
  return(dfdata)
}



