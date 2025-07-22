
#' @title Determine the threshold using Locally estimated or weighted Scatterplot Smoothing.
#'
#' @inheritParams ocindex
#' @param data \code{Dataframe}. The reference dataframe were absolute outliers will be removed.
#' @param outliers \code{datacleaner}. Datacleaner output with outliers flagged in \code{multidetect} function.
#' @param plot \code{logical}. to show plot of loess fitted function with minima and maxima (optimal threshold and clean data).
#' @param colors \code{vector}. Colors for both the true data and the loess fitted data lines.
#' @param var_col \code{string}. A column with species names if \code{dataset} for species is a dataframe not a list.
#'        See \code{\link{pred_extract}} for extracting environmental data.
#' @param verbose \code{logical}. If true, then messages about the outlier flagging will be displayed.
#' @param useropt \code{numeric} The default is 0.8, to ensure that the loess maximum does not fall below user optima if it is not properly searched
#'      using the loess model.
#' @importFrom stats loess
#' @return Returns \code{numeric} of most suitable threshold at maxima of the loess smoothing.
#'
#'
#'
search_threshold <- function(data, outliers,  sp = NULL, plot=FALSE, var_col = NULL,
                             warn=FALSE,
                             verbose=FALSE,
                             colors = c('darkblue', 'orange'),
                             useropt= 0.8){

  var <- outliers@varused

  if(length(var)>1) var <- sp else var

  tuneLoess <- seq(0.1, 1, 0.1)

  el <- lapply(tuneLoess, function(tt){

    ee <- tryCatch(expr = ocindex(x= outliers, sp = sp, absolute = TRUE, threshold = tt, warn = warn),
                   error = function(e) {

                     #handle groups without enough data
                     if(grepl('The methods with outliers are', e$message)==TRUE){

                       return(NULL)
                     }else if(grepl('No absolute outliers found with a threshold', e$message)){

                       return(NA)
                     }else{
                       stop('Unknown error')
                     }
                   })

    if(!is.null(ee)) {

      varc <- unlist(data[, var])

      indx <- which(!varc %in% ee)

      datIn <- data[indx,]

      dt <- data.frame(th = tt, val= nrow(datIn))
    }
  })

  elout <-  Reduce(rbind, el)

  if(!is.null(elout)){
    tl <- sapply(tuneLoess, function(oo){
      lwrs <-tryCatch(expr = loess(val~th, data= elout, span = oo),
                      error= function(e){
                        return(NULL)
                      },
                      warning=function(w){
                        return(NULL)
                      })
      if(!is.null(lwrs)) vec <- data.frame(rmse = sqrt(mean((predict(lwrs)-unlist(elout$val))^2)), spans = oo) else vec <- data.frame(rmse = NA, spans = oo)

    }, simplify = FALSE, USE.NAMES = FALSE)

    spanout <- Reduce(rbind, tl)

    bestspan <- spanout$spans[which.min(spanout$rmse)]

    lwrsout <- loess(val~th, data= elout, span = bestspan)

    #Get the maxima and minima ##thresholds when clean extracted data has reached highest.
    #Flat curve or where the slope of the line is the highest.: identify the first derivative
    #For the fitted data
    #completely, the f(x) = 0
    #maxima: a maximum where any shift the gradient decreases
    #https://stackoverflow.com/questions/12183137/calculate-min-max-slope-of-loess-fitted-curve-with-r

    firstdirev <- diff(lwrsout$fitted)

    maximaval <- lwrsout$fitted[which.min(firstdirev)]


    minimalval <- lwrsout$fitted[which.max(firstdirev)]

    optimalmax <- elout$th[which.min(firstdirev)]

    optimalmin<- elout$th[which.max(firstdirev)]

    if(optimalmax<useropt) optimalmax <- useropt

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

      th <- NULL; val <- NULL

      gplot <- ggplot2::ggplot(data = elout, ggplot2::aes(x = th, y = val))+

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

  } else{
   if(isTRUE(verbose)) message('The group ', sp, ' has fewer records and less than 2 methods returned outliers. Original data will be returned.')
    return(elout)
  }
}


#' @title Optimize threshold for clean data extraction.
#'
#' @inheritParams search_threshold
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

    dfdata <- search_threshold(data = refdata, outliers = outliers, plot = plot, warn=warn, verbose=verbose)

  }else{

    if(is(refdata, 'list')){

      if(length(refdata)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

      splist <- refdata

    } else if(is(refdata, 'data.frame')){

      if(is.null(var_col)) stop('Provide the column with species names in parameter, var_col .')

      splist <- split(refdata, f= refdata[,var_col])

      if(length(splist)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

    }else{
      stop('Only list or dataframe of species occurences accepted or set the var_col parameter.')
    }
    spdata <- list()

    for (opt in seq_along(splist)) {

      spnames <- names(splist)[opt]

      if(is(refdata, 'data.frame'))  data2<- refdata[refdata[,var_col] == spnames, ] else  data2<- refdata[[spnames]]

      minmax <- search_threshold(data = data2, outliers = outliers, sp = spnames, plot = FALSE, var_col = var_col,
                              warn=warn, verbose=verbose)

      if(!is.null(minmax)) spdata[[opt]] <- data.frame(minima = unname(minmax[1]), maxima= unname(minmax[2])) else spdata[[opt]] <- data.frame(minima = NA, maxima = NA)

      spdata[[opt]]['species'] <- spnames

      dfdata <- do.call(rbind, spdata)

    }
  }
  return(dfdata)
}



