
#' @title Determine the threshold using Locally estimated or weighted Scatterplot Smoothing.
#'
#' @inheritParams ocindex
#' @param data \code{Dataframe}. The reference dataframe were absolute outliers will be removed.
#' @param outliers \code{datacleaner}. Datacleaner output with outliers flagged in \code{multidetect} function.
#' @param plotsetting \code{list}. to show plot of loess fitted function with local and global maxima (optimal threshold and clean data).
#'        The list had two parameters. 1) plot to indicate the plot and group to provide the plot title.
#' @param var_col \code{string}. A column with species names if \code{dataset} for species is a dataframe not a list.
#'        See \code{\link{pred_extract}} for extracting environmental data.
#' @param verbose \code{logical}. If true, then messages about the outlier flagging will be displayed.
#' @param cutoff \code{numeric}. Ranging from 0.5 to 0.8 indicating the cutoff to initiate the
#'        LOESS model to optimize the identification of absolute outliers.
#' @param tloss \code{seqences} Indicates the sequence for tuning the the span parameter of the LOESS model.
#' @importFrom stats loess optimize
#' @importFrom graphics abline legend lines points title
#' @return Returns \code{numeric} of most suitable threshold at globalmaxima or localmaxima of the loess smoothing.
#'
#'
#'
search_threshold <- function(data, outliers,
                             sp = NULL,
                             plotsetting,
                             var_col = NULL,
                             warn=FALSE,
                             verbose=FALSE,
                             cutoff,
                             tloss = seq(0.1, 1, 0.1)){

  var <- outliers@varused

  if(length(var)>1) var <- sp else var

  #handle data groups with proportions less than 0.5 and groups with less than 2 methods with outliers.

  gcheck <- tryCatch(expr = ocindex(x= outliers, sp = sp, threshold = 0.1,
                                    absolute = TRUE, props = TRUE, warn = FALSE),
                     error = function(e) if(grepl('The methods with outliers', e$message)) NULL else 'unknown error')

  if(!is.null(gcheck)){

    maxtr <- max(gcheck$absolute_propn)

    if(maxtr >= cutoff){

      #get unique proportions
      uniqprop <- unique(gcheck$absolute_propn)

      if(length(uniqprop)<=2){
        NULL
      }else{

        el <- lapply(tloss, function(tt){

          ee <- try(ocindex(x= outliers, sp = sp, absolute = TRUE, threshold = tt, warn = warn),
                    silent = TRUE)

          if(!inherits(ee, 'try-error')) {

            varc <- unlist(data[, var])

            indx <- which(!varc %in% ee)

            datIn <- data[indx,]

            dt <- data.frame(th = tt, val= nrow(datIn))
          }
        })
        elout <-  Reduce(rbind, el)

        tl <- sapply(tloss, function(oo){

          lwrs <- tryCatch(expr = loess(val~th, data= elout, span = oo),#lowess(x = elout$th, y = elout$val, f = oo),#

                           error= function(e) NULL, warning=function(w) NULL)

          if(!is.null(lwrs)) vec <- data.frame(rmse = sqrt(mean((predict(lwrs)-unlist(elout$val))^2)), spans = oo) else vec <- data.frame(rmse = NA, spans = oo)

        }, simplify = FALSE, USE.NAMES = FALSE)

        spanout <- Reduce(rbind, tl)

        bestspan <- spanout$spans[which.min(spanout$rmse)]

        if(length(bestspan) != 0){

          #Get the maxima and minima ##thresholds when clean extracted data has reached highest.
          #Flat curve or where the slope of the line is the highest.: identify the first derivative
          #For the fitted data
          #completely, the f(x) = 0
          #maxima: a maximum where any shift the gradient decreases
          #https://stackoverflow.com/questions/12183137/calculate-min-max-slope-of-loess-fitted-curve-with-r

          fit <- loess(val ~ th, data = elout, span = bestspan)

          grid_x <- seq(min(elout$th), max(elout$th), length.out = 100)

          pred_y <- predict(fit, newdata = data.frame(th = grid_x))

          maxlog <- c(FALSE, diff(sign(diff(pred_y))) == -2, FALSE)

          l_max_x <- min(grid_x[maxlog])

          l_max_y <- min(pred_y[maxlog])


          opt <- optimize(function(x) -predict(fit, newdata = data.frame(th = x)),
                          interval = range(elout$th))

          g_max_x = opt$minimum

          g_max_y = -opt$objective

          #handle instance when g_max_x is less than the max threshold

          if(g_max_x <= maxtr && g_max_x<=cutoff) {
            g_max_x <- max(elout$th)
            g_max_y <- elout$val[which.max(elout$th)]
          }
          plotvars <- modifyList(x = list(plot=FALSE, group = NULL), plotsetting)

          # get variables
          group <- plotvars$group
          plot  <- plotvars$plot


          if(isTRUE(plot)){

            plot(elout$th, elout$val, pch = 20, col = "grey15", main = " ",
                 mgp = c(2, 1, 0), xlab ='Thresholds', ylab = 'Data retained after outlier removal',
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 cex.main = 0.7,
                 xlim = c(min(elout$th), max(elout$th)+0.05))

            lines(grid_x, pred_y, col = "grey40", lwd = 2)

            points(l_max_x, l_max_y, col = "purple", pch = 8, cex = 1.2)

            points(g_max_x, g_max_y, col = "red", pch = 8, cex = 1.2)

            abline(v=l_max_x, col='purple', lty = 2)

            abline(h=l_max_y, col='purple', lty = 4)

            abline(h=g_max_y, col='red', lty = 4)

            abline(v=g_max_x, col='red', lty = 4)

            if(!is.null(group)) title(group, line = 0.5)

            step <- 10^(floor(log10(l_max_y)) - 1)

            xmax <- if(g_max_x<=0.4)  g_max_x else g_max_x-0.4

            legend(x= xmax, y = l_max_y-step,
                   legend = c("Fitted curve", "Local maximum", "Global maximum"),
                   col    = c("grey40", "purple", "red"),
                   lty    = c(1, 0, 0),
                   pch    = c(NA, 19, 19),
                   lwd    = c(2, 0, 0),
                   y.intersp = 0.8,
                   x.intersp = 0.1,
                   pt.cex = 0.5,
                   bty = 'n')
          }
          return(c(localmaxima = l_max_x, globalmaxima = g_max_x))
        }else {
          NULL
        }
      }
    }else{
      NULL
    }
  }else{
    NULL
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
                              plotsetting =  list(plot = FALSE, group = NULL),
                              cutoff = 0.6){

  #for a single species: clean data extraction

  if(deparse(substitute(refdata))!= outliers@dfname)stop('The reference dataset used in outlier detection and the output of outlier detection are different.')

  if(outliers@mode==FALSE){

    dfdata <- search_threshold(data = refdata, outliers = outliers, plotsetting = plotsetting, warn=warn, verbose=verbose, cutoff = cutoff)

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

      minmax <- search_threshold(data = data2, outliers = outliers, sp = spnames, plotsetting = plotsetting, var_col = var_col,
                                 warn=warn, verbose=verbose, cutoff = cutoff)

      if(!is.null(minmax)) spdata[[opt]] <- data.frame(localmaxima = unname(minmax[1]), globalmaxima= unname(minmax[2])) else spdata[[opt]] <- data.frame(localmaxima = NA, globalmaxima = NA)

      spdata[[opt]]['species'] <- spnames

      dfdata <- do.call(rbind, spdata)

    }
  }
  invisible(dfdata)
}



