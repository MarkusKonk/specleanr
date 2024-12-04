

#' @param x . the datacleaner object
#'
#' @param y . the species index or name for multiple species
#'
#' @param raw Whether total number of outlier flagged in each method are provided (\code{TRUE}) or
#'        \code{FALSE} to return the percentage outlier contribution for each method.
#'
#' @param var_col \code{string} If the reference dataset is a \code{dataframe} class,  \code{multiple = TRUE}, and
#'      \code{raw = FALSE}, provide the column with species or parameter names.
#'
#' @title Visualize the outliers identified by each method
#'
#' @return ggplot object indicating outlier detection methods and number of outlier flagged.
#'
#' @export
#'
#'
setMethod(f="plot", signature = signature(x= "datacleaner", y="ANY"),

          definition = function(x, y, raw=TRUE, var_col = NULL){

            if(x@mode==FALSE){

              outdf <- extract_outliers(x)


              if(nrow(outdf)<1) stop("Nothing to plot. No outliers were flagged by all methods.")

              spnames <- ""

              refrecords <-nrow(get(x@dfname))

            }  else{

              outdf <- extract_outliers(x, sp =y)

              if(is.numeric(y)) spnames <- names(x@result)[y] else spnames <- y

              #the reference dataset is extracted from the dfname slot of multidetect function.

              if(length(x@varused)>1) {

                refrecords <- nrow(get(x@dfname))
              } else{
                if(is(get(x@dfname), 'list')) {

                  refrecords <- nrow(get(x@dfname)[[y]])

                } else {
                  #get ref data
                  if(is.null(var_col)) stop('Provide the column with species or parameter names.', call. = FALSE)

                  rfd <- get(x@dfname)

                  splitdf <- split(rfd, f= rfd[,var_col])

                  refrecords <- nrow(splitdf[[y]])
                }
              }

            }

            #show percentage outlier composition to the total number or record in reference datasets

            if(raw==TRUE){
              outdf
              ylab <- 'Number of outliers flaaged'
            }else{

              outdf[,"totrecords"] <- refrecords

              outdf[,"pct"] <- (outdf$totaloutliers/outdf$totrecords)*100

              ylab <- "Percentage outlier contribution (%)"
            }
            if(nrow(outdf)>=7) {
              angle <- 45
              hjust <- 1
            } else{
              angle <- 0
              hjust <- 0.5
            }
            #suppressMessages(suppressWarnings(suggested.packages(listpkgs=c("ggplot2"),reason="plotting outliers")))

            method = NULL; pct = NULL ; totaloutliers = NULL

            pltout <- ggplot2::ggplot(outdf, ggplot2::aes(x=method, y= if(raw==TRUE) totaloutliers else pct))+
              ggplot2::geom_bar(stat = 'identity', fill='grey40')+
              ggplot2::theme_bw()+
              ggplot2::theme(legend.position = 'none',
                             panel.grid.major.x = ggplot2::element_blank(),
                             panel.grid.major.y = ggplot2::element_blank(),
                             axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust),
                             plot.title = ggplot2::element_text(size = 10),
                             axis.text = ggplot2::element_text(size = 12))+
              ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+

              ggplot2::labs(x="Outlier detection methods", y=ylab, title = spnames)

            return(pltout)
          })



#' @param x . the datacleaner object
#'
#' @param select . the species index or name for multiple species
#'
#' @param color \code{string}. Color of the bars. Default is \code{grey}.
#' @param var_col \code{string} If the reference dataset is a \code{dataframe} class,  \code{multiple = TRUE}, and
#'      \code{raw = FALSE}, provide the column with species or parameter names.
#'
#' @title Visualize the outliers identified by each method
#'
#' @return ggplot object indicating outlier detection methods and number of outlier flagged.
#'
#' @export
#'
#'
ggoutliers <-  function(x, select = NULL, color='purple', desc = TRUE){

  outdf <- extractoutliers(x)

  if(x@mode==FALSE){

    if(all(outdf$totaloutliers<1)==TRUE) stop("Nothing to plot. No outliers were flagged by all methods")

    outdf$method <- factor(outdf$method , levels = outdf$method [order(outdf$totaloutliers , decreasing = desc)])

    meanval <- mean(outdf$totaloutliers)

  }else{
      if(unique(length(outdf$groups)>=10) && is.null(select)){

        stop("Provide a vectors of particular groups to plot, use the select parameter to provide a vector of groups.")

        }
     if(!is.null(select)) outdf <- outdf[outdf$groups %in% select, ] else outdf

      meanval <- aggregate(totaloutliers~groups, data = outdf, mean)

      colnames(meanval) <- c('groups', 'meanvalue')
    }

  if(nrow(outdf)>=7) {
    angle <- 45; hjust <- 1
  } else{
    angle <- 0; hjust <- 0.5
  }

  method = NULL; groups = NULL ; totaloutliers = NULL

  if(!requireNamespace("ggplot2", quietly = TRUE)) stop('Please install the ggplot2 first to continue.')

  if(!requireNamespace("tidytext", quietly = TRUE)) stop('Please install the tidytext first to continue.')

  pltout <- ggplot2::ggplot(data = outdf, ggplot2::aes(x= if(x@mode==TRUE) tidytext::reorder_within(method, -totaloutliers, groups) else x = method,
                                                y = totaloutliers))+

    ggplot2::geom_bar(stat = 'identity', fill=color)+

    ggplot2::theme_bw()+

    {if(x@mode==TRUE) ggplot2::facet_wrap(~groups, scales = 'free')}+

    {if(x@mode==TRUE) tidytext::scale_x_reordered()} +

    ggplot2::theme(legend.position = 'none',

                   panel.grid.major = ggplot2::element_blank(),

                   panel.grid.minor = ggplot2::element_blank(),

                   axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust),


                   axis.text = ggplot2::element_text(size = 10))+

    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+

    {if(x@mode==FALSE){
      ggplot2::geom_hline(yintercept = meanval, linetype='twodash', linewidth = 1)

    }else{
      ggplot2::geom_hline(data = meanval, aes(yintercept = meanvalue),linetype='twodash', linewidth = 1)
    }}+

    ggplot2::labs(x="Outlier detection methods", y ='Number of outliers')

  return(pltout)
}

