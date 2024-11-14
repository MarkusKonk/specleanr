

#' @param x . the datacleaner object
#'
#' @param y . the species index or name for multiple species
#'
#' @param raw Whether total number of outlier flagged in each method are provided (\code{TRUE}) or
#'        \code{FALSE} to return the percentage outlier contribution for each method.
#'
#' @param colsp \code{string} If the reference dataset is a \code{dataframe} class,  \code{multiple = TRUE}, and
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

          definition = function(x, y, raw=TRUE, colsp = NULL){

            if(x@mode==FALSE){

              OUTDF <- extract_outliers(x)


              if(nrow(OUTDF)<1) stop("Nothing to plot. No outliers were flagged by all methods.")

              spnames <- ""

              refrecords <-nrow(get(x@dfname))

            }  else{

              OUTDF <- extract_outliers(x, sp =y)

              if(is.numeric(y)) spnames <- names(x@result)[y] else spnames <- y

              #the reference dataset is extracted from the dfname slot of multidetect function.

              if(length(x@varused)>1) {

                refrecords <- nrow(get(x@dfname))
              } else{
                if(is(get(x@dfname), 'list')) {

                  refrecords <- nrow(get(x@dfname)[[y]])

                } else {
                  #get ref data
                  if(is.null(colsp)) stop('Provide the column with species or parameter names.', call. = FALSE)

                  rfd <- get(x@dfname)

                  splitdf <- split(rfd, f= rfd[,colsp])

                  refrecords <- nrow(splitdf[[y]])
                }
              }

            }

            #show percentage outlier composition to the total number or record in reference datasets

            if(raw==TRUE){
              OUTDF
              ylab <- 'Number of outliers flaaged'
            }else{

              OUTDF[,"totrecords"] <- refrecords

              OUTDF[,"pct"] <- (OUTDF$totaloutliers/OUTDF$totrecords)*100

              ylab <- "Percentage outlier contribution (%)"
            }
            if(nrow(OUTDF)>=7) {
              angle <- 45
              hjust <- 1
            } else{
              angle <- 0
              hjust <- 0.5
            }
            #suppressMessages(suppressWarnings(suggested.packages(listpkgs=c("ggplot2"),reason="plotting outliers")))

            method = NULL; pct = NULL ; totaloutliers = NULL

            pltout <- ggplot2::ggplot(OUTDF, ggplot2::aes(x=method, y= if(raw==TRUE) totaloutliers else pct))+
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
#' @param y . the species index or name for multiple species
#'
#' @param raw Whether total number of outlier flagged in each method are provided (\code{TRUE}) or
#'        \code{FALSE} to return the percentage outlier contribution for each method.
#' @param color \code{string}. Color of the bars. Default is \code{grey}.
#' @param colsp \code{string} If the reference dataset is a \code{dataframe} class,  \code{multiple = TRUE}, and
#'      \code{raw = FALSE}, provide the column with species or parameter names.
#'
#' @title Visualize the outliers identified by each method
#'
#' @return ggplot object indicating outlier detection methods and number of outlier flagged.
#'
#' @export
#'
#'
ggoutliers <-  function(x, y, raw=TRUE, color='purple', colsp= NULL){

  if(x@mode==FALSE){

    OUTDF <- extractoutliers(x)

    if(all(OUTDF$totaloutliers<1)==TRUE) stop("Nothing to plot. No outliers were flagged by all methods")

    spnames <- ""

    refrecords <-nrow(get(x@dfname))

  }  else{

    if(is.null(y)) stop("Provide the index or species to plot the outliers for a particular species.")

    if(is.numeric(y) && y>length(x@result))stop("The index number provided for y is out of bounds, the index number sholud not exceed ", length(x@result), ".", call. = FALSE)

    OUTDF <- extractoutliers(x, sp =y)

    if(all(OUTDF$totaloutliers<1)==TRUE) stop("Nothing to plot. No outliers were flagged by all methods")

     if(is.numeric(y)) spnames <- names(x@result)[y] else spnames <- y

    #the reference dataset is extracted from the dfname slot of multidetect function.

    if(length(x@varused)>1) {

      refrecords <- nrow(get(x@dfname))
      } else{
        if(is(get(x@dfname), 'list')) {

          refrecords <- nrow(get(x@dfname)[[y]])

        } else {
          #get ref data
          if(is.null(colsp)) stop('Provide the column with species or parameter names.', call. = FALSE)

          rfd <- get(x@dfname)

          splitdf <- split(rfd, f= rfd[,colsp])

          refrecords <- nrow(splitdf[[y]])
          }
      }

  }

  #show percentage outlier composition to the total number or record in reference datasets

  if(raw==TRUE){
    OUTDF
    ylab <- 'Number of outliers flaaged'
  }else{

    OUTDF[,"totrecords"] <- refrecords

    OUTDF[,"pct"] <- (OUTDF$totaloutliers/OUTDF$totrecords)*100

    ylab <- "Percentage outlier contribution (%)"
  }
  if(nrow(OUTDF)>=7) {
    angle <- 45
    hjust <- 1
  } else{
    angle <- 0
    hjust <- 0.5
  }
  method = NULL; pct = NULL ; totaloutliers = NULL
  pltout <- ggplot2::ggplot(OUTDF, ggplot2::aes(x=method, y= if(raw==TRUE) totaloutliers else pct))+
    ggplot2::geom_bar(stat = 'identity', fill=color)+
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
}



