

#' @param x . the datacleaner object
#'
#' @param y . the species index or name for multiple species
#'
#' @param raw Whether total number of outlier flagged in each method are provided (\code{TRUE}) or
#'        \code{FALSE} to return the percentage outlier contribution for each method.
#'
#' @title Visualize the outliers identified by each method
#'
#' @return ggplot object indicating outlier detection methods and number of outlier flagged.
#'
#' @export
#'
#'
setMethod(f="plot", signature = signature(x= "datacleaner", y="ANY"),

          definition = function(x, y, raw=TRUE){

            if(x@mode==FALSE){

              OUTDF <- extract_outliers(x)

              spnames <- ""
              refrecords <-nrow(get(x@dfname))

            }  else{

              OUTDF <- extract_outliers(x, sp =y)

              spnames <- names(x@result)[y]

              #the reference dataset is extracted from the dfname slot of multidetect function.
              refrecords <- nrow(get(x@dfname)[[y]])

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
            suppressMessages(suppressWarnings(suggested.packages(listpkgs=c("ggplot2"),reason="plotting outliers")))

            pltout <- ggplot2::ggplot(OUTDF, ggplot2::aes_string(x="method", y= if(raw==TRUE) "totaloutliers" else "pct"))+
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
#'
#' @title Visualize the outliers identified by each method
#'
#' @return ggplot object indicating outlier detection methods and number of outlier flagged.
#'
#' @export
#'
#'
ggoutliers <-  function(x, y, raw=TRUE){

  if(x@mode==FALSE){

    OUTDF <- extract_outliers(x)

    spnames <- ""
    refrecords <-nrow(get(x@dfname))

  }  else{

    OUTDF <- extract_outliers(x, sp =y)

    spnames <- names(x@result)[y]

    #the reference dataset is extracted from the dfname slot of multidetect function.
    refrecords <- nrow(get(x@dfname)[[y]])

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
  suppressMessages(suppressWarnings(suggested.packages(listpkgs=c("ggplot2"),reason="plotting outliers")))

  pltout <- ggplot2::ggplot(OUTDF, ggplot2::aes_string(x="method", y= if(raw==TRUE) "totaloutliers" else "pct"))+
    ggplot2::geom_bar(stat = 'identity', fill='bisque4')+
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



