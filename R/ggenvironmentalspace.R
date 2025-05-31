

#' Title
#'
#' @param qcdata zz
#' @param xvar zz
#' @param yvar t
#' @param xlab tz
#' @param ylab zzt
#' @param ncol zz
#' @param nrow tz
#' @param setcolors tt
#' @param colornames zzt
#' @param legendposition zzz
#' @param pointsize uz
#' @param themebackground zzz
#'
#' @return ggplot2 graph
#'
#' @export
#'
ggenvironmentalspace <- function(qcdata,
                                 xvar,
                                 yvar,
                                 zvar = NULL,
                                 labelvar = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 zlab = NULL,
                                 ncol = 1,
                                 nrow = 1,
                                 scalecolor = NULL, #grey, manual, viridis
                                 colorvalues = 'auto', #manual color values for ggplot2
                                 legend_position = 'inside',
                                 legend_inside = c(0.6, 0.4),
                                 pointsize = 1,
                                 themebackground ='bw',
                                 fontsize = 13,
                                 legtitle = 'blank',
                                 ggxangle = 1,
                                 xhjust = 0.5,
                                 xvjust = 1,
                                 type = '2D',
                                 tick.marks= NULL,
                                 label.tick.marks=NULL,
                                 axis = NULL,
                                 main = NULL,
                                 angle = NULL,
                                 pch = 'auto',
                                 lpos3d = NULL,
                                 cexsym= NULL
){

  xc <- c(xvar, yvar)%in%colnames(qcdata)

  vardd <- c(xvar, yvar)[which(xc==FALSE)]

  if(length(which(xc==FALSE)>=1))stop('The variable indicated ', paste(vardd, collapse =' and '), ' are/is not in the quality controlled dataset ', deparse(substitute(qcdata)))

  len <- length(unique(qcdata$groups))
  if(len>20)warning('The facets may not appear properly as the groups are greater than 20.')

  if(!is.null(scalecolor) && scalecolor=='manual'){
    if(length(unique(qcdata$label))!= length(colorvalues)) stop('The of colors set should be equal to ', length(colorvalues))
  }

  if(type=='2D'){
    check_packages(pkgs = c("ggplot2"))

    label <- NULL

    plt <- ggplot2::ggplot(qcdata, ggplot2::aes(x= .data[[xvar]], y= .data[[yvar]], colour = label, shape = label))+

      ggplot2::geom_point(size = pointsize)+

      {if(themebackground=='classic'){

        ggplot2::theme_classic()

      }else if(themebackground=='gray'){

        ggplot2::theme_gray()
      }else{
        ggplot2::theme_bw() +

        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank())
      }
      }+

      {if(len>1) ggplot2::facet_wrap(~groups, scales = 'free', ncol = ncol, nrow = nrow)}+

      {if(scalecolor=='gray'){

        ggplot2::scale_color_grey()

      }else if(scalecolor=='manual'){

        ggplot2::scalecolor(values = colorvalues)

      } else{
        ggplot2::scale_colour_viridis_d(alpha = 1, direction = -1)
      }
      }+

      {if(legend_position=='bottom') ggplot2::theme(legend.position = 'bottom')}+

      {if(legend_position=='top')    ggplot2::theme(legend.position = 'top')}+

      {if(legend_position=='inside') ggplot2::theme(legend.position = 'inside',
                                                    legend.position.inside = legend_inside)}+

      ggplot2::theme(legend.background = ggplot2::element_blank())+

      ggplot2::theme(text = ggplot2::element_text(size = fontsize),
                     axis.text.x = ggplot2::element_text(angle = ggxangle,
                                                         hjust = xhjust,
                                                         vjust = xvjust))+

      {if(legtitle=='blank')ggplot2::theme(legend.title = ggplot2::element_blank())}+

    ggplot2::labs(x = xlab, y= ylab)

    print(plt)
  }else if(type=="3D"){

    check_packages(pkgs = "scatterplot3d")

    if(is.null(zvar)) stop("For 3D plot provide a numeric z parameter.")

    x1 <- qcdata[[xvar]]
    y1 <- qcdata[[yvar]]
    z1 <- qcdata[[zvar]]
    labels1 <- qcdata[[labelvar]]

    #check if x, y, z are numeric

    tf <- sapply(list(x1, y1, z1), is.numeric)

    if(all(tf==TRUE)==FALSE) stop("All xvar, yvar, and zvar parameters should be numeric")

    #change the pch
    if(length(pch)==1 && pch=='auto'){

      pp <- as.numeric(as.factor(unique(labels1)))

      ppd <- as.numeric(as.factor(labels1))

    }  else {
      pp <- pch

      tbv <- table(as.numeric(as.factor(labels1)))

      ppd <- rep(pch, tbv)
    }

    if(length(colorvalues)==1 && colorvalues=='auto'){

      cpp <- as.numeric(as.factor(unique(labels1)))

      cc <- as.numeric(as.factor(labels1))
    }else{

      cpp <- colorvalues

      #cc  <- colorvalues[as.factor(labels1)]

      tbv <- table(as.numeric(as.factor(labels1)))

      cc <- rep(cpp, tbv)
    }


    s3plot <- scatterplot3d::scatterplot3d(x    = x1,
                                           y    = y1,
                                           z    = z1,
                                           color= cc,
                                           pch  =   ppd,
                                           xlab = xlab,
                                           ylab = ylab,
                                           zlab = zlab,
                                           main = main,
                                           cex.symbols = cexsym)
    # Add a legend
    graphics::legend(x = lpos3d,
                     legend = unique(labels1),
                     y.intersp = 1.3,
                     x.intersp = 1.2,
                     bty = "n",
                     cex = 1,
                     col = cpp,
                     pch = pp)
  }else{
    stop('Please for dimensions set either 2D or 3D')
  }
}
utils::globalVariables(".data")
