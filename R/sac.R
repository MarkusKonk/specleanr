
#' Title
#'
#' @param x
#' @param boots
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#'
outliercurve <- function(x, boots = 100,select = NULL,  linecolor = 'purple', seed = 1134){

  var <- x@varused

  #get outliers
  spout <- x@result

  if(unique(length(spout)>20) && is.null(select)) {
    stop("Provide a vector of maximum 20 group variables to display. Use the select parameter.")
  }else{

    inOut <- names(x@result)%in%select

    ing <- names(x@result)[which(inOut==TRUE)]

    if(length(ing)<=0) stop("Groups indicated in select parameter do not exist in the groups in the original data.", call. = FALSE)

    if(length(ing)<length(select)) warning("Some groups were dropped as they were not in the dataset.", call. = FALSE)

    spout <- spout[ing]

  }

  xs <- sapply(names(spout), function(yy){

    #get data based on names

    xd <- spout[[yy]]

    #first check for null values for methods that were not successful
    checkNA <- sapply(xd, nrow)

    #remove methods that didn't execute

    spnull <- xd[!sapply(checkNA,is.null)]

    #check if any method returned no outliers but will be retained while computing absolute outliers.
    len <- sapply(spnull, nrow)

    if(any(len==0)) cleanout <- spnull[len !=0] else cleanout <-  spnull #replace the list with empty data with y

    lgetout <- sapply(cleanout, function(xx)xx[[var]])

    bts <- seq(1, boots, 1)

    set.seed(seed)

    samp <- lapply(bts, function(x){

      xc <- sample(lgetout, replace = FALSE)

      outlierc_csum <- Reduce(union, xc, init = c(), accumulate = TRUE)

      outlierc_csum_null <- outlierc_csum[!sapply(outlierc_csum, is.null)]

      outlierfreq <- sapply(outlierc_csum_null, length)

      methodstot <- seq(1, length(outlierfreq), 1)

      dftf <- as.data.frame(cbind(methodstot, outlierfreq))
    })
    xout <- do.call(rbind, samp)

    xout[,'variables'] <- yy

    xout
  }, simplify = FALSE, USE.NAMES = FALSE)

  xsout <- do.call(rbind, xs)

  if(length(xsout$variables)<=4) ncolx = 2 else ncolx <- 3

  #assign either gam or loess

  if(!requireNamespace('ggplot2', quietly = TRUE))stop('Please ggplot2 to continue.')

  #instantiate the parameters

  methodstot = NULL; outlierfreq = NULL; variables = NULL

  ggplot2::ggplot(data = xsout, ggplot2::aes(x=methodstot, outlierfreq, group = variables))+

    #set to run either loess or gam

    {if(nrow(xsout)<=100){
      ggplot2::geom_smooth(method = 'loess', formula = y ~ x, color= linecolor)
    }else{
      ggplot2::geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color= linecolor)
    }}+

    ggplot2::theme_bw()+

    ggplot2::facet_wrap(~variables, ncol = ncolx, scales = 'free_y')+

    ggplot2::labs(x='Methods', y='Bootstrapped cummulative frequency')

}






