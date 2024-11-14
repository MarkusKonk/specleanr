
#' Title
#'
#' @param outlierdf
#' @param boots
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#'
specaccum_outliers <- function(outlierdf, boots = 100, seed = 1134){

  bts <- seq(1, boots, 1)

  set.seed(seed)

  samp <- lapply(bts, function(x){

    xc <- sample(outlierdf, replace = TRUE)

    outlierc_csum <- Reduce(union, xc, init = c(), accumulate = TRUE)

    outlierc_csum_null <- outlierc_csum[!sapply(outlierc_csum, is.null)]

    outlierfreq <- sapply(outlierc_csum_null, length)

    methodstot <- seq(1, length(outlierfreq), 1)

    dftf <- as.data.frame(cbind(methodstot, outlierfreq))}
  )

  ggplot(data = do.call(rbind, samp), aes(x=methodstot, outlierfreq))+
    geom_smooth()+
    labs(x='Number of methods', y='"Cummulative frequency')
}






