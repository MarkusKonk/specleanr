
#' @title Creating sub-samples for modeling evaluation.
#'
#' @param data Data to be sub sampled and partitioned.
#' @param nboots the number of iterations of sub sampling to be conducted.
#' @param setseed For reproducibility of the sub-samples across platforms and users.
#' @param testprob The probability to be used for partitioning data.
#'
#' @return A list of training and test data sets for use in the sdmfit.
#'
#' @seealso \code{\link{sdmfit}}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(jdsdata)
#'
#' #select species with enough records
#'
#' dataprep <- envextract(occurences = jdsdata, raster = worldclim,
#'                       lat = "lat", lon = "lon", binary = FALSE, prop = 0.8)
#'
#' nboots <- boots(data = dataprep, nboots = 10, testprob = 0.3)
#'
#' }
#' @author ##
#'
boots <- function(data, nboots, setseed=1234, testprob=0.2){

  set.seed(setseed)

  sqlen <- seq(1, nboots, by=1)

  sapply(sqlen, function(x){

    ind <- sample(x= 2, size= nrow(data), replace = TRUE, prob = c(1-testprob, testprob))

    dftrain <- as.data.frame(data[ind==1,])

    dftest <- as.data.frame(data[ind==2,])

    return(list(train = dftrain, test = dftest))

  }, simplify = FALSE)

}

