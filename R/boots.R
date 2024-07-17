
#' @title Creating sub-samples for modeling evaluation.
#'
#' @param data \code{dataframe}. Data to be sub sampled and partitioned.
#' @param nboots \code{integer}. Indicating the number of iterations of sub sampling to be conducted.
#' @param setseed \code{integer} Allow reproducibility of the sub-samples across platforms and users.
#' @param testprob \code{numeric} Value ranging from at least 0.1 to 1. The probability to be used for partitioning data.
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
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
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
  if(!is(data, 'data.frame'))stop("Only dataframes can be partitioned in boot function.")

  if(isFALSE(nboots%%1==0)) stop("nboots must be an interger.")

  if(testprob>0.5) warning("The proportion of test data is greater than the training data.")

  sqlen <- seq(1, nboots, by=1)

  if(nrow(data)<2)stop("Data points are too few to be partitioned.")

  sapply(sqlen, function(x){

    ind <- sample(x= 2, size= nrow(data), replace = TRUE, prob = c(1-testprob, testprob))

    dftrain <- as.data.frame(data[ind==1,])

    dftest <- as.data.frame(data[ind==2,])

    return(list(train = dftrain, test = dftest))

  }, simplify = FALSE)

}

