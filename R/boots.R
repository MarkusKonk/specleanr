
#' @title Creating sub samples for modeling evaluation.
#'
#' @param data Data to be subsampled and partitioned.
#' @param nboots the number of iteration of subsampling to be conducted.
#' @param setseed For reproducibility of the sub samples across platforms and users.
#' @param testprob The probability to be used for partitioning data.
#'
#' @return A list of both training and test datasets to be used in the sdmfit.
#'
#' @seealso [sdmfit()]
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #data <-
#'
#' }
#' @author Anthony Basooma, \email{anthony.basooma@@boku.ac.at}
#'
boots <- function(data, nboots, setseed=1234, testprob=0.2){

  set.seed(setseed)

  sqlen <- seq(1, nboots, by=1)

  sx <- sapply(sqlen, function(x){

    ind <- sample(x= 2, size= nrow(data), replace = TRUE, prob = c(1-testprob, testprob))

    dftrain <- as.data.frame(data[ind==1,])

    dftest <- as.data.frame(data[ind==2,])

    return(list(train = dftrain, test = dftest))

  }, simplify = FALSE)

}
