#' Title
#'
#' @param data
#' @param nboots
#' @param setseed
#' @param testprob
#'
#' @return
#' @export
#'
#' @examples
#'
boots <- function(data, nboots, setseed=1101, testprob=0.2){

  set.seed(setseed)

  df_train <- list()

  df_test <- list()

  #n is the number of bootstraps determined by the user

  for (m_i in 1: nboots) {

    ind <- sample(x= 2, size= nrow(data), replace = TRUE, prob = c(1-testprob, testprob))

    df_train[[m_i]] <- as.data.frame(data[ind==1,])

    df_test[[m_i]] <- as.data.frame(data[ind==2,])
  }
  return(list(df_train, df_test))
}
