#' @title  Species distribution modelling
#'
#' @param data Species data list with both testing and training, probably
#'      from boots function.
#' @param models The models to be used to examine the relationship between
#'      species occurrences and environmental parameters. Only Random forest
#'      and generalized linear
#'      models are accepted. \code{GLM} is used to set to Generalized Linear Models
#'      and \code{RF} for Random Forest. \code{RF1} variant is slower and is suggested
#'      if RF fails.
#' @param cutoff Defines a threshold classify the model predictions/probabilities
#'      as presence or absent. Default is 0.5. The maximum is 1 and lowest is 0.
#' @param metrics Either to only consider threshold-dependent, threshold-independent
#'      or all evaluation metrics to determine model performance.
#'
#' @return model runs, and evaluation metrics.
#'
#' @importFrom ranger ranger
#' @importFrom randomForest randomForest
#' @importFrom stats glm as.formula
#'
#' @export
#'
#' @examples
#'


sdmfit <- function(data, models, cutoff = 0.5, metrics='all', full=FALSE){

  match.argc(models, choices = c('RF', 'GLM', 'RF1'))

  #response <- deparse(substitute(y))

  df <- data[[1]][[1]]

  #if(!response %in%colnames(df)) stop('The response variable', y, ' not found in the data.')

  if(is.null(attr(df, "presence"))) stop('Invalid data type used. Please use both response and boots function.')

  #if(!response%in% colnames(df)) stop(response, ' not found in the training data')

  response <- "y"

  eqn= as.formula(paste(response, paste('.', collapse = " + "),sep = " ~ "))

  rclass = unique(df[,response])

  if(length(rclass)<=1 || length(rclass)>2) stop('The classes must be 2')

  lx <- sapply(data, function(x){

    traindf <- x[[1]]
    traindf$y <- as.factor(traindf$y)

    testdf <- x[[2]]
    testdf$y <- as.factor(testdf$y)

    if(("RF1"%in%models)==TRUE){

      trainfit <- randomForest(eqn, importance = TRUE,ntree=500, proximity = TRUE, data = traindf)

    }else if (("GLM"%in%models)==TRUE){

      trainfit <- suppressWarnings(glm(eqn, data = traindf, family = 'binomial'))

    }else if(("RF"%in%models)==TRUE){


      trainfit <- ranger(eqn, importance = 'permutation', scale.permutation.importance = TRUE,
                         mtry = 3, probability = TRUE,  data = traindf)

    }else{
      stop('No model selected.')
    }

    if(("RF1"%in%models)==TRUE){
      testpred <- predict(trainfit, testdf, type='prob')

      trainpred <- predict(trainfit, traindf, type='prob')

      perftest <- evaluate(data =  testdf, predictions = testpred, model = models,
                           response = response, cutoff = cutoff, metrics = metrics)

      perftrain <- evaluate(data = traindf, predictions = trainpred, model = models,
                            response = response, cutoff = cutoff, metrics = metrics)

    }else if(("RF"%in%models)==TRUE){

      testpred <- predict(trainfit, testdf, type='response')

      trainpred <- predict(trainfit, traindf, type='response')

      perftest <- evaluate(data =  testdf, predictions = testpred, response = response,
                           model=models, cutoff = cutoff, metrics = metrics)

      perftrain<- evaluate(data = traindf ,predictions = trainpred, response = response,
                           model=models, cutoff = cutoff, metrics = metrics)

    }else if(("GLM"%in%models)==TRUE){

      testpred <- predict(trainfit, testdf)

      trainpred <- predict(trainfit, traindf)

      perftest <- evaluate(data = testdf, predictions = testpred, response = response,
                           model = models, cutoff = cutoff, metrics = metrics)

      perftrain <- evaluate(data = traindf, predictions = trainpred, response = response,
                            model = models, cutoff = cutoff, metrics = metrics)

    }else{
      stop('No model selected. Please choose either or GLM')
    }
    if(isTRUE(full)){
      return(list(modeloutput = trainfit, predtest = testpred, predtrain = trainpred, perftest = perftest, perftrain = perftrain))
    }else{
      return(list(predtest = testpred, predtrain = trainpred, perftest = perftest, perftrain = perftrain))
    }

  }, simplify = FALSE)
}
