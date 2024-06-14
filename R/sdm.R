#' @title  Species distribution modelling
#'
#' @param y The species variable with presence (1) and absences (0).
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
#' @param mode Either to only consider threshold-dependent, threshold-independent
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

sdmfit <- function(y, data, models, cutoff = 0.5, mode='all'){

  match.arg(models, choices = c('RF', 'GLM', 'RF1'))

  resp <- y

  response <- deparse(substitute(resp))

  df <- data[[1]][[1]]

  if(!response %in%colnames(df)) stop('The response variable', resp, ' not found in the data.')


  if(is.null(attr(df, "presence"))) stop('Invalid data type used. Please use both response and boots function.')

  if(!response%in% colnames(df)) stop(response, ' not found in the training data')

  eqn= as.formula(paste(response, paste('.', collapse = " + "),sep = " ~ "))

  rclass = unique(df[,response])

  if(length(rclass)<=1 || length(rclass)>2) stop('The classes must be 2')

  lx <- sapply(data, function(x){

    traindf <- x[[1]]

    testdf <- x[[2]]

    if(models=='RF1'){
      trainfit <- randomForest(eqn, importance = TRUE,ntree=500, proximity = TRUE, data = traindf)

    }else if (models =='GLM'){
      trainfit <- suppressWarnings(glm(eqn, data = traindf, family = 'binomial'))

    }else if(models == 'RF'){
      trainfit <- ranger(eqn, importance = 'permutation', scale.permutation.importance = TRUE,
                         mtry = 3, probability = TRUE,  data = traindf)

    }else{
      stop('No model selected.')
    }

    if(models=='RF1'){
      testpred <- predict(trainfit, testdf, type='prob')

      trainpred <- predict(trainfit, traindf, type='prob')

      perftest <- evaluate(data =  testdf, predictions = testpred, model = models,
                           response = response, cutoff = cutoff, mode = mode)

      perftrain <- evaluate(data = traindf, predictions = trainpred, model = models,
                            response = response, cutoff = cutoff, mode = mode)

    }else if(models=='RF'){

      testpred <- predict(trainfit, testdf, type='response')

      trainpred <- predict(trainfit, traindf, type='response')

      perftest <- evaluate(data =  testdf, predictions = testpred, response = response,
                           model=models, cutoff = cutoff, mode = mode)

      perftrain<- evaluate(data = traindf ,predictions = trainpred, response = response,
                           model=models, cutoff = cutoff, mode = mode)

    }else if(models=='GLM'){

      testpred <- predict(trainfit, testdf)

      trainpred <- predict(trainfit, traindf)

      perftest <- evaluate(data = testdf, predictions = testpred, response = response,
                           model = models, cutoff = cutoff, mode = mode)

      perftrain <- evaluate(data = traindf, predictions = trainpred, response = response,
                            model = models, cutoff = cutoff, mode = mode)

    }else{
      stop('No model selected. Please choose either or GLM')
    }
    return(list(modeloutput = trainfit, predtest = testpred, predtrain = trainpred,
                perftest = perftest, perftrain = perftrain))#switch
  }, simplify = FALSE)
}
