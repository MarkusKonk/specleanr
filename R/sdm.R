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
#' @param metrics Either to only consider threshold-dependent \code{(dep)}, threshold-independent \code{(indep)}
#'      or \code{all} evaluation metrics to determine model performance.
#' @param full Either full model output for \code{TRUE} or \code{FALSE} for only performance metrics output.
#'
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
#' \dontrun{
#'
#'
#' data(efidata)
#'
#'  datacheckf <- subset(efidata,  subset = scientificName %in%c("Squalius cephalus"))
#'
#'
#' #select species with enough records
#'
#' dataprep <- envextract(occurences = datacheckf, raster = worldclim,
#'                       lat = "decimalLatitude", lon = "decimalLongitude", binary = FALSE, prop = 0.8)
#'
#' nb <- boots(data = dataprep, nboots = 10, testprob = 0.3)
#'
#' sdmd <- sdmfit(data = nb, models = 'RF')
#' }
#'


sdmfit <- function(data, models, cutoff = 0.5, metrics='all', full=FALSE){

  match.argc(models, choices = c('RF', 'GLM', 'RF1'))

  df <- data[[1]][[1]]

  #if(!response %in%colnames(df)) stop('The response variable', y, ' not found in the data.')

  if(is.null(attr(df, "presence"))) stop('Invalid data type used. Please use both response and boots function.')

  response <- "y" #already genrated in exextract

  #create a model equation to enable dynamic acceptance of parameters
  eqn= as.formula(paste(response, paste('.', collapse = " + "),sep = " ~ "))

  #obtain the number of categories in the response column(presence/absence(P/A), 1/0, 1 only, 0 only)
  rclass = unique(df[,response])

  #if the categories are less/greater than 2, stop
  if(length(rclass)<=1 || length(rclass)>2) stop('The classes must be 2')

  sapply(data, function(x){

    traindf <- x[[1]]
    traindf$y <- as.factor(traindf$y)

    testdf <- x[[2]]
    testdf$y <- as.factor(testdf$y)

    if(("RF1"%in%models)==TRUE){
      #the basic random forest model using randomForest function
      trainfit <- randomForest(eqn, importance = TRUE,ntree=500, proximity = TRUE, data = traindf)
      #GLM formualation
    }else if (("GLM"%in%models)==TRUE){

      trainfit <- suppressWarnings(glm(eqn, data = traindf, family = 'binomial'))
      #another random forest implementation using ranger function: speed is higher
    }else if(("RF"%in%models)==TRUE){

      trainfit <- ranger(eqn, importance = 'permutation', scale.permutation.importance = TRUE,
                         mtry = 3, probability = TRUE,  data = traindf)

    }else{
      stop('No model selected.')
    }
    #get model predictions and evaluations
    if(("RF1"%in%models)==TRUE){
      testpred <- predict(trainfit, testdf, type='prob')

      trainpred <- predict(trainfit, traindf, type='prob')

      perftest <- suppressWarnings(evaluate(data =  testdf, predictions = testpred, model = models,
                           response = response, cutoff = cutoff, metrics = metrics))

      perftrain <- suppressWarnings(evaluate(data = traindf, predictions = trainpred, model = models,
                            response = response, cutoff = cutoff, metrics = metrics))

    }else if(("RF"%in%models)==TRUE){

      testpred <- predict(trainfit, testdf, type='response')

      trainpred <- predict(trainfit, traindf, type='response')

      perftest <- suppressWarnings(evaluate(data =  testdf, predictions = testpred, response = response,
                           model=models, cutoff = cutoff, metrics = metrics))

      perftrain<- suppressWarnings(evaluate(data = traindf ,predictions = trainpred, response = response,
                           model=models, cutoff = cutoff, metrics = metrics))

    }else if(("GLM"%in%models)==TRUE){

      testpred <- predict(trainfit, testdf)

      trainpred <- predict(trainfit, traindf)

      perftest <- suppressWarnings(evaluate(data = testdf, predictions = testpred, response = response,
                           model = models, cutoff = cutoff, metrics = metrics))

      perftrain <- suppressWarnings(evaluate(data = traindf, predictions = trainpred, response = response,
                            model = models, cutoff = cutoff, metrics = metrics))

    }else{
      stop('No model selected. Please choose either or GLM')
    }
    #saving the model outputs...this is later used in the modelcomparison function.
    if(isTRUE(full)){
      return(list(modeloutput = trainfit, predtest = testpred, predtrain = trainpred, perftest = perftest, perftrain = perftrain))
    }else{
      return(list(perftest = perftest, perftrain = perftrain))
    }

  }, simplify = FALSE)
}
