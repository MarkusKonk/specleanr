#' Title
#'
#' @param y
#' @param data
#' @param models
#' @param positive
#'
#' @return
#' @export
#'
#' @examples
sdmfit <- function(y, data, models, positive){

  #if(all(sapply(data, is.na))!=TRUE) data <- data[complete.cases(data)] else data

  match.arg(models, choices = c('RF', 'GLM', 'RF1'))

  modelist <- list()

  predictlist_train <- list()

  predictlist_test <- list()

  performance_train <- list()

  performance_test <- list()

  auctrain <- list()

  for (m_ii in 1:length(data)) {

    traindf <- data[[1]]

    testdf <- data[[2]]

    for (m_iii in 1:length(traindf)) {

      traindffinal <- traindf[[m_iii]]

      for (m_iv in 1:length(testdf)) {

        testdfinal <- testdf[[m_iv]]

        #========
        response <- deparse(substitute(y))


        if(!response%in% colnames(traindffinal)) stop(response, ' not found in the training data')

        eqn= as.formula(paste(response, paste('.', collapse = " + "),sep = " ~ "))

        rclass = unique(traindffinal[,response])

        if((length(rclass))<=1) stop('The classes must be atleast 2')

        if(models=='RF1'){
          trainfit <- randomForest(eqn, importance = TRUE,ntree=500, proximity = TRUE, data = traindffinal)

        }else if (models =='GLM'){
          trainfit <- glm(eqn, data = traindffinal, family = 'binomial')

        }else if(models == 'RF'){
          trainfit <- ranger(eqn, importance = 'permutation',scale.permutation.importance = TRUE,
                             mtry = 3, data = traindffinal)

        }else{
          stop('No model selected.')
        }

        modelist[[m_iii]] <- trainfit

        testpred <- predict(trainfit, testdfinal)

        trainpred <- predict(trainfit, traindffinal)

        predictlist_test[[m_iv]] <- testpred

        predictlist_train[[m_iv]] <- trainpred

        if(models=='RF1'){
          performance_test[[m_iv]] <- threshol_depend(obs =  testdfinal$species,model = testpred, positive = positive)

          performance_train[[m_iv]] <- threshol_depend(obs = traindffinal$species ,model = trainpred, positive = positive)

          auctrain[[m_iv]] <- auc_value(model = trainfit, newdata = testdfinal, mode = models)

        }else if(models=='RF'){
          performance_test[[m_iv]] <- threshol_depend(obs =  testdfinal$species,model = testpred$predictions, positive = positive)

          performance_train[[m_iv]] <- threshol_depend(obs = traindffinal$species ,model = trainpred$predictions, positive = positive)

          auctrain[[m_iv]] <- auc_value(model = trainfit, newdata = testdfinal)

        }else if(models=='GLM'){
          #convert probabilities to classes
          testclasses <- ifelse(testpred>=0.5, 'P', 'A')

          performance_test[[m_iv]] <- threshol_depend(obs =  testdfinal$species,
                                                      model = testclasses, positive = positive)
          trainclasses <- ifelse(trainpred>=0.5, 'P', 'A')

          performance_train[[m_iv]] <- threshol_depend(obs = traindffinal$species ,
                                                       model = trainclasses, positive = positive)

        }else{
          stop('No model selected. Please choose either or GLM')
        }


      }
    }
  }
  return(list(modelist, predictlist_test, predictlist_train,
              performance_test, performance_train, auctrain))

}
