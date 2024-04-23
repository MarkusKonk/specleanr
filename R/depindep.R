#' Title
#'
#' @param obs
#' @param model
#' @param positive
#'
#' @return
#' @export
#'
#' @examples
threshol_depend <- function(obs, model, positive){

  tab <- table(obs, model)
  levels <- as.character(unique(obs))

  if(length(levels)>2) stop('The species classes should be equal to 2 (present/absent')

  p<- levels[levels %in%positive] #Affects the sensitivity, specificity, NPV, PPV
  a<- levels[!levels %in%positive]

  #create confusion matrix table

  tp = tab[p,p]; fn=tab[p,a]; fp= tab[a,p]; tn = tab[a,a]

  total = tp+tn+fp+fn
  accuracy <- (tp + tn) / (tp+fn+fp+tn)
  sensitivity <- tp/(tp+fn)
  specificity <- tn/(fp+tn)
  precision  <- tp/(tp + fp)
  ppv <- precision
  npv <- tn/(tn+fn)
  detection_rate <- tp/(tp+fn)
  f1score <- 2*tp/(2*tp + fp + fn)
  prevalence <- (fn+tp)/(tn+fp+fn+tp)
  balancedaccuracy <- (sensitivity + specificity) / 2
  false_discovery_rate <- fp/(fp+tp)
  recall <- tp / (tp + fn)
  tss         = (sensitivity+specificity)-1
  misclassificationrate <- 1-accuracy
  po = accuracy
  pcorrect = ((tp+fp)/total)*((fn+tp)/total)
  pincorrect = ((fn+tn)/total)*((fp+tn)/total)
  pe = pcorrect+pincorrect
  kappa = (po-pe)/(1-pe)

  output <- data.frame(list(Accuracy=accuracy, Sensitivity = sensitivity,
                            Specificity= specificity,TSS= tss, Kappa = kappa,
                            PPV= ppv,NPV = npv,Precision= precision,Recall =recall,
                            F1=f1score, Prevalence= prevalence,
                            DetectionRate = detection_rate,
                            FDR = false_discovery_rate,BalancedAccuracy = balancedaccuracy,
                            MisclassificationRate = misclassificationrate))
  return(output)
}

#' Title
#'
#' @param model
#' @param newdata
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
auc_value <- function(model, newdata, mode='RF'){

  aucd <- list()

  if(mode=='RF') testpr <- predict(model, data = newdata) else predict(model, newdata = newdata)

  obs <- newdata$species
  n = length(obs)
  if (length(which(obs %in% c('A','P')))!=n)
    stop('observed values must be 0 or 1') #ensure observed are values 0 or 1
  n1 = as.double(length(which(obs=='P')));
  n0 = as.double(length(which(obs=='A')))
  if (n1==0 || n1==n) return( NaN ) #if all observed 1's or 0's return NaN

  if(mode=='RF') {
    pred0 = testpr$predictions[which(obs=='A')]
    pred1 = testpr$predictions[which(obs=='P')]
    ranks = rank(testpr$predictions,ties.method='average')#define ranks
  }else{
    pred0 = testpr[which(obs=='A')]
    pred1 = testpr[which(obs=='P')]
    ranks = rank(testpr,ties.method='average')#define ranks
  }

  ranks0 = ranks[which(obs=='A')]
  ranks1 = ranks[which(obs=='P')]
  U = n0*n1 + (n0*(n0+1))/2 - sum(ranks0) #calc U stat
  AUC = U/(n0*n1) #estimate AUC
  if (AUC<.5) AUC = 1-AUC
  aucvalues1 <- round(AUC, 3)
  aucd <- data.frame( auc= aucvalues1)#, dupmode= dupmode
  datamode <- do.call(rbind, aucd)

  return(datamode)
}



