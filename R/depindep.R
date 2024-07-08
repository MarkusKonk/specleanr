#' @title Threshold dependent parameters for species distribution model evaluations.
#'
#' @param observed The observed occurrences of the species.
#' @param predicted The fitted model obtained from \code{sdmfit}
#' @param P Character for presence label. Important to indicate the presence/absence character to correctly compute the evaluation metric in \code{evaluate} function.
#' @param A Character for absence label. Important to indicate the presence/absence character to correctly compute the evaluation metric in \code{evaluate} function.
#'
#' @details
#' These are threshold-dependent metrics which vary if the cutoff is changed. The default is 0.5 which indicates that probabilities greater than 0.5 are recorded as
#' the species is present and less than 0.5 indicates absence of a species. However, if the cutoff is increased, probabilities converted as positive reduce and also the
#' model evaluation metrics such as sensitivity, specificity or accuracy changes significantly. Different literature is used to compute the metrics including
#' \strong{\code{(Allouche et al. 2006)}} and \strong{\code{(Erickson & Kitamura 2021)}}.
#'
#'
#' @return A list of threshold dependent metrics after models evaluation.
#' @export
#'
#' @references
#'\enumerate{
#'  \item Allouche O, Tsoar A, Kadmon R. 2006. Assessing the accuracy of species distribution models: Prevalence,
#'  kappa and the true skill statistic (TSS). Journal of Applied Ecology 43:1221-1232.
#'  \item Erickson BJ, Kitamura F. 2021. Magician's corner: 9. performance metrics for machine learning models.
#'  Radiology: Artificial Intelligence 3:1-7.
#'  }
#'
#' @seealso \code{\link{sdmfit}}
#'
#'
dep <- function(observed, predicted, P, A){

  tab <- table(observed = observed, predicted = predicted)

  if(length(tab)>2){
    tab <- tab
  }else{
    lv <- sort(union(observed,  predicted))

    tab <- table(factor(observed,levels = lv), factor(predicted , levels = lv))
  }

  #create confusion matrix table

  tp = tab[P,P]; fn=tab[P,A]; fp= tab[A,P]; tn = tab[A,A]


  total           =       tp+tn+fp+fn
  accuracy        =       (tp + tn) / (tp+fn+fp+tn)
  sensitivity     =       tp/(tp+fn)
  specificity     =       tn/(fp+tn)
  precision       =       tp/(tp + fp)
  ppv             =       precision
  npv             =       tn/(tn+fn)
  detection_rate  =       tp/(tp+fn)
  f1score         =       2*tp/(2*tp + fp + fn)
  prevalence      =       (fn+tp)/(tn+fp+fn+tp)
  balancedaccuracy=       (sensitivity + specificity) / 2
  FDR             =       fp/(fp+tp)
  recall          =       tp / (tp + fn)
  tss             =       (sensitivity+specificity)-1
  misRate         =       1-accuracy
  po              =       accuracy
  pcorrect        =       ((tp+fp)/total)*((fn+tp)/total)
  pincorrect      =       ((fn+tn)/total)*((fp+tn)/total)
  pe              =       pcorrect+pincorrect
  kappa           =       (po-pe)/(1-pe)

  output <- data.frame(list(Accuracy=accuracy, Sensitivity = sensitivity,
                            Specificity= specificity,TSS= tss, Kappa = kappa,
                            PPV= ppv,NPV = npv,Precision= precision,Recall =recall,
                            F1=f1score, Prevalence= prevalence,
                            DetectionRate = detection_rate,
                            FDR = FDR,BalancedAccuracy = balancedaccuracy,
                            MisclassificationRate = misRate))
  return(output)
}


#' threshold independent evaluation
#'
#' @param probs Model probabilities after predictions.
#' @param observed The observed probabilities.
#' @param P Character for presence label. Important to indicate the presence/absence character to correctly compute the evaluation metric in \code{evaluate} function.
#' @param A Character for absence label. Important to indicate the presence/absence character to correctly compute the evaluation metric in \code{evaluate} function.
#'
#' @details
#' The threshold-independent evalation parameter does not dependent on the cutoff to classify probability if a species is present or not.
#' In this package we incorporated the Area Under Receiver Operating Curve based on \strong{Hanley & McNeil Barbra (1982)}.
#'
#'
#' @importFrom stats wilcox.test
#'
#' @return Area Under the Curve values
#'
#'
#' @references
#' \enumerate{
#'   \item Hanley J. A, McNeil Barbra J. 1982. The meaning and use of the Area under
#'   a Receiver Operating Characteristic (ROC) Curve. Radiology 143:29-36.
#'   }
#'
indep <- function(probs, observed, P, A){

  absences <- probs[which(observed == A)]
  presence <- probs[which(observed == P)]

 #catch the errors with th wilxcon test especially when the records are few
  W <-  tryCatch(expr = unname(suppressWarnings(wilcox.test(presence, absences))$statistic),
                 error = function(e) e)

  if(inherits(W, 'error')) output = NA else output <- W/(length(presence)*length(absences))

  return(output)
}


