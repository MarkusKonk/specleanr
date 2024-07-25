
#' @title Plot to the performance metrics across the thresholds used in model comparison
#'
#' @param modelout \code{list}. The model comparison output obtained from the \code{modelcomaprison} function.
#' @param eval \code{vector}. The model evaluation metrics. Check \code{\link{indep}} and \code{\link{dep}} for details.
#' @param type \code{string}. Either test data output \code{test} or training data output \code{train}. Default \code{test}.
#' @param scales \code{string}. options to for facets scales based on ggplot2 object. Allowed \code{free_y, free_x, free}.
#' @param scolor \code{vector}. Two color options for Cleaned and Reference categories.
#' @param FUN a function to summarize the performance metrics. Default \code{mean}.
#'
#' @return ggplot2 output with performance metrics such as accuracy, sensitivity, specificity..
#'
#' @importFrom utils stack
#'
#' @export
#'
#'
ggperform <- function(modelout, eval=c('auc', 'Accuracy'), type = 'test',
                      scales="free", FUN='mean', scolor =c('grey75', 'grey50')){

  #Get the performance values form the model comparison output
  perfdata <- get_performance(modelcomp  = modelout, type = type)

  #Stack the data to elongate for effective filtering and plotting
  #eval to select out metrics that are needed
  stackdata <- cbind(perfdata[, c("species", "scenario")], stack(perfdata[,eval]))

  #Change scenario  names as Reference for REF and all other as Cleaned
  stackdata['scenario'] <- ifelse(stackdata$scenario != 'REF', "Cleaned" , "Reference")

  stackdata['ind2'] <- ifelse(stackdata$ind == 'auc', "AUC" ,as.character(stackdata$ind))

  #Get a summary out for plotting#
  #get a general picture if the species are more than 1

 # meanperf <- aggregate(values~ind2+ scenario, data = stackdata, FUN = noquote(FUN))

  xlab <- paste0(FUN, " performance")


  #Plotting the summary using ggplot2

  ind2 <- NULL; values = NULL; scenario = NULL
  gplot <- ggplot2::ggplot(data = stackdata, ggplot2::aes(x = ind2, y = values, fill=scenario))+
    ggplot2::stat_boxplot(geom = 'errorbar', width = 0.3, position = ggplot2::position_dodge(width = 1))+

    ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 1))+

    ggplot2::scale_fill_manual(values = scolor)+
    {if(length(unique(stackdata$species))>1)ggplot2::facet_wrap(~ species,scales = scales)}+
   ggplot2::labs(y =xlab, x="Thresholds")

  return(gplot)
}
