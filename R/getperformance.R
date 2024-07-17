#' @title Extract the model performance
#'
#' @param modelcomp \code{list}. Model comparison output object where data is extracted.
#' @param type \code{character}. Either test or train performance metrics.
#'
#' @return A data frame with performance metrics between reference and outlier cleaned dataset. This function takes the output from \code{\link{modelcomparison}} function.
#'
#' @export
#'
get_performance <- function(modelcomp, type='test'){

  #check if all objects have data-based on classes returned
  objdata <- sapply(modelcomp, function(x) class(sapply(x, is.na)))

  #check for single species
  if(length(modelcomp)==1){

    if(('logical'%in%objdata)==FALSE){
      retained <- modelcomp
    }else{

      stop("All model comparison objects did not return any data. Records are few to compute to model comparisons.")
    }
  }else{
    #remove logical # since no data was returned
    if("logical"%in%objdata==TRUE) {
      xdata <- objdata[!objdata=='logical']
      retained <- modelcomp[(names(modelcomp)%in%names(xdata)) ==TRUE]
    }else{
        retained <- modelcomp
      }
  }
  perfdata <- list(); splist <- list(); runlist<- list();   pdata <- list()

  #run through the species
  for (nm in 1:length(retained)) {

    #extract species name
    if(is.null(names(retained)[nm])) spname <-  1 else spname <- names(retained)[nm]

    spd = retained[[spname]]

    #run through the thresholds from REF to 0.9..
    for(ref in 1:length(spd)){

      refnames <- names(spd)[ref] #the thresholds

      modedata <- spd[[refnames]]

      if(length(modedata)>1) modedata else next

      for(r in 1:length(modedata)){

        rundata <- modedata[[r]] #check if all thresholds executed

        if(attributes(modelcomp)$full==FALSE){

          pdata[[r]]  <- switch (type, test = rundata[[1]], train=rundata[[2]] )

        }else{
          pdata[[r]]  <- switch (type, test = rundata[[4]], train=rundata[[5]] )

        }

        runlist <- do.call(rbind, pdata)

        runlist$species = spname

        runlist$scenario = refnames
      }
      #save data which has been extracted
      perfdata[[ref]] <- runlist

      pfinal <- do.call(rbind, perfdata)

    }
    splist[[nm]] <- pfinal

    runfinal <- do.call(rbind, splist)
  }
  return(runfinal)
}
