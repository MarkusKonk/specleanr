#' Title
#'
#' @param data
#' @param outliers
#' @param models
#' @param threshold
#' @param species
#' @param var
#' @param exclude
#' @param colsp
#' @param raster
#' @param mode
#' @param nboots
#' @param testprop
#' @param coords
#' @param prbackground
#' @param positive
#'
#' @return
#' @export
#'
#' @examples
modelperform <- function(data, outliers, models = 'GLM', threshold =0.6, species, var, exclude = NULL, colsp = NULL,
                         raster = worldclimfinal, mode = c('Reference', 'threshold60'), nboots= 10,
                         testprop=0.2, coords =c('x', 'y'), prbackground=1, positive = 'P'){

  perfdf <- list()
  modelist <- list()
  modelout <- list()

  for (spl in species) {

    #spp <- species[spl]

    for (p in 1:length(mode)){

      mdl = mode[p]

      if(is(data, "data.frame")){
        data <- split(data, f = data[, colsp])
      }else{
        data
      }

      if(mdl=='Reference'){
        dfspx <- data[[spl]]

      }else if(mdl=='threshold60'){

        dfspx <- tryCatch(expr =  extract_clean(data = data, outliers = outliers, sp=spl, var = var,
                                                threshold = threshold), error = function(e) e)

        if(inherits(dfspx, 'error')){
          message('No absolute outliers at a threshold of ', threshold,', so data is similar to reference dataset.' )
          next
          print(nrow(dfspx))
        }else{
          dfspx
        }

      }else{
        stop('Mode not recorganised.')
      }

      dfdata <- dfspx[,!colnames(dfspx) %in% exclude]

      bgd = gen_background(occurences = dfdata, raster = raster, coords = coords,prop = prbackground )#prop of background data to presences

      bdslt <- bgd[,c(1,2)]

      bfinaldf<- terra::extract(x = worldclimfinal , y=bdslt, ID=FALSE)

      bcordf <- usdm::vifcor(bfinaldf, th=0.7)

      exdf<- usdm::exclude(bfinaldf, bcordf)

      df_final <- cbind(exdf, species= bgd[,3])

      df_final$species <- as.factor(df_final$species)

      btdata <- boots(data = df_final, nboots = nboots, testprob = testprop)

      modelout[[p]] <- sdmfit(y=species, data=btdata, positive = positive, models = models)

      names(modelout)[p]  <- mdl
    }
    modelist[[spl]] = modelout#checkdata

  }
  return( modelist)
}

#' Title
#'
#' @param modeloutput
#' @param type
#'
#' @return
#' @export
#'
#' @examples

extract_performance <- function(modeloutput, type='test'){

  match.arg(type, choices = c('train', 'test'))

  perfdata <- list()
  splist <- list()

  for (nm in 1:length(modeloutput)) {

    spname = names(modeloutput)[nm]

    spd = modeloutput[[spname]]

    for(ref in 1:length(spd)){

      refnames <- names(spd)[ref]

      modedata <- spd[[refnames]]

      pdata  <- switch (type, test = modedata[[4]], train=modedata[[5]] )

      perfdata[[ref]] <- do.call(rbind, pdata)

      perfdata[[ref]]$species = spname

      perfdata[[ref]]$scenario = refnames


      runsdata <- do.call(rbind, perfdata)

    }
    splist[[nm]] <- runsdata

    runfinal <- do.call(rbind, splist)

  }
  return(runfinal)
}



