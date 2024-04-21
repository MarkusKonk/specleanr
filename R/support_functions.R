.abspath <- function(dir, verbose=TRUE){

  gwd <- getwd()

  folder <- paste0(gwd,'/',dir)

  if(dir.exists(folder)==FALSE){

    if(isTRUE(verbose)==TRUE) message('New directory ', dir, ' formed')

    dir.create(dir)

    pathabs= paste0(gwd, '/', dir)

  }else{
    if(isTRUE(verbose)==TRUE)message(dir, ' already present')
    pathabs= folder
  }

  return(pathabs)
}


.absx <- function(x, var){#x is the absolute path from .abspath function

  folder <- paste0(x,'/',var)

  if(dir.exists(folder)==FALSE){

    dir.create(folder)

    px= paste0(x,'/',var)
  }else{
    px= folder
  }

  return(px)
}



#' @title  caching data
#'
#' @param x to indicate absolute path.
#'
#' @importFrom memoise memoise
#'

.cache <- function(x){

  abpath <- .abspath(x, verbose = F)

  d <- memoise::cache_filesystem(abpath)

  return(d)
}

.mem_files <- function(fn, path){

  cdx <- .cache(x=path)

  mfn <- memoise::memoise(f = fn, cache = cdx) # memoise

  outdata <- mfn(x = path)#takes the input of the function

  return(outdata)
}

mode <- function(x, bin = NULL){

  if(inherits(x, 'character')){
    xm <- table(x)
    m <- x[which(xm==max(xm))]

  }else if(inherits(x, 'numeric')){
    xm <- table(sort(x))

    m <- x[which(xm==max(xm))]

    if(length(m)>1) message('the vector is bimodal..')
  }

  return(m)
}



#' @title Check for suggested packages.
#'
#' @param listpkgs Packages to be included.
#' @param reason Reason why packages needs to be installed.
#' @param quiet Default TRUE, for no messages.
#'
#' @importFrom utils install.packages
#'
#' @return Packages
#' @export
#'
suggested.packages <- function(listpkgs=c("shiny", "shinydashboard", "DT", "dplyr"),
                               reason='Opening R Shiny Application', quiet=FALSE){

  #check if suggested packages are installed and prompt the user to install them or not continue

  sgt <- sapply(listpkgs, require, character.only = TRUE, mask.ok =FALSE,
                warn.conflicts=FALSE, quietly=TRUE)

  if(all(sgt)==TRUE){
    if(isTRUE(quiet))message('All required messages are installed')
  }else{
    cat("Enter 1:  install needed packages \nEnter 0:  to abort\n")
    opt<-readline(prompt ="Enter option: ")

    if(opt=="1"){

      fl <- sgt[which(sgt==FALSE)]

      pk <- sapply(names(fl), install.packages, quiet=TRUE, verbose=FALSE)

    }else if(opt=='0'){
      fl <- sgt[which(sgt==FALSE)]

      #trivial
      if(length(fl)==1) {
        pkg ="package"
        isare = 'is'
      }else {
        pkg ="packages"
        isare = "are"
      }

      stop(reason, " cannot continue since the neccesary", " ", pkg, " ", paste(names(fl), collapse = ' ,'), " ", isare, " not installed.")
    }else{
      stop('Please input either 1 to continue or 0 to abort the function')
    }
  }

}



