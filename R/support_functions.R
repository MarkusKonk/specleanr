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
                               reason='open the R Shiny Application', quiet= TRUE){

  #check if suggested packages are installed and prompt the user to install them or not continue

  sgt <- sapply(listpkgs, require, character.only = TRUE, mask.ok =FALSE,
                warn.conflicts=FALSE, quietly=TRUE)

  if(all(sgt)==TRUE){
    if(isFALSE(quiet))message('All required packages are installed')
  }else{
    fl <- sgt[which(sgt==FALSE)]
      #trivial
      if(length(fl)==1) {
        pkg ="Package"
        isare = 'is'
      }else {
        pkg ="Packages"
        isare = "are"
      }
    if(isFALSE(quiet)) message(pkg, " ", paste(names(fl), collapse = ' ,'), " ", isare, " installed to ", reason, " .")

    pk <- sapply(names(fl), install.packages, quiet=TRUE, verbose=FALSE, repos = "http://cran.us.r-project.org")
  }

}



#' @title Customised match function
#'
#' @param x The category with words to match
#' @param choices The different options or choices in a particular category that are allowed.
#' @param quiet Default \code{FALSE} not to return messages.
#'
#' @return choices
#' @export
#'
match.argc <- function(x, choices, quiet=TRUE){

  if(any(choices%in%x)==FALSE){
    stop("The value for ", deparse(substitute(x)), " is not allowed. Choose from ", paste0(choices, collapse = ', '))
  } else{
    if(isFALSE(quiet))message("The ", deparse(substitute(x)), " is not among the allowed choices ", paste0(choices, collapse = ', '))
  }
}


check.exclude <- function(x, exclude, quiet=TRUE){

  xcnames <- colnames(x)

  indcols <- exclude%in%xcnames

  colsnotindf <- exclude[which(indcols==FALSE)]

  if(length(colsnotindf)>=1){

    stop("The column name/s: ", paste(colsnotindf, collapse = ', '), " to be excluded are/is not in the species-environment extracted data.")
  }else{

    if(isFALSE(quiet)) message("All indicated columns to be excluded are in the dataset.")
  }

}



