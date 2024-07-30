
#' @noRd
fishbase <- function(tables){

  suppressMessages(suppressWarnings(suggested.packages(listpkgs =c('curl', 'rfishbase'), reason = 'to access FishBase data')))

  if (!curl::has_internet()) stop('No internet connection, connect and try again later to access FishBase.')

  fb_sy <- suppressMessages(rfishbase::synonyms())

  if(nrow(fb_sy)<0) stop('The synonym table from rfishbase has not successfully loaded and names cannot be checked.')

  fb_ranges <- suppressMessages(rfishbase::stocks())

  if(nrow(fb_ranges)<0) stop('The stocks table from rfishbase has not successfully loaded and temperature/geogrpahical ranges and  cannot be determined.')

  switch(tables, synonym = return(fb_sy), ranges = return(fb_ranges))
}



#' @noRd
clean_names <- function(sp){

  suppressWarnings(specleanr::suggested.packages(c('stringr'), reason='Strings'))

  #convert all letters to lower
  tlw <- tolower(sp)
  #remove accents
  actr <- iconv(tlw, from = 'UTF-8', to = 'ASCII//TRANSLIT')

  sppt <- gsub("[[:punct:]]", "", actr)

  spc <- gsub("[^[:alnum:]]", " ", sppt)

  spaces <- trimws(gsub("\\s+"," " ,spc), which = 'both')

  spclean <- stringr::str_to_sentence(spaces)

  return(spclean)
}

#' @title obtain absolute path for the user
#'
#' @param dir to user the user directory to save data for future use.
#' @param verbose to show messages during implementation or not. Default \code{FALSE}.
#'
#' @return absolute path
#'
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


#' @title create sub folders in the absolute path folder
#'
#' @param x is the absolute path set in .abspath
#' @param var is the name of the folder with specific variables.
#'
#' @return subfolder in the absolute path

.absx <- function(x, var){#x is the absolute path from .abspath function

  folder <- paste0(x,'/',var)

  #if the folder doesn't exist, then a new one will be created to store data.

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
#'
.cache <- function(x){

  #to allow caching the data in the particular folder.

  abpath <- .abspath(x, verbose = F)

  d <- memoise::cache_filesystem(abpath)

  return(d)
}

#' @noRd
.mem_files <- function(fn, path){

  cdx <- .cache(x=path)

  #to keep the data from the particular function in the user pc to avoid multiple downloads.
  mfn <- memoise::memoise(f = fn, cache = cdx) # memoise

  outdata <- mfn(x = path)#takes the input of the function

  return(outdata)
}


#' @title Check for suggested packages.
#'
#' @param listpkgs A \code{list} of packages to be suggested.
#' @param reason A \code{string} of character to describe the reason why packages are suggested.
#' @param quiet \code{logical} Default TRUE, for no messages.
#'
#' @importFrom utils install.packages
#'
#' @return install suggested packages
#'
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

    sapply(names(fl), install.packages, quiet=TRUE, verbose=FALSE, repos = "http://cran.us.r-project.org")
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


#' @title indicate excluded columns.
#'
#' @param x \code{dataframe} with columns to where the columns are supposed to be removed.
#' @param exclude \code{string} or \code{vector} column names  to be checked if it is in the data.
#' @param quiet TRUE if implementation messages to be shown. Default \code{FALSE}.
#'
#' @return columns that are not in the dataframe.
#'
check.exclude <- function(x, exclude, quiet=TRUE){

  indcols <- exclude%in%colnames(x)

  colsnotindf <- exclude[which(indcols==FALSE)]

  if(length(colsnotindf)>=1){

    stop("The column name/s: ", paste(colsnotindf, collapse = ', '), " to be excluded are/is not in the species-environment extracted data.")
  }else{

    if(isFALSE(quiet)) message("All indicated columns to be excluded are in the dataset.")
  }

}

#' @title get dataframe from the large dataframe.
#'
#' @param x Small dataset
#' @param y Large dataset for intersection
#' @param full Whether the whole column names are checked or not. Default \code{FALSE} where only the first column is considered.
#'      if FALSE; then the returned columns may be few or more if the considered column has less or more similar
#'      rows across the two data sets.
#'
#' @return Data to extracted from large dataset.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' x = data.frame(id=c(1,2,3,4,5),  name=c('a','b','c', 'd','e'))
#'
#' y=data.frame(id=c(1,2,3,4,7,6,5), tens=c(10,29,37,46,58, 34, 44),
#'                  name=c('a','b','c','d','e', 'f','g'))
#'
#' }
getdiff <- function(x, y, full=FALSE){

  c1 <- colnames(x); c2 <- colnames(y)

  if(all(c1 %in% c2)==FALSE) stop("All column names are different.")

  if(identical(x, y))stop("x and y names are identical.")

  if(full==FALSE){

    #use one column name same across the two datasets
    getcol <- c1[which(c1%in%c2==TRUE)][1]

    out <- y[which(y[,getcol] %in% x[,getcol]),]

  }else{

    getcol <- c1[which(c1%in%c2==TRUE)]

    #loop through all same data sets names and extract same rows across the two data sets.

    xx <- sapply(getcol, function(cl){ y[which(y[,cl] %in% x[,cl]),] }, simplify = FALSE)

    rws <- sapply(xx, nrow)

    out <- xx[[which(rws==min(rws))]]
  }
  return(out)
}
