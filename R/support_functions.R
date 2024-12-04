
#' @noRd
#'
fishbase <- function(tables){

  if(!requireNamespace('rfishbase', quietly = TRUE))stop("Please install rfishbase package to continue.")

  if(!requireNamespace('curl', quietly = TRUE))stop('Please install package curl to continue.')

  if (!curl::has_internet()) stop('No internet connection, connect and try again later to access FishBase.')

  fb_sy <- suppressMessages(rfishbase::synonyms())

  if(nrow(fb_sy)<0) stop('The synonym table from rfishbase has not successfully loaded and names cannot be checked.')

  fb_ranges <- suppressMessages(rfishbase::stocks())

  if(nrow(fb_ranges)<0) stop('The stocks table from rfishbase has not successfully loaded and temperature/geogrpahical ranges and  cannot be determined.')

  fb_species <- suppressMessages(rfishbase::species())

  if(nrow(fb_species)<0)stop('The species table didnot load properly and species ecosystem cannot be extracted from fishbase.')

  switch(tables, synonym = return(fb_sy), ranges = return(fb_ranges), spnames = return(fb_species))
}


#' @noRd
clean_names <- function(sp){

  #convert all letters to lower
  tlw <- tolower(sp)
  #remove accents
  actr <- iconv(tlw, from = 'UTF-8', to = 'ASCII//TRANSLIT')

  sppt <- gsub("[[:punct:]]", "", actr)

  spc <- gsub("[^[:alnum:]]", " ", sppt)

  spaces <- trimws(gsub("\\s+"," " ,spc), which = 'both')

  str1 <- unlist(strsplit(spaces, " "))[1]

  strother <- paste0(unlist(strsplit(spaces, " "))[-1], collapse = ' ')

  spclean <- paste0(paste0(toupper(strtrim(str1, 1)), substring(str1, 2)),' ',strother)

  return(spclean)
}


#' @title Customized match function
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
