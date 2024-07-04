#_______________________________

#Data handling function, including cleaning and checking dates and species names
#_______________________________

#' @title Internal function to clean names before checking them with fishbase
#'
#' @param sp species names
#'
#'
#' @return corrected species
#'
#' @examples
#'
#' \dontrun{
#'
#' speciesname <- clean_names(sp='salmo trutta??')
#'
#' }
#'
#'
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


#Clean date before being fed into check dates fucntion

#' @title Internal function to clean dates before checking for consitencies
#'
#' @param x Date to to be cleaned
#'
#' @return cleaned date
#'
#' @examples
#'
#' \dontrun{
#'
#' date <- clean_date(x = '12 january 2010')
#'
#' }
#'
#'
clean_date <- function(x){

  dt = gsub(pattern = '\\s', replacement = '', x=x)

  mts <- c('jan|january', 'feb|february','mar|march', 'apr|april','may', 'jun|june','jul|july', 'aug|august',
           'sep|september|sept', 'oct|october','nov|november', 'dec|december')
  num_mon <- c(paste0(0, seq(1,9)), seq(10, 12))

  pttn <- c()
  for (mi in 1:length(mts)) {
    chks <- grepl(mts[mi], x=dt, ignore.case = TRUE)

    if(chks==TRUE) {
      pttn[mi] = mts[mi]

    }else{
      pttn[mi] <- NA
      'No character month in the data'
    }
  }

  pt_out <- mts[which(!is.na(pttn))]

  if(length(pt_out)>0){

    dst1 <- adist(pt_out, mts,ignore.case = T)

    idx= which(dst1==min(dst1))

    monthptn <- mts[idx]

    numrepl <-  num_mon[idx]

    dt <- gsub(monthptn, replacement = numrepl, x=dt,ignore.case = TRUE)
  }else{
    dt
  }
  return(dt)
}


#' @title Collates synoynm data tables from FishBase.
#'
#' @param tables Tables from FishBase
#'
#' @return Two datasets
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' stdf <- fishbase(tables = 'synonym')
#'
#' }
#'
fishbase <- function(tables){

  suppressMessages(suppressWarnings(suggested.packages(listpkgs =c('curl', 'rfishbase'), reason = 'to access FishBase data')))

  if (!curl::has_internet()) stop('No internet connection, connect and try again later to access FishBase.')

  fb_sy <- suppressMessages(rfishbase::synonyms())

  if(nrow(fb_sy)<0) stop('The synonym table from rfishbase has not successfully loaded and names cannot be checked.')

  fb_ranges <- suppressMessages(rfishbase::stocks())

  if(nrow(fb_ranges)<0) stop('The stocks table from rfishbase has not successfully loaded and temperature/geogrpahical ranges and  cannot be determined.')

  switch(tables, synonym = return(fb_sy), ranges = return(fb_ranges))
}



#' @title Harmonise dates form different data sets
#'
#' @param x vector or list of dates
#' @param month position of the month, if a date of 03/04/2020. Indicate if the month is 04,
#' therefore month = 'middle' or month= 'lastfirst' if the month is 03
#'
#' @importFrom utils adist
#' @importFrom utils data
#'
#' @return Standardised dates
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(efidata)
#'
#' efidates <- check_dates(x= efiplus$Date, month ='middle')
#'
#' }
#'
check_dates <- function(x, month=NULL){

  match.arg(month, choices = c('middle', 'lastfirst'))

  dfstandard <- specleanr::dateformats

  fmts <- dfstandard$specifier

  exdates <- dfstandard$ex

  dates <- unique(x)

  dt_out <- c()

  for (idt in 1:length(dates)) {

    dt <- clean_date(dates[idt])

    lenc <- sapply(exdates, nchar)

    eqlen <- exdates[lenc==nchar(dt)]

    fmtseq <- fmts[lenc==nchar(dt)]

    #check sperator

    gen_sep =stringr::str_extract(eqlen, pattern = '[/.-]')

    dt_sep <- stringr::str_extract(dt, pattern = '[/.-]')

    spdates <- eqlen[which(gen_sep==dt_sep)]

    spfmts <- fmtseq[which(gen_sep==dt_sep)]

    dst <- adist(dt, spdates)

    minv <- which(dst== min(dst))

    if(length(minv)>1){

      dtl <- c()
      fmts_filt <- c()
      for (idt_i in 1:length(minv)) {

        ed_out_min <- spfmts[minv][idt_i]


        dtl[idt_i] <- format(as.Date(dt , format = ed_out_min))

        fmts_filt[idt_i] <- ed_out_min
      }

      if(all(is.na(dtl))){

        #warning('Wrong date entered and NA will be produced', call. = FALSE)

        dt_out[idt] <- NA

      } else if(identical(dtl[1], dtl[2]) || all(!is.na(dtl))){

        #message('varrying dates obtained and one selected')

        chk <- grepl('(^%m|%m$)', x=fmts_filt)

        fmtnid <- fmts_filt[which(chk==TRUE)]

        fmtmd <- fmts_filt[which(chk==FALSE)]


        dt_out[idt] = switch(month, middle= format(as.Date(dt , format = fmtmd)),
                             lastfirst = format(as.Date(dt , format = fmtnid)))

      }else if(any(is.na(dtl))){

        #print('One selected')

        dt_out[idt] <- dtl[which(!is.na(dtl))]

      }else{
        #message('Failed to choose the date')
      }

    }else{

      message('Only one date format identifed ')

      dt_out[idt] <- format(as.Date(dt, fmts[minv]))
    }

  }
  return(as.Date(dt_out))
}







