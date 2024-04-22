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

  suppressMessages(suppressWarnings(specleanr::suggested.packages(listpkgs =c('curl', 'rfishbase'), reason = 'to access FishBase data')))

  if (!curl::has_internet()) stop('No internet connection, connect and try again later to access FishBase.')

  fb_sy <- suppressMessages(rfishbase::synonyms())

  if(nrow(fb_sy)<0) stop('The synonym table from rfishbase has not successfully loaded and names cannot be checked.')

  fb_ranges <- suppressMessages(rfishbase::stocks())

  if(nrow(fb_ranges)<0) stop('The stocks table from rfishbase has not successfully loaded and temperature/geogrpahical ranges and  cannot be determined.')

  switch(tables, synonym = return(fb_sy), ranges = return(fb_ranges))
}

#' @title Check species names for inconsistencies
#'
#' @param data Data frame with species names to checked from FishBase.
#' @param colsp variable in data with the species column names.
#' @param verbose To indicate the merges during checking of names. The default is FALSE not to show whether the species are in Fish base or not found.
#' @param pct The percentage similarity used to assign a relatively similar name from FishBase if the exact match is absent. Default 90%.
#' The higher the values, the higher percentage similarity are considered to replace a species name that is cheeked from Fishbase.
#' @param merge Default is \strong{FALSE}, not to merge the cleaned species column on to the data frame but rather only two columns
#' returned.
#' @param sn whether to consider synonyms. Default FALSE so accepted names will be considered from
#' FishBase database.
#'
#' @details
#' The function produces a data set with species names corresponding with
#' \href{https://www.fishbase.se/}{Fishase}. If synoynm is provided in the data set, the function will by defualt
#' return the accepted name. However, if the synoymn is desired, then set the sn parameter to \strong{TRUE}.
#' The function also check for spellings of species names and returns a name that is closer to the one in FishBase
#' with a particular degree of similarity set with pct parameter. pct of 1 indicates the name must 100% similar.
#' The user can iterate with different pct and decide if the return name is right or wrong. This function is
#' not necessary if the species names are clean and also for other taxa.
#'
#'
#' @importFrom methods is
#'
#' @return Data frame or names of corrected or cleaned species names.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(jdsdata)
#'
#' data(efidata)
#'
#' #step 1. match and bind datasets if more than one datasets
#'
#' matchdata <- merge_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                           lats = 'lat',
#'                           lons = 'lon',
#'                           species = c('speciesname','scientificName'),
#'                           country=c('JDS4_site_ID'),
#'                           date=c('Date', 'sampling_date'))
#'
#' #clean species names to produce one dataset.
#'
#' datafull <- check_names(data= matchdata, colsp='species', pct = 90, merge = TRUE)
#'
#' data2col <- check_names(data = matchdata, colsp='species', pct = 90) #two columns generated
#'
#' cleansp_name <- check_names(data= 'slamo trutta', pct=90) #wrong names vs FB suggestion
#'
#' clean_sp_epithet <- check_names(data = 'Salmo trutta fario') #Salmo trutta will be returned
#'
#' speciesepithet2 <- check_names(data = 'Salmo trutta lacustris', pct=90, )
#'
#' }
#'
#' @seealso [match_datasets()] for standardizing and binding datasets.
#'
#'

check_names <- function(data, colsp = NULL, verbose=T, pct = 90, merge=F, sn=FALSE){

  if(missing(data)) stop('Data is not provided', call. = FALSE)

  if(is(data, 'data.frame') && nrow(data)<1)stop('Dataset provided has zero observations')

  if(is(data, 'data.frame') && is.null(colsp)) {

    stop('Species column names is not provided', call. = FALSE)

  } else if(is(data, 'data.frame') && !is.null(colsp)){

    if(length((colnames(data)[colnames(data)==colsp]))<1){

      stop('Species column name ', colsp, ' is  not found in the ', deparse(substitute(data)), ' data provided')

    } else{

      spls <- unlist(data[, colsp])

    }

  }else if(is(data, 'list')){

    spls <- unlist(data)

  }else if(is(data, 'vector') && length(data>1)){

    spls <- data

  }else if(is(data, 'vector') || is(data, 'atomic')) {

    spls <- data

  }else{
    stop('No data provided for species to check and merge')
  }

  #standard species list from FishBase

  fb_df <- fishbase(tables = 'synonym')

  spnames <- unlist(fb_df$synonym)

  status <- unlist(fb_df$Status)

  spcd <- unlist(fb_df$SpecCode)

  acceptednames_list <- spnames[which(status %in%c('accepted name','provisionally accepted name'))]#accepted names

  synoynm_list <- spnames[which(status !='accepted name')]#accepted names and others categories

  synoynmtrue <- spnames[which(status=='synonym')]#only synonymns

  spna <- replace(spls, grepl("^\\s*$", spls) == TRUE, NA)#if empty space are found in the list or column

  unx <- unique(spna)

  spl <- c()
  speciescheck <- c()
  species <- c()
  for (iii in seq_along(trimws(unx))) {

    species_clean = clean_names(sp = unx[iii])

    #1. clear species names

    if((species_clean%in%acceptednames_list)==TRUE){

      spp = species_clean

    }else if((species_clean%in%synoynm_list)==TRUE){ #handle synonyms

      if(isTRUE(sn)){ #either output with synonym species or replace with accepted names

        if(isTRUE(verbose)) message('The synoynm species ', species_clean, ' maintained in the list.')

        spp = species_clean

      }  else {

        s1 <- spcd[which(spnames == species_clean)]

        s2 <- spcd[which(status %in% c('accepted name', 'provisionally accepted name'))] #species codes for accepted names

        sx <- acceptednames_list[which((s2%in%s1)==TRUE)] #get species name or names that are accepted for a particular synonym

        if(length(sx)>1){ # if more than one

          s11 <- spcd[which(spnames == species_clean & status=='synonym')]

          s21 <- spcd[which(status %in% c('accepted name', 'provisionally accepted name'))] #species codes for accepted names

          spp <- acceptednames_list[which((s21%in%s11)==TRUE)]

          if(isTRUE(verbose)) message('The synoynm ', species_clean, ' will be replaced with ',spp,' based on high synonym simialrity.')

        }else{

          spp <- sx

          if(isTRUE(verbose)) message('The synoynm ', species_clean, ' will be replaced with ',spp,'.')

        }

      }

    } else{

      #handle morph types by only considering species scientific epithet
      txt = scan(text = species_clean, what = ' ', quiet = T)

      sp_ex = paste0(txt[1],' ',txt[2])

      if((sp_ex %in%acceptednames_list)==TRUE){#check in accepted names

        spp = sp_ex

        if(isTRUE(verbose)==TRUE) message(species_clean, ' will be replaced with ', spp)

      }else if((sp_ex%in%synoynm_list)==TRUE){ #handle synonyms

        if(isTRUE(sn)){ #either output with synonym species or replace with accepted names

          if(isTRUE(verbose)) message('The synoynm species ', species_clean, ' maintained in the list.')

          spp = sp_ex

        }  else {

          s111 <- spcd[which(spnames == sp_ex)]

          s211 <- spcd[which(status=='accepted name')] #species codes for accepted names

          sx11 <- acceptednames_list[which((s211%in%s111)==TRUE)] #get species name or names that are accepted for a particular synonym

          if(length(sx11)>1){ # if more than one

            siv <- spcd[which(spnames == sp_ex & status=='synonym')]

            s2v <- spcd[which(status %in% c('accepted name', 'provisionally accepted name'))] #species codes for accepted names

            spp <- acceptednames_list[which((s2v%in%siv)==TRUE)]


            if(isTRUE(verbose)) message('The synoynm ', sp_ex, ' will be replaced with ',spp,' based on high synonym simialrity.')

          }else{

            spp <- sx

            if(isTRUE(verbose)) message('The synoynm ', sp_ex, ' will be replaced with ',spp,'.')

          }
        }

      } else if ((sp_ex %in%c(acceptednames_list, synoynm_list))==FALSE) {#spelling mistakes

        #use name similarities to infer names from FishBase

        #dissimilarity between the names local vs FB

        dst = utils::adist(sp_ex, acceptednames_list)

        sp_prob = acceptednames_list[which(dst==min(dst))]

        if(length(sp_prob)>1){

          sp_prob_sel = sp_prob[1]

        }  else{

          sp_prob_sel = sp_prob
        }

        nc = nchar(sp_prob_sel)

        pcts = (100-(min(dst)/nc)*100)

        #shortest scientific name is ia io
        if(pcts>=pct){

          spp = sp_prob_sel

          if(isTRUE(verbose)==TRUE) message(species_clean, ' replaced with ', spp, ' from FishBase with name percentage simialirity of ', round(pcts, 1), '%')
        }  else{

          spp = NA

          if(isTRUE(verbose)==TRUE) message('No close name in Fish Base for ', species_clean,'.')
        }

      } else{
        spp = NA

        if(isTRUE(verbose)==TRUE) message(species_clean, ' not found in Fishbase.')

      }
    }

    spl[iii] <- species_clean
    species[iii] <- unx[iii]
    speciescheck[iii] <- spp
    df_sp <- data.frame(species = species, speciescheck = speciescheck)
  }
  if(is(data, 'data.frame') && isTRUE(merge)){

    #rename standard out df to include the user entered column name in the main dataset
    names(df_sp)[1] <- colsp

    dfinal <- merge(data, df_sp, by=colsp)

    return(dfinal)

  }else if(is(data, 'data.frame')|| is(data,'list') && isFALSE(merge)){

    return(df_sp)

  }else if(is(data, 'vector') && length(data)>1 && isFALSE(merge)){

    return(df_sp)

  }else{
    return(spp)
  }

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







