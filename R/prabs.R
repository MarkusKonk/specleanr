
#' @title  An internal fucntion to check for missing value in environmental data before estraction.
#'
#' @param raster A raster layer with species environmental data to be used during modelling.
#' @param coords the coordinates geometry. Either x and y or geometry column if the species occurrences are in a geometry format.
#' @param labels If the dataset has presence absence column, it should be indicated here to be used for extracting the labels.
#'        Character labels for example allowed, for example P for presences and A for absences recommended.
#' @param missingness The limit at which a column is discarded from the data rather than removing numerous rows. For example, if the column has 50\% of the dataset missing values
#'        then removing the rows will be problematic rather than column since it only affect one parameter than the whole dataset.
#' @param exclude Indicate columns where checking non missing values or NAs should not be done.
#' @param vifcutoff Used in assessing multicolinearity in environmental predictors using correlation from vifcor function from usdm package \code{Naimi et al., 2014}.
#' @param verbose To return execution messages or not. The Default is F or FALSE or 0.
#'
#' @return extracted environmental data with labels.
#'
#' @export
#'
#' @references
#'
#' \enumerate{
#'   \item Naimi, B., Hamm, N. A., Groen, T. A., Skidmore, A. K., & Toxopeus, A. G. (2014).
#'   Where is positional uncertainty a problem for species distribution modelling?. Ecography, 37(2), 191-203.
#' }

exdata <- function(raster, coords, labels, missingness, exclude, vifcutoff,
                   verbose){

  extractdf<- terra::extract(x = raster, y= coords, ID=FALSE)

  dataextract <- cbind(extractdf, y= labels)#merge it with the labels before na.omit

  #check extracted data for missing values

  pct <- apply(dataextract, 2, function(m) sum(is.na(m))/length(m))

  #the maximum missing in a column to drop i.e, if exceeds 10% defualt

  if(all(pct<missingness)) cleandata <- dataextract else cleandata <- dataextract[, -which(pct>missingness)]

  if(!is.null(exclude)){

    clean2 <- na.omit(cleandata[,!colnames(cleandata) %in% exclude])

  }else{
    clean2 <- na.omit(cleandata)

  }

  # #Remove non numeric columns from data
  clean3 <- clean2[, which(sapply(clean2, class) =='numeric')]


  cordf <- usdm::vifcor(clean3, th=vifcutoff)


  extractdata<- usdm::exclude(clean3, cordf)


  #combine the data with the labels

  spdata <- cbind(extractdata, y= clean2$y)

  return(spdata)
}


#' @title Generating pseudo absences from raster layers for distribution modeling.
#'
#' @param occurences Species occurrences data with coordinates or geometry to enable generate pseudo absences and extract environmental data from the points.
#' @param raster A raster layer with species environmental data to be used during modelling. Different sources, include WORLDCLIM
#' @param geom Is used in data extraction when the species occurrences geometry column instead of latitude and longitude.
#' @param lat,lon If the species occurrences don't have the geometry column or not spatial vector file, the latitude and longitude must be provided for data extraction form the raster layers.
#' @param labels If the dataset has presence absence column, it should be indicated here to be used for extracting the labels.
#'        Character labels for example allowed, for example P for presences and A for absences recommended.
#' @param prop The proportion of pseudo absences to presences. Default of 1 is used. Therefore equal number of pseudo absences are generated commensurate to the species.
#' @param missingness Allowed missing values in a column to allow a user decide whether to remove the individual columns or rows from the data sets. Default 0.1. Therefore, if
#'      if a column has more than 10\% missing values, then it will be removed from the dataset rather than the rows.
#' @param mode Either \code{presenceonly} if the species dataset do not have label column for presence absence. Therefore, the \code{presenceonly} is used
#'        when the parameter \code{label} is NULL. If only species presences and absences are indicated, the user should select \code{presenceabsence} option.
#' @param positive if \code{presenceabsence} is used, the user should indicate the positive label. For example, P for presence label. This is important in fitting the
#'        models and computing the evaluation metrics.
#' @param vifcutoff Used in assessing multicolinearity in environmental predictors using correlation from vifcor function from usdm package \code{Naimi et al., 2014}.
#' @param verbose To return execution messages or not. The Default is F or FALSE or 0.
#' @param set.seed to ensure reproduciblity the seed is set. Same psudoabsences will be produced during each run.
#' @param exclude Remove non numeric variables from the data.
#'
#' @importFrom sf st_crs
#' @importFrom terra crs
#'
#' @return  Labeled presence absence data.
#'
#'
#' @export
#'
#' @examples
#'
#' @references
#'
#' \enumerate{
#'   \item Naimi, B., Hamm, N. A., Groen, T. A., Skidmore, A. K., & Toxopeus, A. G. (2014).
#'   Where is positional uncertainty a problem for species distribution modelling?. Ecography, 37(2), 191-203.
#' }
#'
 ##@seealso [usdm::vifcor()]

envextract <- function(occurences, raster, lat =NULL, lon = NULL, geom = FALSE,
                       mode = 'presenceonly', labels=NULL, prop = 0.8, set.seed=1124, positive=NULL,
                       missingness = 0.1, exclude=NULL, verbose=TRUE, vifcutoff = 0.7){

  match.arg(mode, choices = c('presenceonly', 'presenceabsence'))

  if(missing(occurences)) stop('Provide species presence or presence/absence data')

  if(prop<0.1||prop>1)stop('Prop should be between 0.1 (10% generated) and 1 (equal to length of presence)')

  if(mode=='presenceonly' && is.null(labels)){

    if(!is(raster, 'SpatRaster')) stop('Only raster of SpatRater format is accepted.')

    #check if the coords parameter has valid columns

    if(inherits(occurences, 'sf')){

      if(isFALSE(geom)){

        stop('The species occurences are in sf format, set the `geom = "geometry"')

      }else if(all('geometry' %in% colnames(occurences))==FALSE) {

        stop('The coords parameter must be foun itn the species occurence data.')

      }else if(st_crs(occurences)[[1]]!= crs(raster, parse=FALSE, describe = TRUE)[1,1]){

        stop('Both species and environmental data are in a different cordinate reference system.')
      }else{
        px <- occurences[,'geometry']
      }
    }else if(!inherits(occurences, 'sf') && is(occurences, 'data.frame')){

      if(is.null(lat) || is.null(lon)){
        stop('For no sf format, the latitude and longitude should be provided.')

      } else if(all(max(occurences[,lat])>90|| min(occurences[,lat])<(-90))==TRUE){

        stop('Latitude should range from 90  to -90')

      } else if(all(max(occurences[,lon])>180|| min(occurences[,lon])<(-180))==TRUE){
        stop('Longitude should range from 180 to -180')
      }else{
        px <- occurences[,c(lon, lat)]
      }

    }else{
      stop('Only sf or data.frame are accepted for species occurences data.')
    }

    px$pa <- 'P'

    ppn <- nrow(px)*prop

    set.seed(set.seed)

    genback <- terra::spatSample(x= raster[[1]], as.df=TRUE, xy=TRUE, size =  ppn,
                                 na.rm = TRUE)[,c(1,2)]

    if(!inherits(occurences, 'sf')) ax = genback else ax <- genback |> sf::st_as_sf(coords =c('x', 'y'), crs=st_crs(4326))

    ax$pa <- 'A'

    #merge both the presence and absence data
    spmerge <- rbind(px, ax)


    #select out the first x and y columns from data extract
    selcordinates <- spmerge[,c(1,2)]

    labels <- as.factor(spmerge[,3])

    spdata <- exdata(raster = raster, coords =selcordinates,
                     labels = labels, missingness = missingness, exclude = exclude,
                     vifcutoff = vifcutoff, verbose= verbose)

    attr(spdata, 'presence') <- 'P'
    attr(spdata, 'absence') <- 'A'

  }else if(mode =='presenceabsence' && !is.null(labels)){#no need to extract pseudo absence or backgorund data.

    if(is.null(labels)) {
      stop('Provide the column with labels of presence  and absences')

    } else if(all(labels %in% colnames(occurences))==FALSE) {

      stop('Presence/Absence column not found in species data', deparse(substitute(occurences)),'.')

    }else if(!is(occurences, 'data.frame')){
      stop('Only dataset is accepted.')
    }else if(length(unique(occurences[,labels]))>2){

      stop('Only two labels of presences (P or  or Yes) and absences (A or 0 or No) are accepted.')

    }else if(is(occurences[,labels], 'numeric')){

      stop('Please convert the labels to categories or factor of 1 OR 0s, P or A, or Yes and Nos')
    }else{

      cordinates <- occurences[, c(lon, lat)]

      labels <- occurences[, labels]

      if(is.null(positive))stop('Provide the positve label (or presence label).')
      labs <- unique(labels)

      if(all(positive%in%labs)==FALSE) stop('positive label indicated not in the columnn for presence absence.')

      #extract environment data using cordinate

      spdata <- exdata(raster = raster, coords = cordinates,
                       labels = labels, missingness =missingness, exclude = exclude,
                       vifcutoff = vifcutoff, verbose= verbose)

      attr(spdata, 'presence') <- positive

      attr(spdata, 'absence') <- labs[which(labs!=positive)]
    }

  }else{
    stop('No mode of species occurences manipulation has been selected.')
  }
  return(spdata)
}

