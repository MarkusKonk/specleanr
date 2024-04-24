
#' Title
#'
#' @param occurences
#' @param raster
#' @param coords
#' @param class
#' @param prop
#' @param set.seed
#'
#' @return
#' @export
#'
#' @examples
gen_background <- function(occurences, raster, coords=c('geometry'),
                           class =list(present='P', absent='A'), prop = 1, set.seed=1124){

  if(missing(occurences)) stop('Provide species presence data to generate the background records')

  if(missing(raster))stop('Provide environmental raster layer for point extraction')

  if(prop<0||prop>1) stop('Prop should be between 0 (not generated) and 1 (equal to length of presence')


  set.seed(set.seed)

  presence <- occurences[,coords]

  presence$species <- class$present

  nprop <- nrow(presence)*prop

  bgpoints <- terra::spatSample(x= raster[[1]], as.df=TRUE, xy=TRUE,
                                size =  nprop, na.rm = TRUE)
  bgpoints1<- bgpoints[,c(1,2)]

  if(is(occurences, 'sf')){

    bgpoints1 <- bgpoints1 |>

      sf::st_as_sf(coords =c('x', 'y'), crs=st_crs(4326))

  }else{
    bgpoints1
  }

  bgpoints1$species <- class$absent

  speciespts <- rbind(presence, bgpoints1)

  return(speciespts)
}
