data("jdsdata")
data("efidata")

wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))

mdf <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
                      lats = 'lat', lons = 'lon',
                      country = 'JDS4_site_ID',
                      species = c('scientificName', 'speciesname'),
                      date=c('sampling_date','Date'))

mdfclean <- check_names(mdf, colsp = 'species', verbose = F, merge = T)

db <- sf::read_sf(system.file('extdata/danube/basinfinal.shp', package = "specleanr"), quiet = TRUE)


#extract data

refdata <- pred_extract(data = mdfclean, raster = wcd,
                        lat = 'decimalLatitude', lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'speciescheck',
                        list = TRUE,
                        verbose = F,
                        minpts = 6,
                        merge = F)

sp <- refdata[['Salmo trutta']]
#Preferred temperature (Ref. 123201): 6.5 - 15.8, mean 10.1 °C #FishBase

test_that('use minimum and maximum temperature to flag suspicious outlier',
          code = {
            dx <- ecological_ranges(data = sp, min = 6.5, max = 15.8, var = 'bio1', output ='outlier')
            expect_s3_class(dx, 'data.frame')
          })

test_that('use only one parameter such as max temperature-(ecoparam and direction)',
          code = {
            dx <- ecological_ranges(data = sp, ecoparam = 15.8, var = 'bio1', output ='outlier',
                                    direction = 'greater')
            expect_s3_class(dx, 'data.frame')
          })

#using parameter in a dataframe
#1. dataset with optimal parameters//collated in literature
optdata <- data.frame(species= c("Salmo trutta", "Abramis brama"),
                      mintemp = c(2, 10),maxtemp = c(24, 24),
                      meantemp = c(9, NA),
                      direction = c('less', 'greater'))

test_that(desc = "optimal ranges from literature for multiple species",
          code = {
            dx <- sdata3 <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                              var = 'bio1', output = "outlier",
                                              optimumSettings = list(optdf = optdata,
                                              maxcol = "maxtemp",
                                              mincol ="mintemp",
                                              optspcol = "species"))
            expect_s3_class(dx, "data.frame")
          })

test_that(desc = "optimal ranges from literature for multiple species but only one",
          code = {
            dx <- sdata3 <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                              var = 'bio1', output = "outlier",
                                              optimumSettings = list(optdf = optdata,
                                                                     ecoparam = "meantemp",
                                                                     direction ="direction",
                                                                     optspcol = "species"))
            expect_s3_class(dx, "data.frame")
          })

#If the taxa is fish and connected on internet, the user can access both the temperature and
#geospatial ranges (latitude and longitudinal ranges)

test_that(desc = "check for temperature or georanges",
          code = {
            testthat::skip_if_offline()
            #provide var or variable to check, annual temperature (bio1)
            dx <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                    var = 'bio1', output = "outlier",
                                    checkfishbase = TRUE, mode = 'temp')
            expect_s3_class(dx, "data.frame")

            #provide decimal longitude and latitude for georanges (x and y extracted at pred_extract)
            dx2 <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                    lat = 'y', lon = 'x', output = "outlier",
                                    checkfishbase = TRUE, mode = 'geo')
            expect_s3_class(dx2, "data.frame")
          })
