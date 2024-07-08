

data("jdsdata")
data("efidata")

matchd <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
                         lats = 'lat', lons = 'lon',
                         country = 'JDS4_site_ID',
                         species = c('scientificName', 'speciesname'),
                         date=c('sampling_date','Date'))

sp <- check_names(data = matchd, colsp = 'species', verbose = F, pct = 90, merge = T, sn = FALSE)

db <- sf::st_read(dsn=system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)

zz <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))


testthat::test_that(desc = 'Less number of records after discarding duplicates and missing values.',
          code = {

            testthat::expect_error(
              pred_extract(data = jdsdata,raster= zz , lat ='lat',lon = 'lon',
                           colsp = 'speciesname', bbox  = db, multiple = TRUE, verbose = F,
                           list= TRUE, minpts = 10, merge=T))
          }
)

testthat::test_that(desc = 'error if multiple is set to FALSE for more than one species.',
                    code = {

                      testthat::expect_error(
                        pred_extract(data = sp,raster= zz ,
                                     lat ='decimalLatitude',lon = 'decimalLongitude',colsp = 'species',
                                     bbox  = db, multiple = FALSE, verbose = F,
                                     list= TRUE, minpts = 10, merge=T))

                    }
)

testthat::test_that(desc = "Check if merge increase the number of columns on the reference dataset.",
                    code = {
                      #return other orginal columns on the dataset
                      mergedf <- pred_extract(data = sp,raster= zz ,
                                   lat ='decimalLatitude',lon = 'decimalLongitude',colsp = 'species',
                                   bbox  = db, multiple = TRUE, verbose = F,
                                   list= FALSE,minpts = 10, merge=T)

                      nomergedf <- pred_extract(data = sp,raster= zz ,
                                              lat ='decimalLatitude',lon = 'decimalLongitude',colsp = 'species',
                                              bbox  = db, multiple = TRUE, verbose = F,
                                              list= FALSE,minpts = 10, merge=F)

                      testthat::expect_gt(ncol(mergedf), ncol(nomergedf))

                    })
