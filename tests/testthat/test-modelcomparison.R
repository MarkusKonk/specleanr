
#worldclim data for to extract environmental predictors
worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))

#get 500 records online using getdata function to compliment Salmo trutta records and basin polygon
#basin to get the bounding box to delineate the area of concern

db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)

salmonline <- getdata(data = "Salmo trutta", gbiflim = 500, inatlim = 3, vertlim = 3, bbox = db)

salextract <- extract_online(salmonline)

#merge both online and offline data and filter Salmo trutta

#select species with enough records

datafinal <- salextract[salextract[,'species'] == "Salmo trutta", ]

#initial data extraction and preliminary analysis

rdata <- pred_extract(data = datafinal, raster= worldclim ,lat = 'decimalLatitude',
                      lon= 'decimalLongitude', colsp = 'species', bbox  = db,
                      multiple = FALSE,minpts = 10, list=TRUE,
                      merge=F, verbose = F)


outliersdf <- multidetect(data = rdata, multiple = FALSE,var = 'bio6',output = 'outlier',
                          exclude = c('x','y'),
                          methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))

modeout <- modelcomparison(refdata = rdata, outliers = outliersdf,
                           raster = worldclim,
                           lat = 'y', lon = 'x', models = "GLM",
                           mode = 'best', testprop = 0.2, metrics = 'all',
                           thresholds = 0.2, full = FALSE, minpts = 10)

test_that(desc = "Model output comparison",
                    code = {
                      #model output
                      testthat::expect_equal(length(modeout), 1)

                      getperf <- get_performance(modelcomp  = modeout)

                      #equal to number of boots
                      testthat::expect_equal(nrow(getperf), 20)

                      #test for test
                      expect_equal(length(class(ggperform(modelout = modeout))), 2)

                      #for training data
                      expect_equal(length(class(ggperform(modelout = modeout, type = 'train'))), 2)

                      #test for scale
                      expect_equal(length(class(ggperform(modelout = modeout, type = 'train', scales = 'free_x'))), 2)

                    })
test_that("Outliers parameter should be datacleaner class",
          code = {
              expect_error(modelcomparison(refdata = rdata, outliers = datafinal,
                                           raster = worldclim,
                                           lat = 'y', lon = 'x', models = "GLM",
                                           mode = 'best', testprop = 0.2, metrics = 'all',
                                           thresholds = 0.2, full = FALSE, minpts = 10))
          }
)

test_that("Error if latitude and longitude not refdata",
          code = {
            expect_error(modelcomparison(refdata = rdata, outliers = outliersdf,
                                         raster = worldclim,
                                         lat = 'decimalLatitude', lon = 'decimalLongitude',
                                         models = "GLM",
                                         mode = 'best', testprop = 0.2, metrics = 'all',
                                         thresholds = 0.2, full = FALSE, minpts = 10))
          }
)
test_that("Error if any threshold value is greater than 1",
          code = {
            expect_error(modelcomparison(refdata = rdata, outliers = outliersdf, raster = worldclim,
                                         lat = 'y', lon = 'x',
                                         models = "GLM",mode = 'best', testprop = 0.2, metrics = 'all',
                                         thresholds = c(0.3,1.1, 2), full = FALSE, minpts = 10))
          }
)
test_that("Error if mode is not either best or abs",
          code = {
            expect_error(modelcomparison(refdata = rdata, outliers = outliersdf, raster = worldclim,
                                         lat = 'y', lon = 'x',
                                         models = "GLM",mode = 'bestmethod',
                                         testprop = 0.2, metrics = 'all',
                                         thresholds = c(0.3,0.4), full = FALSE, minpts = 10))
          }
)

