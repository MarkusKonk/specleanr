data(efidata)

worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))

testthat::test_that("confirm that y varible is created on the dataset with P and A and they 2only" ,
                    code = {
                      dfprep <- envextract(occurences = efidata, raster = worldclim,
                                           lat = "decimalLatitude", lon = "decimalLongitude",
                                           binary = FALSE, prop = 0.8)

                      testthat::expect_true("y"%in%colnames(dfprep))

                      testthat::expect_equal(length(unique(dfprep$y)), 2)
                    })
#test for binary labels
efidata2 <- efidata
efidata2$pa<- rep(c("A", "P"), c(nrow(efidata2)/2, nrow(efidata2)/2))

testthat::test_that("Returns an error if binary is TRUE but the labels column is not provided.",
                    code = {
                      testthat::expect_error(envextract(occurences = efidata2,
                                                        raster = worldclim, lat = "decimalLatitude",
                                                        lon = "decimalLongitude", binary = TRUE,
                                                        positive = "P", vifcutoff = 0.9))
                    })

testthat::test_that("Returns an error if binary is TRUE but positive class is not indicated.",
                    code = {
                      testthat::expect_error(envextract(occurences = efidata2,
                                                        raster = worldclim, lat = "decimalLatitude",
                                                        lon = "decimalLongitude",
                                                        binary = TRUE,
                                                        labels = 'pa',
                                                        vifcutoff = 0.9))
                    })

testthat::test_that("Returns dataframe and a warning of perfect fit from usdm vifcor function .",
                    code = {
                      df <- suppressWarnings(envextract(occurences = efidata2,
                                                          raster = worldclim, lat = "decimalLatitude",
                                                          lon = "decimalLongitude",
                                                          binary = TRUE,
                                                          labels = 'pa',
                                                          positive = 'P',
                                                          vifcutoff = 0.9))
                      testthat::expect_s3_class(df, 'data.frame')
                    })

testthat::test_that("Return an error if the latitude and longitude are interchanged .",
                    code = {
                      testthat::expect_error(envextract(occurences = efidata2,
                                                        raster = worldclim, lat = "decimalLongitude",
                                                        lon = "decimalLatitude",
                                                        binary = TRUE,
                                                        labels = 'pa',
                                                        positive = 'P',
                                                        vifcutoff = 0.9))
                    })


efisfdata <- sf::st_as_sf(efidata, coords = c("decimalLongitude",
                                              "decimalLatitude"), crs= sf::st_crs(4326))

testthat::test_that("Return error if occurences is sf and geom parameter is not provided",
                    code = {
                      testthat::expect_error( envextract(occurences = efisfdata,
                                                         raster = worldclim,
                                                         lat = "decimalLongitude",
                                                         lon = "decimalLatitude",
                                                         binary = TRUE,
                                                         positive = "P", labels = "pa",
                                                         vifcutoff = 0.9))
                    }
)

testthat::test_that("binary =TRUE overides both labels and positive parameters: dataframe expected",
                    code = {
                       df2 <- suppressWarnings(envextract(occurences = efisfdata,
                                                         raster = worldclim,
                                                         geom = 'geometry',
                                                         binary = FALSE,
                                                         positive = "P",
                                                         vifcutoff = 0.9))
                      testthat::expect_s3_class(df2, 'data.frame')

                    }
)



