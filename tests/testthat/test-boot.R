data(jdsdata)

worldclim1 <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))

dataprep1 <- envextract(occurences = jdsdata, raster = worldclim1,
                        lat = "lat", lon = "lon", binary = FALSE, prop = 0.8)

testthat::test_that("A list equal number of nboots produced",
                    code = {
                      nboots10 <- boots(data = dataprep1, nboots = 10, testprob = 0.3)

                      testthat::expect_equal(length(nboots10), 10)

                      testthat::expect_type(nboots10, 'list')

                      nboots2 <- boots(data = dataprep1, nboots = 2, testprob = 0.3)

                      testthat::expect_equal(length(nboots2), 2)

                    })

testthat::test_that("First inner list has two list of two dataframes",
                    code = {
                      nboots1 <- boots(data = dataprep1, nboots = 10, testprob = 0.3)

                      testthat::expect_equal(length(nboots1[[1]]), 2)

                      testthat::expect_s3_class(nboots1[[1]][[1]], 'data.frame')

                    })

testthat::test_that("nboots is not integer",
                    code = {
                      testthat::expect_error(boots(data = dataprep1, nboots = 10.3, testprob = 0.3))
                    })
testthat::test_that("Return warning when test prop is greater than/equal to 0.5",
                    code = {
                      testthat::expect_warning(boots(data = dataprep1, nboots = 10, testprob = 0.6))
                    })
testthat::test_that("data is not a dataframe",
                    code = {
                      testthat::expect_error(boots(data = dataprep1$bio1, nboots = 10, testprob = 0.3))
                    })
#initialize the data to zero

dfnull <- dataprep1[NULL, ]

testthat::test_that("If number of rows in data is zero, returns error",
                    code = {
                      testthat::expect_error(boots(data = dfnull , nboots = 10))
                    }
                    )


