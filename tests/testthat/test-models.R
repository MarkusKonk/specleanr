
data(efidata)
worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))

datacheckf <- subset(efidata,  subset = scientificName %in%c("Squalius cephalus"))

#select species with enough records

dataprep <- envextract(occurences = datacheckf, raster = worldclim,
                      lat = "decimalLatitude", lon = "decimalLongitude",
                      binary = FALSE, prop = 0.8)


nb <- boots(data = dataprep, nboots = 10, testprob = 0.3)


test_that("Size of the output differ when full vs not full",

                    code= {
                      fullf = sdmfit(data = nb, models = "GLM")

                      fullt= sdmfit(data = nb, models = "GLM", full = TRUE)

                      testthat::expect_gt(object.size(fullt), object.size(fullf))
                      expect_equal(length(fullt[[1]]), 5) #full output--TRUE
                      expect_equal(length(fullf[[1]]), 2)#full output --FALSE

                      xc <- sdmfit(data = nb, models = 'RF1')

                      #check for randomForest function
                      expect_equal(length(xc[[1]]), 2)

                      #check for ranger model
                      xc2 <- sdmfit(data = nb, models = 'RF')

                      expect_equal(length(xc[[1]]), 2)
                    }
)
test_that("Error if data is not from boot function and metrics are not dep, indep and all",
          code = {
            expect_error(sdmfit(data = datacheckf, models = 'GLM'))

            expect_error(sdmfit(data = nb, models = 'GLM', metrics = 'deps'))
          })

test_that("Error if models used are not GLM, RF, and RF1 ",
          code = {
            expect_error(sdmfit(data = nb, models = 'GLM2'))
          })

test_that("Warning if cutoff is less than 0.5 or if its and integer",
          code = {
            expect_warning(sdmfit(data = nb, models = 'GLM', cutoff = 0.4))
            expect_error(sdmfit(data = nb, models = 'GLM', cutoff = 2))
          })


datafew <- subset(efidata,  subset = scientificName %in%c("Tinca tinca"))

#select species with few records: Tinca tinca with 4 records

prepfew <- suppressWarnings(envextract(occurences = datafew, raster = worldclim,
                       lat = "decimalLatitude", lon = "decimalLongitude",
                       binary = FALSE, prop = 0.8))

nbf <- boots(data = prepfew, nboots = 10, testprob = 0.3)

testthat::test_that("Model fails with fews reccords",
                    code = {
                     testthat::expect_error(suppressWarning(sdmfit(data = nbf, models = "GLM")))
                    })




