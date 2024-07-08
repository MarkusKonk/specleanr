
data(efidata)
worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))

datacheckf <- subset(efidata,  subset = scientificName %in%c("Squalius cephalus"))

#select species with enough records

dataprep <- envextract(occurences = datacheckf, raster = worldclim,
                      lat = "decimalLatitude", lon = "decimalLongitude",
                      binary = FALSE, prop = 0.8)


nb <- boots(data = dataprep, nboots = 10, testprob = 0.3)


testthat::test_that("Size of the output differ when full vs not full",

                    code= {
                      fullf = sdmfit(data = nb, models = "GLM")

                      fullt= sdmfit(data = nb, models = "GLM", full = TRUE)

                      testthat::expect_gt(object.size(fullt), object.size(fullf))
                    }
)


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




