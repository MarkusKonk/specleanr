
data("efidata")

sal <- subset(efidata, scientificName=='Anguilla anguilla')

testthat::test_that(desc = "Returns a datframe of records",
                    code = {
                      testthat::skip_if_offline()
                      testthat::skip_on_cran()
                      br <- basinranges(occurrences = sal, species = "Anguilla anguilla",
                                        lat = 'decimalLatitude', lon = 'decimalLongitude')
                      testthat::expect_s3_class(br, 'data.frame')
                    })


testthat::test_that("Latitude or Longitude missing in the data",
                    code = {
                     expect_error(basinranges(occurrences = sal, species = "Anguilla anguilla",
                                  lat = 'decimalLatitu', lon = 'decimalLongitde'))
                    })

testthat::test_that("Check if output choices are true-only records and basin allowed",
                    code = {
                      testthat::expect_error(basinranges(occurrences = sal,
                                                         species = "Anguilla anguilla",
                                                         lat = 'decimalLatitu',
                                                         lon = 'decimalLongitde', output = 'reco'))
                    })
