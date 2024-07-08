
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
