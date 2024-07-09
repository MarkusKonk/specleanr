
data("efidata")

testthat::test_that(desc = "Not found/found in the NATURA 2000 species list",
                    code = {

                      testthat::expect_null(check_natura(x="Salmo trutta", quiet = TRUE, citeshow = FALSE))
                      #the species Alca torda is found on the NATURA 2000 list

                      testthat::expect_type(check_natura("Alca torda", citeshow = FALSE), 'character')

                      #return list of species details
                      testthat::expect_type(check_natura("Alca torda", details = TRUE, citeshow = F), 'list')

                    })

test_that("Return error if data row are zero",
          code = {
            df <- efidata[NULL,]
            expect_error(naturaranges(data= df, species = 'scientificName',
                                       lon = 'decimalLongitude', lat='decimalLatitude',
                                       verbose = T, batch = TRUE))
            expect_error(naturaranges(data= unlist(efidata$decimalLatitude), species = 'scientificName',
                                      lon = 'decimalLongitude', lat='decimalLatitude',
                                      verbose = T, batch = TRUE))
            })


