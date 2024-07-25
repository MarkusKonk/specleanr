
data("efidata")

# test_that(desc = "download natura ranges/ Errors and success",
#           code = {
#             skip_on_cran()
#             #low time out
#             expect_error(downloadnatura(x = 'natura', timeout = 2))
#
#           })

test_that(desc = "Errors and success for check natura",
                    code = {

                      expect_null(check_natura(x="Salmo trutta", quiet = TRUE, citeshow = FALSE))

                      #the species Alca torda is found on the NATURA 2000 list

                      expect_type(check_natura("Alca torda", citeshow = FALSE), 'character')

                      #return list of species details
                      expect_type(check_natura("Alca torda", details = TRUE, citeshow = F), 'list')

                      expect_message(check_natura(x="Salmo trutta", quiet = FALSE))

                      expect_message(check_natura(x="Salmo trutta", quiet = FALSE))

                    })

test_that("Return error if data row are zero",
          code = {
            expect_error(naturaranges(data= efidata[NULL,], species = 'scientificName',
                                       lon = 'decimalLongitude', lat='decimalLatitude',
                                       verbose = T, batch = TRUE))
            expect_error(naturaranges(data= unlist(efidata$decimalLatitude), species = 'scientificName',
                                      lon = 'decimalLongitude', lat='decimalLatitude',
                                      verbose = T, batch = TRUE))

            #expect error for missing data file
            expect_error(naturaranges( species = 'scientificName',
                                      lon = 'decimalLongitude', lat='decimalLatitude',
                                      verbose = T, batch = TRUE))
            })


