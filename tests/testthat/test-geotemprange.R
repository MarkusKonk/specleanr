
data("efidata")

db <- sf::read_sf(system.file('extdata/danube.shp.zip', package = "specleanr"), quiet = TRUE)

test_that(desc = "Latitudinal/Longitudinal range",
          code = {
            #Missing data error
            expect_error(geo_ranges())

            #Wrong data
            expect_error(geo_ranges(data = db))

            #error if column for species not provided for dataframe

            expect_error(geo_ranges(data = efidata))

            #wrong column species

            expect_error(geo_ranges(data = efidata, colsp = 'speciesname'))
            #dataframe
            expect_type(geo_ranges(data = efidata, colsp = 'scientificName'), 'double')
            #list
            expect_type(geo_ranges(data = as.list(efidata$scientificName)), 'double')
            #vector
            expect_type(geo_ranges(data = c("Salmo trutta", "Lates niloticus")), 'double')

            #species not in Fishbase
            expect_true(is.na(suppressWarnings(geo_ranges(data = 'shshshsh'))))

          })

#temp ranges checks

test_that(desc = "Sucess and errors for thermal range from FishBase",
          code = {
            #missing data
            expect_error(thermal_ranges())

            #error if column for species not provided for dataframe

            expect_error(thermal_ranges(x = efidata))

            #wrong column species

            expect_error(thermal_ranges(x = efidata, colsp = 'speciesname'))

            expect_error(thermal_ranges(x = efidata, colsp = 'speciesname'))
            #dataframe
            expect_type(thermal_ranges(x = efidata, colsp = 'scientificName'), 'list')
            #list
            expect_type(thermal_ranges(x = efidata$scientificName), 'list')
            #vector
            expect_type(thermal_ranges(x = c("Salmo trutta", "Lates niloticus")), 'list')

            expect_message(thermal_ranges("Brycinus nurse", verbose = TRUE))

            #In fishase and df of rnages provided.
            expect_equal(nrow(thermal_ranges("Salmo trutta")),1 )

            #not in fishBase
            expect_warning(thermal_ranges("Salmo trddfddutta"))

          #no maximum/max/ref temperature but in FB
            expect_true(is.na(thermal_ranges("Lates niloticus")$tempmin))#max
          })





