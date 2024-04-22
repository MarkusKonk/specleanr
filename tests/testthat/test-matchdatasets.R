
data("jdsdata")
data("efidata")


test_that(desc = 'Succefully match and bound the datasets',
          code = {
            matchdf <- match_datasets(datasets = list(jds = jdsdata, efi=efidata),
                           lats = 'lat',
                           lons = 'lon',
                           species = c('speciesname','scientificName'),
                           date = c('Date', 'sampling_date'),
                           country = c('JDS4_site_ID'))
            expect_equal(nrow(matchdf), sum(nrow(jdsdata), nrow(efidata)))
            expect_gt(ncol(matchdf), 5)
          })


test_that(desc = 'Standard column for country is left NULL',
          code = {

            expect_error(match_datasets(datasets = list(jds = jdsdata, efi = efidata),
                                  lats = 'lat',
                                  lons = 'lon',
                                  species = c('speciesname','scientificName'),
                                  date = c('Date', 'sampling_date')))#country left
          })

test_that(desc = 'Datasets not provided in a list with names',
          code = {
            expect_error(match_datasets(datasets = c(jdsdata, efidata),#in vector not list
                                        lats = 'lat',
                                        lons = 'lon',
                                        species = c('speciesname','scientificName'),
                                        date = c('Date', 'sampling_date'),
                                        country = c('JDS4_site_ID')

                                        )
                         )
          })

test_that(desc = 'The names provided are not in the merged data',
          code = {
            expect_error(
              match_datasets(datasets = list(jdsdata, efidata),
                             lats = 'latitude',#not found in any df
                             lons = 'lon',
                             species = c('speciesname','scientificName'),
                             date = c('Date', 'sampling_date'),
                             country = c('JDS4_site_ID')
              )
            )

          })




