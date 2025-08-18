
#test check names whether they return dataframe, list or vector

#species data
data("jdsdata")
data("efidata")

test_that(desc = 'A dataframe is returned',
          code = {
            skip_on_cran()
            expect_s3_class(check_names(data = jdsdata, colsp = 'speciesname',verbose = F,
                              pct = 90, merge = TRUE), 'data.frame')
          })

#check if a species cleaned name is returned

test_that(desc = 'Character of clean returned',
          code = {
            skip_on_cran()
            expect_type(check_names(data = 'Salmo Trutta FARIO',verbose = F, pct = 90,
                                    merge = F, sn=FALSE), 'character')
            expect_type(check_names(data = 'Salmo Trutta FARIO',verbose = F, pct = 90,
                                    merge = F, sn=TRUE), 'character')

            expect_message(check_names(data = 'Salmo Trutta FARIO',verbose = T, pct = 90,
                        merge = F, sn=TRUE), "The synoynm species Salmo trutta fario maintained in the list.")

            expect_message(check_names(data = 'Anthony Basooma',verbose = T, pct = 90,
                                       merge = F, sn=FALSE),"No close name in Fish Base for Anthony basooma.")
          })
test_that(desc = 'Species not found in FishBase',
          code = {
            skip_on_cran()
            expect_message(check_names(data = 'Salmerj ncnd', verbose = TRUE))
          })

test_that(desc = 'Expect number changes when merge is either FALSE or TRUE/contain speciescheck)',
          code = {

            skip_on_cran()

            df2col <- check_names(data = efidata, colsp = 'scientificName', verbose = F)#merge false

            expect_equal(ncol(df2col), 2)

            #expect error if the species column is not in the data

            expect_error(check_names(data = efidata, colsp = 'species', merge = T, verbose = F))#species not in EFI data

            #error if the species column is not provided

            expect_error(check_names(data = efidata, merge = T, verbose = F))

            #merged is expected on dataframes only
            expect_error(check_names(data = 'Salmo Trutta FARIO',verbose = F, pct = 90, merge = T, sn=FALSE))

            dfmanycol <- check_names(data = efidata, colsp = 'scientificName', merge = T, verbose = F)

            expect_gt(ncol(dfmanycol), 2)

            expect_contains(colnames(df2col), 'speciescheck')

            expect_contains(colnames(dfmanycol), 'speciescheck')

            skip_if_not_installed("sf")

            efisf <- efidata %>% sf::st_as_sf(coords =c("decimalLongitude", "decimalLatitude" ), crs = sf::st_crs(4326))

            expect_equal(ncol(check_names(data = efisf, colsp = 'scientificName', verbose = F)), 2) #expect two columns

            #expect two columns even if a list is used
            expect_equal(ncol(check_names(data = list(sp='salmo trutta', sp2 = 'abramis brama'),
                                          verbose = F, pct = 90, merge = F, sn=FALSE)), 2)

            #ecosystem types
            xvv <- check_names(data = efidata, colsp = "scientificName", verbose = F, pct = 90,
                              merge = T, sn=FALSE, ecosystem = TRUE, rm_duplicates = TRUE)
            expect_contains(colnames(xvv), 'Fresh')

          })

