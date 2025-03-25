
#test check names whether they return dataframe, list or vector

#species data
data("jdsdata")
data("efidata")
species = 'Salmo Trutta FARIO'

test_that(desc = 'A dataframe is returned',
          code = {
            df <- check_names(data = jdsdata,
                              colsp = 'speciesname',
                              verbose = F,
                              pct = 90,
                              merge = TRUE)

            expect_s3_class(df, 'data.frame')

          })

#check if a species cleaned name is returned

test_that(desc = 'Character of clean returned',
          code = {
            spx <- check_names(data = species,
                               verbose = F,
                               pct = 90,
                               merge = F,
                               sn=FALSE)
            expect_type(spx, 'character')
          })
test_that(desc = 'Species not found in FishBase',
          code = {
            expect_message(check_names(data = 'Salmerj ncnd', verbose = TRUE))
          })

test_that(desc = 'Expect number changes when merge is either FALSE or TRUE/contain speciescheck)',
          code = {
            testthat::skip_on_cran()
            df2col <- check_names(data = efidata, colsp = 'scientificName', verbose = F)#merge false

            expect_equal(ncol(df2col), 2)

            dfmanycol <- check_names(data = efidata, colsp = 'scientificName', merge = T, verbose = F)

            expect_gt(ncol(dfmanycol), 2)

            expect_contains(colnames(df2col), 'speciescheck')

            expect_contains(colnames(dfmanycol), 'speciescheck')
          })












