
test_that(desc = 'Return species data',
          code = {
            ondata <- getdata(data = 'Gymnocephalus baloni', mode = 'all', gbiflim = 10, vertlim = 10,
                              inatlim = 10, verbose = FALSE)

            extd <- extract_online(online = ondata)

            expect_lt(nrow(extd), 30)

            expect_contains(colnames(extd), c('country', 'species', 'dates', 'decimalLatitude','decimalLongitude'))

          })
