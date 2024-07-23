
#check taxa names

test_that(desc = "Errors and success in check taxa names",

          code= {
            #name output
            expect_type(check_taxa_names(sp = "Lates niloticus"), 'character')

            #expect warning as specie name is not found and spfinal is the original name

            expect_warning(check_taxa_names(sp = "LLatess nilotus"))
          }
)


test_that(desc = 'Return species data',
          code = {
            ondata <- getdata(data = 'Gymnocephalus baloni', mode = 'all', gbiflim = 10, vertlim = 10,
                              inatlim = 10, verbose = FALSE)

            extd <- extract_online(online = ondata)

            expect_lt(nrow(extd), 30)

            expect_contains(colnames(extd), c('country', 'species', 'dates', 'decimalLatitude','decimalLongitude'))

          })
