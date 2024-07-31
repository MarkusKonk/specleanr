
#all data combined for all species
soutl <- multidetect(data = iris, var = 'Sepal.Width',
                 multiple = FALSE, exclude = "Species",
                 methods = c('mixediqr', 'logboxplot','lof',
                             'distboxplot','iqr', 'semiqr','adjbox',
                             'zscore', 'hampel'), showErrors = FALSE)

#multiple species per species
moutl <- multidetect(data = iris, var = 'Sepal.Width',
                      multiple = TRUE, colsp = "Species",
                      methods = c('mixediqr', 'logboxplot','lof'), showErrors = FALSE)

test_that(desc = "count the classes produced",
          code = {

            expect_equal(length(class(ggoutliers(soutl))), 2)
            #raw = FALSE
            expect_equal(length(class(ggoutliers(soutl, raw = FALSE))), 2)#get the same result

            #test for multiple species

            expect_equal(length(class( ggoutliers(moutl, 1, colsp = 'Species'))), 2)

            #expect error if no index provided for multiple items ggoutlier
            expect_error(ggoutliers(moutl))

            #similar for plot, the same is obtained

            expect_equal(length(class(plot(soutl))), 2)
            #raw = FALSE
            expect_equal(length(class(plot(soutl, raw = FALSE))), 2)#get the same result

            #test for multiple species

            expect_equal(length(class( plot(moutl, 1, colsp = "Species"))), 2)

            #expect error if no index provided for multiple items
            expect_error(plot(moutl))
          })




