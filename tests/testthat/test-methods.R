data("jdsdata")
data("efidata")

wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))

mdf <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
                      lats = 'lat', lons = 'lon',
                      country = 'JDS4_site_ID',
                      species = c('scientificName', 'speciesname'),
                      date=c('sampling_date','Date'))

mdfclean <- check_names(mdf, colsp = 'species', verbose = F, merge = T)

db <- sf::read_sf(system.file('extdata/danube/basinfinal.shp', package = "specleanr"), quiet = TRUE)


#extract data

refdata <- pred_extract(data = mdfclean, raster = wcd,
                        lat = 'decimalLatitude', lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'speciescheck',
                        list = TRUE,
                        verbose = F,
                        minpts = 6,
                        merge = F)

sp <- refdata[['Salmo trutta']]
#Preferred temperature (Ref. 123201): 6.5 - 15.8, mean 10.1 °C #FishBase

testthat::test_that('use minimum and maximum temperature to flag suspicious outlier',
          code = {
            dx <- ecological_ranges(data = sp, min = 6.5, max = 15.8, var = 'bio1', output ='outlier')
            expect_s3_class(dx, 'data.frame')
          })

testthat::test_that('use only one parameter such as max temperature-(ecoparam and direction)',
          code = {
            dx <- ecological_ranges(data = sp, ecoparam = 15.8, var = 'bio1', output ='outlier',
                                    direction = 'greater')
            testthat::expect_s3_class(dx, 'data.frame')
          })

#using parameter in a dataframe
#1. dataset with optimal parameters//collated in literature
optdata <- data.frame(species= c("Salmo trutta", "Abramis brama"),
                      mintemp = c(2, 10),maxtemp = c(24, 24),
                      meantemp = c(9, NA),
                      direction = c('less', 'greater'))

testthat::test_that(desc = "optimal ranges from literature for multiple species",
          code = {
            dx <- sdata3 <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                              var = 'bio1', output = "outlier",
                                              optimumSettings = list(optdf = optdata,
                                              maxcol = "maxtemp",
                                              mincol ="mintemp",
                                              optspcol = "species"))
            testthat::expect_s3_class(dx, "data.frame")
          })

testthat::test_that(desc = "optimal ranges from literature for multiple species but only one",
          code = {
            dx <- sdata3 <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                              var = 'bio1', output = "outlier",
                                              optimumSettings = list(optdf = optdata,
                                                                     ecoparam = "meantemp",
                                                                     direction ="direction",
                                                                     optspcol = "species"))
            expect_s3_class(dx, "data.frame")
          })

#If the taxa is fish and connected on internet, the user can access both the temperature and
#geospatial ranges (latitude and longitudinal ranges)

testthat::test_that(desc = "check for temperature or georanges",
          code = {
            testthat::skip_if_offline()
            #provide var or variable to check, annual temperature (bio1)
            dx <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                    var = 'bio1', output = "outlier",
                                    checkfishbase = TRUE, mode = 'temp')
            expect_s3_class(dx, "data.frame")

            #provide decimal longitude and latitude for georanges (x and y extracted at pred_extract)
            dx2 <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                    lat = 'y', lon = 'x', output = "outlier",
                                    checkfishbase = TRUE, mode = 'geo')
            testthat::expect_s3_class(dx2, "data.frame")
          })

testthat::test_that(desc = "If the var is not in the enviromental predictor the
                    univariate methos will fail",
                    code = {
                      #var = 'bio' is not in the environmental predictors.
                      #similar to other univariate methods, they will all fail
                      testthat::expect_error(adjustboxplots(data = sp, var = 'bio',
                                                            output = "outlier"))
                    })

testthat::test_that(desc = "Adjusted boxplots",
                    code = {
                      adout <- adjustboxplots(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = adout, 'data.frame')
                    })

testthat::test_that(desc = "Interquartile range returns a dataframe of outliers.",
                    code = {
                      iqrout <- interquartile(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = iqrout, 'data.frame')
                    })

testthat::test_that(desc = "Semi interquartile range returns a dataframe of outliers.",
                    code = {
                      semiiqroiut <- semiIQR(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = semiiqroiut, 'data.frame')
                    })

testthat::test_that(desc = "Hampel returns a dataframe of outliers.",
                    code = {
                      hampelout <- hampel(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = hampelout, 'data.frame')
                    })

testthat::test_that(desc = "Reverse jack knifing returns a dataframe of outliers.",
                    code = {
                      jkout <- jknife(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = jkout, 'data.frame')
                    })

testthat::test_that(desc = "Z-score returns a dataframe of outliers.",
                    code = {
                      zscout <- zscore(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = zscout, 'data.frame')
                    })

testthat::test_that(desc = "Logarthmic boxplot returns a dataframe of outliers.",
                    code = {
                      logout <- logboxplot(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = logout, 'data.frame')
                    })
testthat::test_that(desc = "Mixed interquantile range returns a dataframe of outliers.",
                    code = {
                      mxdout <- mixediqr(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = mxdout, 'data.frame')
                    })

testthat::test_that(desc = "Median rule range returns a dataframe of outliers.",
                    code = {
                      medout <- medianrule(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = medout, 'data.frame')
                    })

testthat::test_that(desc = "Median rule range returns a dataframe of outliers.",
                    code = {
                      distout <- distboxplot(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = distout, 'data.frame')
                    })

testthat::test_that(desc = "Sequential fences returns a dataframe of outliers.",
                    code = {
                      seqout <- seqfences(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = seqout, 'data.frame')
                    })
testthat::test_that(desc = "Sequential fences returns an error if the records exceeds 100 records.",
                    code = {
                      sprbind <- rbind(sp, sp, sp, sp)

                      testthat::expect_error(seqfences(data = sprbind, var = 'bio6', output='outlier'))
                    })
#multivariate tests
testthat::test_that(desc = "Checks for isolation forest",
                    code = {
                      isout <- isoforest(data = sp, size = 0.7,  output='outlier',
                                         exclude = c("x", "y"))

                      testthat::expect_s3_class(object = isout, 'data.frame')
                    })
testthat::test_that(desc = "Checks for one class support vector machines",
                    code = {
                      osvout <- onesvm(data = sp, exclude = c("x", "y"),  output='outlier')

                      testthat::expect_s3_class(object = osvout, 'data.frame')
                    })

testthat::test_that(desc = "Checks for Local outlier factor whether return dataframe of outliers",
                    code = {
                      xlofout <- xlof(data = sp, exclude = c("x", "y"),output='outlier',
                                      metric ='manhattan', minPts = 10, mode = "soft")

                      testthat::expect_s3_class(object = xlofout, 'data.frame')
                    })

testthat::test_that(desc = "Checks for k-nearest neighbours whether return dataframe of outliers",
                    code = {
                      xknnout <- xknn(data = sp, exclude = c("x", "y"),
                                      output='outlier', metric ='manhattan',
                                      mode = "soft")

                      testthat::expect_s3_class(object = xknnout, 'data.frame')
                    })


testthat::test_that(desc = "Checks for Global-Local Outlier Score from Hierarchies whether return dataframe of outliers",
                    code = {
                      xgloshout <- xglosh(data = sp, exclude = c("x", "y"),
                                          output='outlier', metric ='manhattan', k = 3,
                                          mode = "soft")

                      testthat::expect_s3_class(object = xgloshout, 'data.frame')
                    })

testthat::test_that(desc = "Checks Mahalanobis distance measures whether return dataframe of outliers",
                    code = {
                      mahalout <- mahal(data = sp, exclude = c('x','y'), output='outlier')

                      testthat::expect_s3_class(object = mahalout, 'data.frame')
                    })

testthat::test_that(desc = "Checks k-means whether return dataframe of outliers",
                    code = {
                      kmeanout <- xkmeans(data = sp,output='outlier', exclude = c('x', 'y'),
                                          mode = 'soft', k=3)

                      testthat::expect_s3_class(object = kmeanout, 'data.frame')

                      #test that k less than 2 will lead to errors.
                      expect_error(xkmeans(data = sp,output='outlier', exclude = c('x', 'y'),
                                           mode = 'soft', k=1))
                    })
