
data("jdsdata")
data("efidata")

wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))

#match and clean

matchd <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
                         lats = 'lat', lons = 'lon',
                         country = 'JDS4_site_ID',
                         species = c('scientificName', 'speciesname'),
                         date=c('sampling_date','Date'))

matchclean <- check_names(matchd, colsp = 'species', verbose = F, merge = T)

db <- sf::read_sf(system.file('extdata/danube.shp.zip',
                              package = "specleanr"), quiet = TRUE)


#extract environmental data

refdata <- pred_extract(data = matchclean, raster = wcd,
                        lat = 'decimalLatitude',
                        lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'speciescheck',
                        list = TRUE,
                        verbose = F,
                        minpts = 6,
                        merge = F)

#using a dataframe of species not a list# change list to FALSE

refdata_df <- pred_extract(data = matchclean, raster = wcd,
                        lat = 'decimalLatitude',
                        lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'speciescheck',
                        list = FALSE,
                        verbose = F,
                        minpts = 6,
                        merge = F)

#for multiple species
outlist <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                       exclude = c('x','y'),
                       multiple = TRUE,
                       methods = c('mixediqr', "iqr", "seqfences",
                                   "mahal", 'glosh', 'onesvm','logboxplot','knn',
                                   'distboxplot','medianrule',
                                   'iforest','jknife','reference'))

test_that(desc = 'List of species with outliers produced',
                    code = {

                      testthat::expect_equal(length(outlist@result), length(refdata))
                      testthat::expect_identical(outlist@dfname, "refdata")
                      testthat::expect_identical(outlist@varused, "bio6")
                      testthat::expect_identical(outlist@excluded, c('x','y'))
                      testthat::expect_identical(outlist@methodsused, c('mixediqr', "iqr", "seqfences",
                                                                        "mahal", 'glosh', 'onesvm','logboxplot','knn',
                                                                        'distboxplot','medianrule',
                                                                        'iforest','jknife','reference'))
                      testthat::expect_identical(outlist@out, "outlier")
                      testthat::expect_identical(outlist@mode, TRUE)
                      testthat::expect_type(refdata, 'list') #not a dataframe but list

                      #test that the datacleaner is printed out: multiple species
                      expect_output(show(outlist))

                      #multiple but index not provided.
                      expect_s3_class(extractoutliers(outlist), 'data.frame')
                    })

#test trycatch errors

test_that(desc = "Return errors and no errors",
          code = {
            #expect warning when the kmpar for kmeans the k is less than 2 and silence_true_errors = FALSE
            #that some methods particularly kmeans did not execute
            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                     exclude = c('x','y'),
                                     multiple = TRUE,
                                     methods = c('mixediqr', "iqr", "seqfences",
                                                 "mahal", 'glosh', 'onesvm','kmeans'),
                                     kmpar= list(k = 1, method = "silhouette", mode = "soft"),
                                     silence_true_errors = FALSE))

            #expect to hide the error/warning in the hood when silence_true_errors is FALSE and expect datacleaner

            outlierFALSE <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                        exclude = c('x','y'),
                                        multiple = TRUE,
                                        methods = c('mixediqr',"mahal", 'glosh', 'onesvm'),
                                        kmpar= list(k = 1, method = "silhouette", mode = "soft"),
                                        silence_true_errors = FALSE)
            expect_s4_class(outlierFALSE, 'datacleaner')

            #extract the outlier for species index = 2 to confirm that kmeans is not among the methods
            dfout <- extractoutliers(x= outlierFALSE, sp = 2)

            expect_false("kmeans"%in%unlist(dfout$method)) # kmeans did not exceute
            expect_true("mahal"%in%unlist(dfout$method)) # mahal executed

            #expect an error if the var (bio61) is not in the data.
            expect_error(multidetect(data = refdata, var = 'bio61', output = 'outlier',
                                     exclude = c('x','y'), multiple = TRUE,
                                     methods = c('mixediqr',"mahal", 'glosh', 'onesvm')))
            #if methods are not provided
            expect_error(multidetect(data = refdata, var = 'bio61', output = 'outlier',
                                     exclude = c('x','y'), multiple = TRUE))

            #if the var is included in the exclude vector
            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                     exclude = c('x','y', "bio6"), multiple = TRUE))

            #if the out the output is wrong (clean and outlier) accepted
            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlyingpoints',
                                     exclude = c('x','y', "bio6"), multiple = TRUE))

            #"All methods indicated in the allowed methods-seqfenc is not in

            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                     exclude = c('x','y'), multiple = TRUE,
                                     methods = c('mixediqr', "iqr", "seqfenc")))
          })



#single species checks
spdata <- refdata[['Anguilla anguilla']]

testthat::test_that(desc = 'Not enough data provided and other methods may not work properly',
                    code = {
                      expect_warning(multidetect(data = spdata, var = 'bio6', output = 'outlier',
                                                 exclude = c('x','y'),
                                                 multiple = FALSE,
                                                 methods = c('mixediqr', "iqr", "kmeans", "mahal")))

                      #provide a list of dataframes but when multiple is FALSE returns an error-expects a dataframe
                      expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                               exclude = c('x','y'),
                                               multiple = FALSE,
                                               methods = c('mixediqr', "iqr", "kmeans", "mahal")))

                    })

#=============
#outlier output to test for data extraction
#==========
outlierdf <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                         exclude = c('x','y'),
                         multiple = TRUE,
                         methods = c('mixediqr', "iqr", "kmeans", "mahal"))

#outliers generated using a dataframe not list: var_col parameter is provided.

outlierdf2 <- multidetect(data = refdata_df, var = 'bio6', output = 'outlier',
                          exclude = c('x','y'),
                          var_col = 'species',
                          multiple = TRUE,
                          methods = c('mixediqr', "iqr", "kmeans", "mahal"))


#test for removing NAs when only univariate methods are selected
irisdata <- iris
#add outlier data and NAs
rowsOutNA <- data.frame(x= c(344, NA,NA, NA), x2 = c(34, 45, 544, NA), x3= c(584, 5, 554, NA),
                        x4 = c(575, 4554,474, NA), x5 =c('Setosa', 'Setosa', 'Setosa', "Setosa"))
colnames(rowsOutNA) <- colnames(irisdata)

dfinal <- rbind(irisdata, rowsOutNA)

test_that(desc= "Errors ",
          code = {
            seldf <- dfinal[, 4:5]
            #Not enough columns to run SDMs
            expect_error(multidetect(data = seldf, var = "Petal.Width", multiple = FALSE,
                                     methods = c('logboxplot', 'iqr', 'semiqr', 'hampel', 'mixediqr'),
                                     sdm = TRUE))
            expect_s4_class(multidetect(data = seldf, var = "Petal.Width", multiple = FALSE,
                                        methods = c('logboxplot', 'iqr', 'semiqr', 'hampel', 'mixediqr'),
                                        sdm = FALSE), 'datacleaner')
          })

test_that(desc = "NAs if univariate methods only selected",
          code = {
            #silence_true_errors FALSE but the methods are well set: datacleaner produced and verbose = TRUE

            outl <- suppressMessages(multidetect(data = dfinal, var = 'Sepal.Width',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','kmeans', 'lof'), verbose =TRUE,
                        silence_true_errors = FALSE))
            expect_s4_class(outl, 'datacleaner')

            #test that the singl species multidetect is printed
            expect_output(show(outl))

            #return an error if the method parameter are wrongly set: using kmeans, warn=TRUE & silence_true_errors=TRUE
            #method wrongly set
            expect_warning(multidetect(data = dfinal, var = 'Sepal.Width',
                       multiple = FALSE,
                       methods = c('mixediqr', 'logboxplot','kmeans'), warn=TRUE,
                       silence_true_errors = TRUE,
                       kmpar = list(method="silhoest")))

            #expect error if non numeric variable are provided as variable checks
            expect_error(multidetect(data = dfinal, var = 'Species',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.01))

            #expect error if data is not provided
            expect_error(multidetect(var = 'Sepal.Width',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.01))

            #NA removed during computation only but the rows are left in the data output.
            testthat::expect_message(dout <- multidetect(data = dfinal, var = 'Sepal.Length',
                                                         multiple = FALSE,
                                                         methods = c('mixediqr', 'logboxplot','iqr'), verbose = TRUE))
            testthat::expect_s4_class(dout, 'datacleaner')

            #check that the cleaned data has the NAs not removed mostly for univariate data

            cleandata <- extract_clean_data(refdata = dfinal, outliers = dout, threshold = 0.6)

            testthat::expect_true(any(is.na(cleandata$Sepal.Length)))

            #if missingness leads to removal of the main variable-- var: missingness set to 0.001
            testthat::expect_error(multidetect(data = dfinal, var = 'Sepal.Length',
                                     multiple = FALSE,
                                     methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.001))

            #execute successfully if the missigness is greater the proportional of missing values in the dataset.
            testthat::expect_s4_class(multidetect(data = dfinal, var = 'Sepal.Length',
                                        multiple = FALSE,
                                        methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.1), 'datacleaner')


          })


#outlier classification
#return errors with wrong specification
test_that(desc = "Errors for wrong specification or data",
          code = {
            #when clean data is output at the multidetect step
            cleanoutdf <- multidetect(data = dfinal, var = 'Sepal.Length', output = 'clean',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'))

            #error since x must datacleaner output but with outliers

            expect_error(ocindex(x = cleanoutdf, threshold = 0.6))

          })


testthat::test_that(desc = 'Test for success and errors during outlier detection and extraction',
                    code = {
                      #check out extract data

                      dx <- extractoutliers(x= outlierdf, sp=1)

                      expect_equal(nrow(dx), 4)#for each method

                      dxm <- extractoutliers(x=outlierdf)

                      #errors since no outliers in threshold of 0.8

                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = 0.8, warn = F))

                      #both threshold = NULL and autothreshold =FALSE
                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = NULL, warn = F, autothreshold = FALSE))

                      #both threshold = NOT NULL and autothreshold = TRUE
                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = 0.2, warn = F, autothreshold = TRUE))

                      #expect warning sinc the threshold is below 0.5
                      expect_warning(ocindex(x=outlierdf, sp=1, threshold = 0.2, warn = T))

                      #species index not provided
                      expect_error(ocindex(x=outlierdf, threshold = 0.2, warn = FALSE))

                      #threshold greater than 1
                      expect_error(ocindex(x = outlierdf, threshold = 1.2))

                      #threshold <less than 0 or = 0
                      expect_error(ocindex(x = outlierdf, threshold = -0.1))

                      #when the outliers x is not provided
                      expect_error(ocindex(threshold = -0.1))

                      #run autothreshold

                      #expect error: No absolute outliers from 0.51 to 1

                      expect_error(ocindex(x = outlist, autothreshold = TRUE, sp = 5))

                      #executed successfully with autothreshold

                      expect_type(ocindex(x = outlist, autothreshold = TRUE, sp = 1), 'character')

                    })

#preliminary for outlier extraction

test_that(desc = "Errors and success for extract outliers, extractoutliers, mult abs",
          code = {

            #expect error missing outlier output
            expect_error(extractoutliers())

            expect_error(extractoutliers())

            expect_error(multiabsolute())

            #expect error if datacleaner not provided
            expect_error(extractoutliers(x=wcd))

            expect_error(extractoutliers(x=wcd))

            expect_error(multiabsolute(x= wcd, threshold = 0.2))

            #expect error if clean output is set at multidetect

            cleanout <- multidetect(data = dfinal, var = 'Sepal.Length', output = 'clean',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'))

            expect_error(extractoutliers(cleanout))

            #expect error for multiple item at multidetect but extractoutliers is used

            expect_error(extractoutliers(x=cleanout))

            #expect error for multiabsolute extract on clean data
            expect_error(multiabsolute(x= cleanout, threshold = 0.2))

            outabs <- multidetect(data = dfinal, var = 'Sepal.Length', output = 'outlier',
                                    multiple = FALSE,
                                    methods = c('mixediqr', 'logboxplot','iqr'))

            #expect error for multiabsolute extract on outlying data but threshold >1
            expect_error(multiabsolute(x= outabs, threshold = 1.3))

            #true multiple absolute outlier extract for multiple species-- the multiple for ociindex

            expect_s3_class(multiabsolute(x= outlist, threshold = 0.5, props = FALSE), 'data.frame')

            expect_s3_class(multiabsolute(outlist, threshold = 0.4, props = TRUE), 'data.frame')

            expect_s3_class(multiabsolute(x= outlist, autothreshold = TRUE, props = FALSE), 'data.frame')

            expect_s3_class(multiabsolute(outlist, autothreshold = TRUE, props = TRUE), 'data.frame')
          })





testthat::test_that(desc = 'Test for different method if they return a character/suitable method',
                    code = {
                      testthat::expect_type(jaccard(x=outlierdf, sp=1, warn = F, threshold = 0.2), 'character')
                      testthat::expect_type(sorensen(x=outlierdf, sp=1,  warn = F, threshold = 0.2), 'character')
                      testthat::expect_type(cosine(x=outlierdf, sp=1, warn = F, threshold = 0.2), 'character')
                      testthat::expect_type(smc(x=outlierdf, sp=1,  warn = F, threshold = 0.2), 'character')
                      testthat::expect_type(hamming(x=outlierdf, sp=1,  warn = F, threshold = 0.2), 'character')
                      testthat::expect_type(overlap(x=outlierdf, sp=1,  warn = F, threshold = 0.2), 'character')

                      #check for absolute = TRUE: expect a vector
                      testthat::expect_type(ocindex(x=outlierdf, threshold = 0.2, absolute = TRUE, warn = F, sp = 1), 'double')

                      #check for absolute = TRUE and prop=TRUE: expect a dataframe
                      testthat::expect_s3_class(ocindex(x=outlierdf, threshold = 0.2, absolute = TRUE, warn = FALSE,
                                                        props = TRUE, sp = 1), 'data.frame')

                      #check for absolute = FALSE and prop=FALSE: expect a dataframe
                      testthat::expect_type(ocindex(x=outlierdf, threshold = 0.2, absolute = FALSE, props = FALSE,
                                                        warn = FALSE, sp=1 ), 'character')

                      #expect error: wrong data set for outliers. only accepts datacleaner
                      testthat::expect_error(ocindex(x=wcd, threshold = 0.2, absolute = TRUE, warn = F, sp = 1))

                    })

#best methods checks

testthat::test_that(desc = "Success and errors expected and checked for best method selection.",
                    code = {
                      #one species
                      expect_type(bestmethod(x=outlierdf, sp=1, threshold = 0.2, warn = F, verbose = F),'character')

                      #expect error if the index is not provided yet the outlierdf is from multiple object.
                      expect_error(bestmethod(x=outlierdf, threshold = 0.2, warn = F, verbose = F))

                      #expect error if x is not provided.
                      expect_error(bestmethod(threshold = 0.2, warn = F, verbose = F, sp = 1))


                      #threshold greater than 1
                      expect_error(bestmethod(x = outlierdf, threshold = 1.2, sp = 1))

                      #threshold <less than 0 or = 0

                      #threshold greater than 1
                      expect_error(bestmethod(x = outlierdf, threshold = 0-1, sp = 1))

                      expect_success(expect_type(bestmethod(x = outlierdf, threshold = 0.2, sp = 1), 'character'))


                      #multiple species, expect dataframe
                      expect_s3_class(multibestmethod(x=outlierdf, threshold = 0.2, warn = F, verbose = F),
                                      'data.frame')

                      #use the one species for Anguilla anguilla (spdata): expect error because this can be extracted in
                      #bestmethod function not here with multiple species
                      out3 <-suppressWarnings(multidetect(data = spdata, output = 'outlier', var = 'bio6',
                                          methods = c('logboxplot', 'iqr', 'mixediqr'), multiple = FALSE))
                      expect_error(multibestmethod(x = out3, threshold = 0.2))
                    })










