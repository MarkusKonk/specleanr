
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

db <- sf::read_sf(system.file('extdata/danube/basinfinal.shp',
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

testthat::test_that(desc = 'List of species with outliers produced',
                    code = {
                      outlist <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                             exclude = c('x','y'),
                                             multiple = TRUE,
                                             methods = c('mixediqr', "iqr", "seqfences",
                                                         "mahal", 'glosh', 'onesvm','logboxplot','knn',
                                                         'distboxplot','medianrule',
                                                         'iforest','jknife','reference'))

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
                    })


#test trycatch errors

test_that(desc = "Return errors and no errors",
          code = {
            #expect warning when the kmpar for kmeans the k is less than 2 and showErrors = TRUE
            #that some methods particularly kmeans did not execute
            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                     exclude = c('x','y'),
                                     multiple = TRUE,
                                     methods = c('mixediqr', "iqr", "seqfences",
                                                 "mahal", 'glosh', 'onesvm','kmeans'),
                                     kmpar= list(k = 1, method = "silhouette", mode = "soft"),
                                     showErrors = TRUE))

            #expect to hide the error/warning in the hood when showErrors is FALSE and expect datacleaner

            outlierFALSE <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                        exclude = c('x','y'),
                                        multiple = TRUE,
                                        methods = c('mixediqr',"mahal", 'glosh', 'onesvm'),
                                        kmpar= list(k = 1, method = "silhouette", mode = "soft"),
                                        showErrors = TRUE)
            expect_s4_class(outlierFALSE, 'datacleaner')

            #extract the outlier for species index = 2 to confirm that kmeans is not among the methods
            dfout <- extract_outliers(x= outlierFALSE, sp = 2)

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

#outliers generated using a dataframe not list: colsp parameter is provided.

outlierdf2 <- multidetect(data = refdata_df, var = 'bio6', output = 'outlier',
                          exclude = c('x','y'),
                          colsp = 'species',
                          multiple = TRUE,
                          methods = c('mixediqr', "iqr", "kmeans", "mahal"))


#test for removing NAs when only univariate methods are selected
irisdata <- iris
#add outlier data and NAs
rowsOutNA <- data.frame(x= c(344, NA,NA, NA), x2 = c(34, 45, 544, NA), x3= c(584, 5, 554, NA),
                        x4 = c(575, 4554,474, NA), x5 =c('Setosa', 'Setosa', 'Setosa', "Setosa"))
colnames(rowsOutNA) <- colnames(irisdata)

dfinal <- rbind(irisdata, rowsOutNA)

test_that(desc = "NAs if univariate methods only selected",
          code = {
            #showErrrors FALSE but the methods are well set: datacleaner produced and verbose = TRUE

            expect_s4_class(suppressMessages(multidetect(data = dfinal, var = 'Sepal.Width',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','kmeans', 'lof'), verbose =TRUE,
                        showErrors = FALSE)), 'datacleaner')


            #return an error if the method parameter are wrongly set: using kmeans, warn=TRUE & showError=FALSE
            #method wrongly set
            expect_warning(multidetect(data = dfinal, var = 'Sepal.Width',
                       multiple = FALSE,
                       methods = c('mixediqr', 'logboxplot','kmeans'), warn=TRUE,
                       showErrors = FALSE,
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

            cleandata <- clean_data_extract(refdata = dfinal, outliers = dout, threshold = 0.6)

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

                      dx <- extract_outliers(x= outlierdf, sp=1)

                      expect_equal(nrow(dx), 4)#for each method

                      dxm <- batch_extract(x=outlierdf)

                      expect_gt(length(unique(dxm$speciesname)), 4) #

                      #errors since no outliers in threshold of 0.8

                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = 0.8, warn = F))

                      #both threshold = NULL and autothreshold =FALSE
                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = NULL, warn = F, autothreshold = FALSE))

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



#test for suitable method for all similarity measures
testthat::test_that(desc = 'Data extraction: errors and success',
                    code = {
                      dxx <- clean_data_extract(refdata = refdata, outliers = outlierdf, threshold = 0.2)

                      #dataframe produced
                      testthat::expect_s3_class(dxx, 'data.frame')

                      #species in the cleandata equal to species in outlier df
                      testthat::expect_equal(length(unique(dxx$species)), length(outlierdf@result))

                      #if both threshold and loess are provided: error

                      expect_error(clean_data(data = refdata, outliers = outlierdf, threshold = 0.6, sp = 1,
                                                      loess = TRUE))
                      #expect error if autothreshold is FALSE and threshold is NULL and loess is FALSE

                      expect_error(clean_data(data = refdata, outliers = outlierdf, threshold = NULL,
                                                      loess = FALSE, autothreshold = FALSE))

                      #expect error if refdata and refdata used in outliers is different

                      expect_error(clean_data_extract(data = spdata, outliers = outlierdf, threshold = 0.3))

                      #expect error if the threshold used does not capture absolute outliers

                      expect_error(clean_data(data = refdata, outliers = outlierdf, threshold = 1, sp = 1))

                      #expect error if the mode entered is wrong

                      expect_error(clean_data(data = refdata, outliers = outlierdf, mode = 'abslute', sp = 1))

                      #if colsp is not provided yet refdata is a dataframe not a list

                      expect_error(clean_data_extract(refdata = refdata_df, outliers = outlierdf2, threshold = 0.2))

                    })


#test thresh search using loess method

testthat::test_that(desc = "Check for possible errors threshold optimal",
                    code = {

                      #expect names minima and maxima

                      expect_named(thresh_search(data = refdata[["Phoxinus phoxinus"]], outliers = outlierdf, sp = "Phoxinus phoxinus"), c('minima', 'maxima'))

                      #expect error if the multiple speces reference data is provided for thresh_search

                      expect_error(thresh_search(data = refdata, outliers = outlierdf))

                      #expect error if column name is not provided and yet reference data is a dataframe not list

                      expect_error(optimal_threshold(data = refdata_df, outlierdf2, sp = 1))

                      #expect a dataframe but no plot for multiple species. even plot = TRUE

                      expect_s3_class(optimal_threshold(refdata = refdata, outliers = outlierdf,
                                                        plot = TRUE, colsp = 'species'), 'data.frame')

                      #expect unused argument error for sp index or name for multiple threshold search

                      expect_error(optimal_threshold(refdata = refdata, outliers = outlierdf, sp = 1, colsp = 'species'))

                    })









