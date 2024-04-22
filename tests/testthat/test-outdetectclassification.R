
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

db <- sf::read_sf(system.file('extdata/danube/basinfinal.shp', package = "specleanr"), quiet = TRUE)


#extract data

refdata <- pred_extract(data = matchclean, raster = wcd,
                        lat = 'decimalLatitude', lon = 'decimalLongitude',
                        basin = db,
                        colsp = 'speciescheck',
                        list = TRUE,
                        verbose = F,
                        minpts = 6,
                        merge = F)


test_that(desc = 'List of species with outliers produced',
          code = {
            outlist <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                   exclude = c('x','y'),
                                   multiple = TRUE,
                                   methods = c('mixediqr', "iqr",
                                               "kmeans", "mahal"))
            expect_equal(length(outlist@result), length(refdata))
          })

#when the data is a data frame

test_that(desc = 'Success when reference of precleaned data is a dataframe not list',
          code = {
            refdata2 <- pred_extract(data = matchclean, raster = wcd,
                                     lat = 'decimalLatitude', lon = 'decimalLongitude',
                                     basin = db,
                                     colsp = 'speciescheck',
                                     list = FALSE,
                                     verbose = F,
                                     minpts = 6,
                                     merge = F)

            outlist2 <- multidetect(data = refdata2, var = 'bio6', output = 'outlier',
                                    exclude = c('x','y', 'species'),
                                    multiple = TRUE,
                                    colsp = 'species',
                                    methods = c('mixediqr', "iqr",
                                                "kmeans", "mahal"))
            expect_s3_class(refdata2, 'data.frame')

            expect_equal(length(outlist2@result), length(unique(refdata2$species)))
          }
)


test_that(desc = 'Not enough data provided and other methods may not work properly',
          code = {
            #filter one species

            spdata <- refdata[['Anguilla anguilla']]
            expect_warning(multidetect(data = spdata, var = 'bio6', output = 'outlier',
                                       exclude = c('x','y', 'species'),
                                       multiple = FALSE,
                                       methods = c('mixediqr', "iqr",
                                                   "kmeans", "mahal")))
          })

test_that(desc = 'Check out outliers for one species and multiple',
          code = {
            #check out

            outlist2 <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                    exclude = c('x','y'),
                                    multiple = TRUE,
                                    methods = c('mixediqr', "iqr",
                                                "kmeans", "mahal"))
            dx <- ext_outliers(x= outlist2, sp=1)

            expect_equal(nrow(dx), 4)#for each method

            dxm <- batch_extract(x=outlist2)

            expect_gt(length(unique(dxm$speciesname)), 4)
          })

#outlier classification
test_that(desc = 'Errors and warnings when the threshold is low or high',
          code = {
            out3 <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                   exclude = c('x','y'),
                                   multiple = TRUE,
                                   methods = c('mixediqr', "iqr",
                                               "kmeans", "mahal"))
            expect_error(ocindex(x=out3, sp=1, var = 'bio6', threshold = 0.8, warn = F))

            expect_warning(ocindex(x=out3, sp=1, var = 'bio6', threshold = 0.2, warn = T))
          })

test_that(desc = 'Test for different method if they return a character/suitable method',
          code = {
            out3 <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                exclude = c('x','y'),
                                multiple = TRUE,
                                methods = c('mixediqr', "iqr",
                                            "kmeans", "mahal"))
            expect_type(jaccard(x=out3, sp=1, var = 'bio6', warn = F, threshold = 0.2), 'character')
            expect_type(sorensen(x=out3, sp=1, var = 'bio6', warn = F, threshold = 0.2), 'character')
            expect_type(cosine(x=out3, sp=1, var = 'bio6', warn = F, threshold = 0.2), 'character')
            expect_type(smc(x=out3, sp=1, var = 'bio6', warn = F, threshold = 0.2), 'character')
            expect_type(hamming(x=out3, sp=1, var = 'bio6', warn = F, threshold = 0.2), 'character')
            expect_type(overlap(x=out3, sp=1, var = 'bio6', warn = F, threshold = 0.2), 'character')

            #one species
            expect_type(bm(x=out3, var = 'bio6', sp=1, threshold = 0.2, mode = 'all', warn = F, verbose = F),'character')

            #multple species, expect dataframe
            expect_s3_class(mult_bm(x=out3, var = 'bio6', threshold = 0.2, warn = F, verbose = F), 'data.frame')

          })
#test for suitable method for all similarity measures
test_that(desc = 'Extract data after discarding absolute outliers',
          code = {
            out3 <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                exclude = c('x','y'),
                                multiple = TRUE,
                                methods = c('mixediqr', "iqr",
                                            "kmeans", "mahal"))
            dxx <- multextract(data = refdata, outliers = out3, threshold = 0.2, warn = F, verbose = F,
                               var = 'bio6')
            expect_s3_class(dxx, 'data.frame')
            expect_equal(length(unique(dxx$species)), length(out3@result))
          })
