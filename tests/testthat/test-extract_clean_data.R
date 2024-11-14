

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

#single species checks
spdata <- refdata[['Anguilla anguilla']]

outlierdf <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                         exclude = c('x','y'),
                         multiple = TRUE,
                         methods = c('mixediqr', "iqr", "kmeans", "mahal"))


#test for suitable method for all similarity measures
test_that(desc = 'Data extraction: errors and success',
                    code = {
                      dxx <- extract_clean_data(refdata = refdata, outliers = outlierdf, threshold = 0.2)

                      #dataframe produced
                      testthat::expect_s3_class(dxx, 'data.frame')

                      #species in the cleandata equal to species in outlier df
                      testthat::expect_equal(length(unique(dxx$species)), length(outlierdf@result))

                      #if both threshold and loess are provided: error

                      expect_error(cleandata(data = refdata, outliers = outlierdf, threshold = 0.6, sp = 1,
                                              loess = TRUE))
                      #expect error if autothreshold is FALSE and threshold is NULL and loess is FALSE

                      expect_error(cleandata(data = refdata, outliers = outlierdf, threshold = NULL,
                                              loess = FALSE, autothreshold = FALSE))

                      #expect error if refdata and refdata used in outliers is different

                      expect_error(extract_clean_data(data = spdata, outliers = outlierdf, threshold = 0.3))

                      #expect error if the threshold used does not capture absolute outliers

                      expect_error(cleandata(data = refdata, outliers = outlierdf, threshold = 1, sp = 1))

                      #expect error if the mode entered is wrong

                      expect_error(cleandata(data = refdata, outliers = outlierdf, mode = 'abslute', sp = 1))

                      #if colsp is not provided yet refdata is a dataframe not a list

                      expect_error(extract_clean_data(refdata = refdata_df, outliers = outlierdf2, threshold = 0.2))

                    })


#test thresh search using loess method

testthat::test_that(desc = "Check for possible errors threshold optimal",
                    code = {

                      #expect names minima and maxima

                      expect_named(thresh_search(data = refdata[["Phoxinus phoxinus"]], outliers = outlierdf,
                                                 sp = "Phoxinus phoxinus"), c('minima', 'maxima'))

                      #expect an output map if plot is true
                      expect_named(thresh_search(data = refdata[["Phoxinus phoxinus"]], outliers = outlierdf,
                                                 sp = "Phoxinus phoxinus", plot = TRUE), c('minima', 'maxima'))

                      ##expect error, warn, and next while optimizing span
                      expect_named(thresh_search(data = refdata[["Phoxinus phoxinus"]], outliers = outlierdf,
                                                 sp = "Phoxinus phoxinus", tuneLoess = seq(0.1, 1, 0.1)),
                                   c('minima', 'maxima'))

                      #expect error if the multiple species reference data is provided for thresh_search

                      expect_error(thresh_search(data = refdata, outliers = outlierdf))

                      #optimal threshold for one species
                      spoutliers <- suppressWarnings(multidetect(data = spdata, var = 'bio6', output = 'outlier',
                                                                 exclude = c('x','y'),
                                                                 multiple = FALSE,
                                                                 methods = c('mixediqr', "iqr", "kmeans", "mahal")))

                      expect_named(optimal_threshold(refdata = spdata, outliers = spoutliers), c('minima', 'maxima'))

                      #expect error if column name is not provided and yet reference data is a dataframe not list

                      expect_error(optimal_threshold(data = refdata_df, outliers = outlierdf2))

                      #expect a dataframe but no plot for multiple species. even plot = TRUE

                      expect_s3_class(optimal_threshold(refdata = refdata, outliers = outlierdf,
                                                        plot = TRUE, colsp = 'species'), 'data.frame')

                      #expect unused argument error for sp index or name for multiple threshold search

                      expect_error(optimal_threshold(refdata = refdata, outliers = outlierdf, sp = 1, colsp = 'species'))

                      #check if the same refdata was used in outlier detection threshold optimization

                      expect_error(optimal_threshold(refdata = spdata, outliers = outlierdf))

                      #use wrong reference datat

                      expect_error(optimal_threshold(refdata = wcd, outliers = outlist))

                    })
