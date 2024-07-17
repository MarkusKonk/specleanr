
data(jdsdata)
data(efidata)

#Danube basin polygon

db <- sf::st_read(system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)


matchdata <- match_datasets(datasets = list(jds = jdsdata, efi = efidata),
                            lats = 'lat',
                            lons = 'lon',
                            species = c('speciesname','scientificName'),
                            country= c('JDS4_site_ID'),
                            date=c('sampling_date', 'Date'))

datacheck <- check_names(matchdata, colsp= 'species', pct = 90, merge =TRUE)


worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))

rdata <- pred_extract(data = datacheck,
                      raster= worldclim ,
                      lat = 'decimalLatitude',
                      lon= 'decimalLongitude',
                      colsp = 'speciescheck',
                      bbox = db,
                      multiple = TRUE,
                      minpts = 10,
                      list=TRUE,
                      merge=F)



out_df <- multidetect(data = rdata, multiple = TRUE,
                      var = 'bio6',
                      output = 'outlier',
                      exclude = c('x','y'),
                      methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))

#extracting optimal threshold for each species

test_that(desc = "Data frame of minima and maxima for 9 species",
          code = {
            df <- optimal_threshold(refdata = rdata, outliers = out_df)

            expect_s3_class(df, "data.frame")
            expect_equal(nrow(df), 9)
          })
#test for one species

thymallus <- datacheck[datacheck$species=="Thymallus thymallus",]

rdata1 <- pred_extract(data = thymallus,
                      raster= worldclim ,
                      lat = 'decimalLatitude',
                      lon= 'decimalLongitude',
                      bbox = db,
                      colsp = 'speciescheck',
                      multiple = FALSE,
                      minpts = 10,
                      list=TRUE,
                      merge=F)

#supress warning of not enough data
toutliers1 <- suppressWarnings(multidetect(data = rdata1, multiple = FALSE,
                                           var = 'bio6',
                                           output = 'outlier',
                                           exclude = c('x','y'),
                                           methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel')))


test_that(desc = "One species optimal threshold checks.",
          code = {
            minmax <- thresh_search(data = rdata1, outliers = toutliers1)
            expect_equal(length(minmax), 2)
            expect_type(class(toutliers1), 'character')
            expect_s3_class(rdata1, 'data.frame')

          })



