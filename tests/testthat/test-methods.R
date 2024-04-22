
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
                        basin = db,
                        colsp = 'speciescheck',
                        list = TRUE,
                        verbose = F,
                        minpts = 6,
                        merge = F)

sp <- refdata[['Anguilla anguilla']]


test_that('z-score tests',
          code = {
            dx <- zscore(data = sp, var = 'bio6', output = 'outlier', type = 'mild', mode = 'soft')
            expect_s3_class(dx, 'data.frame')
          })

test_that('Adjusted boxplots tests',
          code = {
            dx1 <- adjustboxplots(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx1, 'data.frame')
          })

test_that('Interquartile range tests',
          code = {
            dx2 <- interquartile(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx2, 'data.frame')
          })

test_that('Semi interquartile range tests',
          code = {
            dx3 <- semiIQR(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx3, 'data.frame')
          })

test_that('Hampel Filters tests',
          code = {
            dx4 <- hampel(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx4, 'data.frame')
          })

test_that('Reverse Jack Knifing tests',
          code = {
            dx5 <- jknife(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx5, 'data.frame')
          })

test_that('Log boxplot tests',
          code = {
            dx6 <- logboxplot(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx6, 'data.frame')
          })

test_that('Mixed Interquartile range tests',
          code = {
            dx7 <- mixediqr(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx7, 'data.frame')
          })

test_that('Median Rule tests',
          code = {
            dx8 <- medianrule(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx8, 'data.frame')
          })

test_that('Median Rule tests',
          code = {
            dx8 <- medianrule(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx8, 'data.frame')
          })

test_that('Distribution boxplot tests',
          code = {
            dx9 <- distboxplot(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx9, 'data.frame')
          })

test_that('Sequential fences tests',
          code = {
            dx10 <- seqfences(data = sp, var = 'bio6', output = 'outlier')
            expect_s3_class(dx10, 'data.frame')
          })







































