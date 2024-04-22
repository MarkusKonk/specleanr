

data("jdsdata")

sp <- check_names(data = jdsdata, colsp = 'speciesname', verbose = F, pct = 90, merge = T, sn = FALSE)

db <- sf::st_read(dsn=system.file('extdata/danube/basinfinal.shp', package='specleanr'), quiet=TRUE)

zz <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))


test_that(desc = 'Less number of records after discarding duplicates and missing values.',
          code = {

            expect_error(
              pred_extract(data = sp,
                           raster= zz ,
                           lat ='lat',
                           lon = 'lon',
                           colsp = 'speciescheck',
                           basin = db,
                           multiple = TRUE, verbose = F,
                           list= TRUE,
                           minpts = 10, merge=T))
          }
)
