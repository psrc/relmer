test_that("st_read_elmergeo('county_background') does not return an error", {
  expect_error(psrcelmer::st_read_elmergeo('county_background'), NA)
})


test_that("st_read_elmergeo('fakefake') does return an error of type 'no layer error'", {
  expect_error(psrcelmer::st_read_elmergeo('fakefake'), "no layer error")
})


test_that('st_read_elmergeo() returns a layer with srid 4326', {
  lyr = psrcelmer::st_read_elmergeo('COUNTY_BACKGROUND')
  lyr.crs = sf::st_crs(lyr)
  lyr.srid = lyr.crs[[1]]
  expect_equal(lyr.srid, "EPSG:4326")
})


test_that('st_read_elmergeo(project_to_wgs84=FALSE) returns a layer with srid 2285', {
  lyr = psrcelmer::st_read_elmergeo('COUNTY_BACKGROUND', project_to_wgs84 = FALSE)
  lyr.crs = sf::st_crs(lyr)
  lyr.srid = lyr.crs[[1]]
  expect_equal(lyr.srid, "EPSG:2285")
})

test_that("check_sql_driver('bogus_driver') returns an error", {
  expect_error(psrcelmer::check_sql_driver('bogus_driver'))
})
