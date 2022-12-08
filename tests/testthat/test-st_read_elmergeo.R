test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("st_read_elmergeo('county_background') does not return an error", {
  expect_error(relmer::st_read_elmergeo('county_background'), NA)
})

test_that("st_read_elmergeo('fakefake') does return an error of type 'no layer error'", {
  expect_error(relmer::st_read_elmergeo('fakefake'), "no layer error")
})


test_that('st_read_elmergeo() returns a layer with srid 4326', {
  lyr = relmer::st_read_elmergeo('COUNTY_BACKGROUND')
  lyr.crs = sf::st_crs(lyr)
  lyr.srid = lyr.crs[[1]]
  expect_equal(lyr.srid, "EPSG:4326")
})

