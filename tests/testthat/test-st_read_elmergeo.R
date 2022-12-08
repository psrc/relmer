test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("st_read_elmergeo('county_background') does not return an error", {
  expect_error(relmer::st_read_elmergeo('county_background'), NA)
})

test_that("st_read_elmergeo('fakefake') does return an error", {
  expect_error(relmer::st_read_elmergeo('fakefake'), "no layer error")
})


test_that('errors can occur', {
  expect_error(1/"a")
})
