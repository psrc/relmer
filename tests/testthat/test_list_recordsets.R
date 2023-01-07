test_that("list_recordsets() returns a dataframe", {
  df <- psrcelmer::list_recordsets()
  expect_equal(class(df), 'data.frame')
})
