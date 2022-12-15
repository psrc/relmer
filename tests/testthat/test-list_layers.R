test_that('list_layers() returns a dataframe', {
  df <- psrcelmer::list_layers()
  expect_equal(class(df), 'data.frame')
})

test_that('list_layers() accepts a parameter feature_dataset', {
  fd <- 'census'
  df <- psrcelmer::list_layers(feature_dataset = fd)
  expect_equal(class(df), 'data.frame')
})