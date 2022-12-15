test_that('list_layers() returns a dataframe', {
  df <- psrcelmer::list_layers()
  expect_equal(class(df), 'data.frame')
})

test_that('list_layers() accepts a parameter feature_dataset', {
  fd <- 'census'
  df <- psrcelmer::list_layers(feature_dataset = fd)
  expect_equal(class(df), 'data.frame')
})

test_that('list_layers() returns a dataframe with more than 100 rows', {
  df <- psrcelmer::list_layers()
  rowcount <- nrow(df)
  expect_gt(rowcount, 100)
})

test_that('the feature_dataset para in list_layers() actually filters the data frame', {
  df_full <- psrcelmer::list_layers()
  df_filtered <- psrcelmer::list_layers('census')
  rowcount_full <- nrow(df_full)
  rowcount_filtered <- nrow(df_filtered)
  expect_gt(rowcount_full, rowcount_filtered)
})
