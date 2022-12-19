test_that('list_feature_classes() returns a dataframe', {
  df <- psrcelmer::list_feature_classes()
  expect_equal(class(df), 'data.frame')
})

test_that('list_feature_classes() accepts a parameter feature_dataset', {
  fd <- 'census'
  df <- psrcelmer::list_feature_classes(feature_dataset = fd)
  expect_equal(class(df), 'data.frame')
})

test_that('list_feature_classes() accepts a parameter feature_class', {
  fc <- 'URBAN_CENTERS'
  df <- psrcelmer::list_feature_classes(feature_class = fc)
  expect_equal(class(df), 'data.frame')
})

test_that('list_feature_classes() returns a dataframe with more than 100 rows', {
  df <- psrcelmer::list_feature_classes()
  rowcount <- nrow(df)
  expect_gt(rowcount, 100)
})

test_that('the feature_dataset param in list_feature_classes() actually filters the data frame', {
  df_full <- psrcelmer::list_feature_classes()
  df_filtered <- psrcelmer::list_feature_classes('census')
  rowcount_full <- nrow(df_full)
  rowcount_filtered <- nrow(df_filtered)
  expect_gt(rowcount_full, rowcount_filtered)
})

test_that('feature_dataset(feature_class="URBAN_CENTERS") returns a dataframe with exactly one record', {
  df <- psrcelmer::list_feature_classes(feature_class='URBAN_CENTERS')
  rowcount <- nrow(df)
  expect_equal(1, rowcount)
})
