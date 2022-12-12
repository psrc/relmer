test_that('get_query() returns a dataframe', {
  sql <- "select * from chas.tenure_dim"
  df <- psrcelmer::get_query(db_name = 'Elmer', sql = sql)
  expect_equal(class(df), 'data.frame')
})
