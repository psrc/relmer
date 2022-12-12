test_that('get_table("chas", "tenure_dim") returns a dataframe', {
  scm <- "chas"
  t_name <- "tenure_dim"
  df <- psrcelmer::get_table(db_name = 'Elmer', schema = scm, tbl_name = t_name)
  expect_equal(class(df), 'data.frame')
})

test_that("get_table('bogus_name') returns an error", {
  expect_error(psrcelmer::get_table(schema = 'bogus_schema', tbl_name = 'bogus_name'))
})
