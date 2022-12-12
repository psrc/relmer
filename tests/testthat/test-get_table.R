test_that('get_table("chas", "tenure_dim") returns a dataframe', {
  scm <- "chas"
  t_name <- "tenure_dim"
  df <- psrcelmer::get_table(db_name = 'Elmer', schema = scm, tbl_name = t_name)
  expect_equal(class(df), 'data.frame')
})

test_that("get_table('bogus_schema', 'bogus_table') returns a bad-schema error", {
  db = "Elmer"
  expect_error(psrcelmer::get_table(schema = 'bogus_schema', tbl_name = 'bogus_name'), 'Schema bogus_schema does not exist in the database Elmer' )
})

test_that("get_table('census', 'bogus_table') returns a bad-table error", {
  db = "Elmer"
  expect_error(psrcelmer::get_table(schema = 'census', tbl_name = 'bogus_name'), 'Table bogus_name does not exist in the schema census' )
})
