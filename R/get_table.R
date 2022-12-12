#' get_table(db_name, tbl_name)
#' 
#' Retrieve a dataset defined by a table name.
#'
#' @param db_name String.  The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".
#' @param schema String.  The name of the schema that the table exists in.  No default.
#' @param tbl_name String.  The name of the table to be retrieved.  No default.
#' @return A data frame.
#'
#' @export
get_table <- function(db_name = 'Elmer', schema, tbl_name) {
  
  tryCatch({
    full_tbl_name <- glue::glue("{schema}.{tbl_name}")
    conn <- get_conn(dbname = db_name)
    df <- DBI::dbReadTable(conn, DBI::SQL(full_tbl_name))
    return(df)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_table: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_table: {e}"))
    stop(e)
  })
}