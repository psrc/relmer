#' get_table(db_name, tbl_name)
#' 
#' Retrieve a dataset defined by a table name.
#'
#' @param db_name String.  The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".
#' @param schema String.  The name of the schema that the table exists in.  No default.
#' @param tbl_name String.  The name of the table to be retrieved.  No default.
#' @return A data frame.
#'
#' @examples
#' get_table('chas', 'tenure_dim')
#' 
#' @export
get_table <- function(db_name = 'Elmer', schema, tbl_name) {
  
  tryCatch({
    conn <- get_conn(dbname = db_name)
    check_for_schema(conn, schema = schema, db_name = db_name)
    check_for_table(conn, schema = schema, tbl_name = tbl_name)
    full_tbl_name <- glue::glue("{schema}.{tbl_name}")
    df <- DBI::dbReadTable(conn, DBI::SQL(full_tbl_name))
    DBI::dbDisconnect(conn)
    return(df)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_table: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_table: {e}"))
    stop(e)
  })
}

check_for_schema <- function(conn, schema, db_name) {
  tryCatch({
    sql = glue::glue("SELECT SCHEMA_NAME ",
                     "FROM INFORMATION_SCHEMA.SCHEMATA ",
                     "WHERE SCHEMA_NAME = '{schema}'" )
    df <- DBI::dbGetQuery(conn = conn, DBI::SQL(sql))
    if (nrow(df) == 0) {
      msg <- glue::glue("Schema {schema} does not exist in the database {db_name}.")
      stop(msg)
    }
  }, warning = function(w) {
    print(glue::glue("A warning popped up in check_for_schema: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in check_for_schema: {e}"))
    stop(e)
  })
}


check_for_table <- function(conn, schema, tbl_name) {
  tryCatch({
    sql = glue::glue("SELECT TABLE_NAME ",
                     "FROM INFORMATION_SCHEMA.TABLES ",
                     "WHERE TABLE_SCHEMA = '{schema}' ",
                     "AND TABLE_NAME = '{tbl_name}'")
    df <- DBI::dbGetQuery(conn = conn, DBI::SQL(sql))
    if (nrow(df) == 0) {
      msg <- glue::glue("Table {tbl_name} does not exist in the schema {schema}.")
      stop(msg)
    }
  }, warning = function(w) {
    print(glue::glue("A warning popped up in check_for_table: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in check_for_table: {e}"))
    stop(e)
  })
}
