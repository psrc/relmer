#' send_query(db_name, sql)
#'
#' Execute a SQL command to the database
#'
#' Intended for update, merge or other commands that do not retrieve records. For retrieval, use get_query.
#'
#' @param db_name String.  The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".
#' @param sql String.  The SQL command to send to <db_name>.
#'
#' @export
sql_execute <- function(sql, db_name='Elmer') {

  tryCatch({
    conn <- get_conn(dbname = db_name)
    DBI::dbExecute(conn=conn, statement=DBI::SQL(sql))
    DBI::dbDisconnect(conn)
    return(invisible(NULL))
  }, warning = function(w) {
    print(glue::glue("A warning popped up in send__query: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in send__query: {e}"))
    stop(e)
  })
}

sql_execute_elmer <- function(sql) {

  tryCatch({
    sql_execute(sql, 'Elmer')
  }, warning = function(w) {
    print(glue::glue("A warning popped up in send__query_elmer: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in send__query_elmer: {e}"))
    stop(e)
  })
}

#' stage_table(df, table_name)
#'
#' Write a dataframe as a table within the Elmer.stg schema
#'
#' @param df dataframe - the data object to write
#' @param table_name string - the name you want given the database table
#'
#' @export
stage_table <- function(df, table_name=deparse(substitute(df))) {

  tryCatch({
    conn <- get_conn(dbname = "Elmer")
    table_id <- DBI::Id(schema="stg", table=table_name)
    DBI::dbWriteTable(conn, table_id, df, overwrite=TRUE)
    DBI::dbDisconnect(conn)
    return(invisible(NULL))
  }, warning = function(w) {
    print(glue::glue("A warning popped up in send__query: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in send__query: {e}"))
    stop(e)
  })
}

