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
send_query <- function(db_name = 'Elmer', sql) {

  tryCatch({
    conn <- get_conn(dbname = db_name)
    DBI::dbSendQuery(conn, DBI::SQL(sql))
    DBI::dbDisconnect(conn)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in send__query: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in send__query: {e}"))
    stop(e)
  })
}

send_query_elmer <- function(sql) {

  tryCatch({
    send_query('elmer', sql)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in send__query_elmer: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in send__query_elmer: {e}"))
    stop(e)
  })
}
