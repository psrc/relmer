#' get_query(db_name, sql)
#' 
#' Retrieve a dataset defined by a SQL string
#'
#' @param db_name String.  The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".
#' @param sql String.  The SQL command to send to <db_name>.
#' @return A data frame.
#' 
#' @examples
#' get_query("select * from chas.tenure_dim")
#' get_query(db_name = "ElmerGeo", sql = "select geoid10 from dbo.TRACT2010")
#'
#' @export
get_query <- function(db_name = 'Elmer', sql) {
  
  tryCatch({
    conn <- get_conn(dbname = db_name)
    df <- DBI::dbGetQuery(conn, DBI::SQL(sql))
    DBI::dbDisconnect(conn)
    return(df)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_query: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_query: {e}"))
    stop(e)
  })
}

get_query_elmer <- function(sql) {
  
  tryCatch({
    df <- get_query('elmer', sql)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_query_elmer: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_query_elmer: {e}"))
    stop(e)
  })
}