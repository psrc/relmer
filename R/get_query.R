#' get_query(sql, db_name)
#'
#' Retrieve a dataset defined by a SQL string
#'
#' @param sql String.  The SQL command to send to <db_name>.
#' @param db_name String.  The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".
#' @return A data frame.
#'
#' @examples
#' get_query(sql = "select * from chas.tenure_dim")
#' get_query(sql = "select top 10 geoid10 from dbo.TRACT2010", db_name = "ElmerGeo")
#'
#' @export
get_query <- function(sql, db_name = 'Elmer') {

  tryCatch({
    conn <- get_conn(dbname = db_name)
    df <- DBI::dbGetQuery(conn=conn, statement=DBI::SQL(sql))
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
    df <- get_query(sql, 'Elmer')
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_query_elmer: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_query_elmer: {e}"))
    stop(e)
  })
}
