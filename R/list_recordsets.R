build_recordset_sql <- function(schema_name='', include_base_tables=FALSE) {

  tryCatch({
    base_query <- glue::glue("select t.TABLE_SCHEMA as [schema],
              T.TABLE_NAME as recordset_name,
              T.TABLE_TYPE AS recordset_type
            from INFORMATION_SCHEMA.TABLES t
            where table_schema not in ('dbo', 'tSQLt', 'DBA', 'meta', 'stg')")
    if (schema_name != '') {
      where_schema_sql <- glue::glue("AND t.TABLE_SCHEMA = '{schema_name}'")
      base_query <- glue::glue("{base_query} {where_schema_sql}")
    }
    if (!(include_base_tables)) {
      type_sql <- glue::glue("AND t.TABLE_TYPE = 'view'")
      base_query <- glue::glue("{base_query} {type_sql}")
    }
    return(base_query)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_sql: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_sql: {e}"))
  })
}


#' list_recordsets(schema_name, recordset_type)
#'
#' return a list of tables and views available in PSRC's in-house data warehouse
#'
#' @param schema_name (string, optional) If supplied, this limits the returned list to only those tables and views in schema_name.
#' @param include_base_tables (TRUE|FALSE, optional) If false, return only the views (no tables).  False is the default.
#' @return a list of tables, in the form of a dataframe
#'
#' @examples
#' list_recordsets()
#'
#' list_recordsets(schema_name="HHSurvey")
#'
#' list_recordsets(schema_name="HHSurvey", include_base_tables=TRUE)
#'
#' @export
list_recordsets <- function(schema_name='', include_base_tables=FALSE) {

  tryCatch({
    suppressWarnings({
      conn <- get_conn(dbname="Elmer")
      query <- build_recordset_sql(schema_name, include_base_tables)
      tbl_list <- DBI::dbGetQuery(conn, DBI::SQL(query))
      DBI::dbDisconnect(conn)
    })
    return(tbl_list)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in list_recordsets: {w}"))
  }, error = function(e) {
    stop(glue::glue('stopped within error handler in list_recordsets: {e}'))
  })
}



