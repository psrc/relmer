#' st_read_query(sql, ...)
#'
#' Read geometric data to simple features with a query
#' Allows you to return part of a spatial table
#' For an entire spatial table from either Elmer or ElmerGeo, use st_read_elmergeo(layer_name, ...)
#'
#' @param db_name String.  The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".
#' @param sql String.  The SQL command to send to <db_name>.
#' @param project_to_wgs84 (TRUE/FALSE) If TRUE then deliver the output in WGS84 projection, otherwise NAD84 / WA State Plane North.  Defaults to TRUE.
#' @return object of class sf
#'
#' @export
st_read_query <- function(sql, db_name = 'Elmer',  project_to_wgs84 = TRUE) {

  tryCatch({
    suppressWarnings({
      conn <- get_conn(dbname = db_name)
      #layer_sql <- build_sql(schema_name=schema_name, tbl_name=tbl_name, conn)
      lyr <- sf::st_read(conn, query=sql)
      lyr <- sf::st_set_crs(lyr, 2285) #2285 = WA State Plane N
      if(project_to_wgs84){
        lyr <- reproject_sf(lyr, 4326) #4326 = WGS84
      }
      DBI::dbDisconnect(conn)
    })
    return(lyr)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in st_read_query: {w}"))
  }, error = function(e) {
    stop(glue::glue('stopped within error handler in st_read_query: {e}'))
  })
}
