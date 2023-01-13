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
