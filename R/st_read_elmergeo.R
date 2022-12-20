build_sql <- function(schema_name, tbl_name, conn) {

  tryCatch({
    col_name_ns_sql <- glue::glue("select c.COLUMN_NAME
            from INFORMATION_SCHEMA.COLUMNS c
            where c.TABLE_SCHEMA = '{schema_name}'
            	and c.TABLE_NAME = '{tbl_name}'
            	and c.DATA_TYPE not in ('geometry', 'geography')
            	and c.COLUMN_NAME not in ('GDB_GEOMATTR_DATA')")
    col_df = DBI::dbGetQuery(conn, DBI::SQL(col_name_ns_sql))
    col_names_ns <- paste(t(col_df), collapse=', ')
    s_col_name_sql <- glue::glue("select c.COLUMN_NAME + '.STAsBinary() as Shape '
            from INFORMATION_SCHEMA.COLUMNS c
            where c.TABLE_SCHEMA = '{schema_name}'
            	and c.TABLE_NAME = '{tbl_name}'
            	and c.DATA_TYPE in ('geometry', 'geography')
            	and c.COLUMN_NAME not in ('GDB_GEOMATTR_DATA')")
    s_col_df = DBI::dbGetQuery(conn, DBI::SQL(s_col_name_sql))
    col_names_s <- paste(t(s_col_df), collapse=', ')
    glue::glue('SELECT {col_names_ns}, {col_names_s} FROM {schema_name}.{tbl_name}')
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_sql: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_sql: {e}"))
  })
}

layer_type <- function(layer_name, schema_name, conn) {

  tryCatch({
    if (is_table(layer_name, schema_name, conn, as_evw=TRUE)) {
      table_type <- 'evw'
    } else if (is_table(layer_name, schema_name, conn, as_evw=FALSE)) {
      table_type <- 'nonversioned'
    } else {
      table_type <- 'none'
    }
    table_type
  }, warning = function(w) {
    print(glue::glue("A warning popped up in layer_type: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in layer_type: {e}"))
  })
}

is_table <- function(layer_name, schema_name, conn, as_evw = FALSE){

  tryCatch({
    suffix <- ifelse(as_evw, '_evw', '')
    infoschm_tbl <- ifelse(as_evw, 'VIEWS', 'TABLES')
    info_schema_sql <- glue::glue("SELECT TABLE_NAME
                            FROM INFORMATION_SCHEMA.{infoschm_tbl} as v
                            WHERE v.TABLE_NAME = \'{layer_name}{suffix}\'
                              AND v.TABLE_SCHEMA = \'{schema_name}\'")
    info_schema_df <- DBI::dbGetQuery(conn, DBI::SQL(info_schema_sql))
    if (nrow(info_schema_df) > 0) {
      TRUE
    } else {
      FALSE
    }
  }, warning = function(w) {
    print(glue::glue("A warning popped up in is_table: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in is_table: {e}"))
  })
}

is_evw <- function(layer_name, schema_name, conn){

  tryCatch({
    info_schema_sql <- glue::glue("SELECT TABLE_NAME
                            FROM INFORMATION_SCHEMA.VIEWS as v
                            WHERE v.TABLE_NAME = \'{layer_name}_evw\'
                              AND v.TABLE_SCHEMA = \'{schema_name}\'")
    info_schema_df <- DBI::dbGetQuery(conn, DBI::SQL(info_schema_sql))
    if (nrow(info_schema_df) > 0) {
      TRUE
    } else {
      FALSE
    }
  }, warning = function(w) {
    print(glue::glue("A warning popped up in is_evw: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in is_evw: {e}"))
  })
}

reproject_sf <- function(lyr, out_epsg) {

  tryCatch({
    in_crs <- sf::st_crs(lyr)
    in_epsg_str <- stringr::str_replace(in_crs[[1]], 'EPSG:', '')
    in_epsg <- strtoi(in_epsg_str)
    if (in_epsg != out_epsg) {
      lyr <- sf::st_transform(lyr, out_epsg)
    }
    return(lyr)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in reproject_sf: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in reproject_sf: {e}"))
  })
}


#' st_read_elmergeo(layer_name, ...)
#'
#' read simple features from PSRC's in-house geodatabase #'
#' This function is preferable to sf::st_read() when pulling
#' from Elmer or ElmerGeo, because it avoids some incompatibilities
#' between SQL Server's data types and R's.
#'
#' @param layer_name The name of the feature layer or geodatabase table
#' @param schema_name The name of the schema that layer_name exists in.  Defaults to "dbo", the standard schema for ElmerGeo tables
#' @param project_to_wgs84 (TRUE/FALSE) If TRUE then deliver the output in WGS84 projection, otherwise NAD84 / WA State Plane North.  Defaults to TRUE.
#' @return object of class sf
#'
#' @note If the layer has been set up as an ESRI versioned layer in the geodatabase, this function returns the versioned view (which exists in SQL Server with a "_evw" suffix).  If it has not been set up that way, it returns the base table.
#'
#' @examples
#' st_read_elmergeo("COUNTY_BACKGROUND")
#'
#' st_read_elmergeo("COUNTY_BACKGROUND", project_to_wgs84 = FALSE)
#'
#' @export
st_read_elmergeo <- function(layer_name, schema_name='dbo', project_to_wgs84 = TRUE) {

  tryCatch({
    suppressWarnings({
      conn <- get_conn()
      if (layer_type(layer_name, schema_name, conn) == 'evw') {
        tbl_name <- glue::glue("{layer_name}_evw")
      } else if (layer_type(layer_name, schema_name, conn) == 'nonversioned') {
        tbl_name <- layer_name
      } else if (layer_type(layer_name, schema_name, conn) == 'none') {
        stop("no layer error")
      }
      layer_sql <- build_sql(schema_name=schema_name, tbl_name=tbl_name, conn)
      lyr <- sf::st_read(conn, query=layer_sql)
      lyr <- sf::st_set_crs(lyr, 2285) #2285 = WA State Plane N
      if(project_to_wgs84){
        lyr <- reproject_sf(lyr, 4326) #4326 = WGS84
      }
      DBI::dbDisconnect(conn)
    })
    return(lyr)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in st_read_elmergeo: {w}"))
  }, error = function(e) {
    stop(glue::glue('stopped within error handler in st_read_elmergeo: {e}'))
  })
}




