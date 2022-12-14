check_sql_driver <- function(driver_name) {
  
  tryCatch({
    drivers <- odbc::odbcListDrivers()
    d_names <- drivers$name
    if (!(driver_name %in% d_names)) {
      driver_url = "https://learn.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server?view=sql-server-ver16"
      err_msg <- glue::glue("You don't seem to have the {driver_name} installed.",
                            "Try downloading it from {driver_url} (or just google it :)).")
      stop(err_msg)
    }
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_conn: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_conn: {e}"))
    stop(e)
  })
}

is.windows <- function() {
  tryCatch({
    sysname = Sys.info()['sysname']
    is.win <- FALSE
    if (sysname == 'Windows') {
      is.win <- TRUE
    }
    return(is.win)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_os: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_os: {e}"))
    stop(e)
  })
}


build_conn <- function(dbname, driver_name) {
  
  tryCatch({
    if (is.windows()) {
      conn <- DBI::dbConnect(odbc::odbc(),
                             driver = driver_name,
                             server = "AWS-PROD-SQL\\Sockeye",
                             database = dbname,
                             trusted_connection = "yes")
    } else {
      auth <- get_auth()
      conn <- DBI::dbConnect(odbc::odbc(),
                             driver = driver_name,
                             server = "AWS-PROD-SQL\\Sockeye",
                             database = dbname,
                             uid = auth$uid,
                             pwd = auth$pwd)
    }
    return(conn)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_conn_str: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_conn_str: {e}"))
    stop(e)
  })
}

get_conn <- function(dbname='ElmerGeo') {

  tryCatch({
    driver_name = 'ODBC Driver 17 for SQL Server'
    check_sql_driver(driver_name)
    conn <- build_conn(dbname, driver_name)
    return(conn)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_conn: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_conn: {e}"))
  })
}
