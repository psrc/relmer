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

get_conn <- function(dbname='ElmerGeo') {

  tryCatch({
    driver_name = 'ODBC Driver 17 for SQL Server'
    check_sql_driver(driver_name)
    DBI::dbConnect(odbc::odbc(),
              driver = driver_name,
              server = "AWS-PROD-SQL\\Sockeye",
              database = dbname,
              trusted_connection = "yes")
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_conn: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_conn: {e}"))
  })
}
