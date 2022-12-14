get_auth <- function(){
  
  tryCatch({
    auth <- list(
        'uid'=Sys.getenv('SOCKEYE_UID'),
        'pwd' = Sys.getenv('SOCKEYE_PWD'))
    return(auth)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_auth: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_auth: {e}"))
    stop(e)
  })
}
