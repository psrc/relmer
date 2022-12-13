get_auth <- function(){
  
  tryCatch({
    conf <- yaml::yaml.load_file('config.yml')
    auth <- list(
        'uid'=conf$Auth$uid,
        'pwd' = conf$Auth$pwd)
    return(auth)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_auth: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_auth: {e}"))
    stop(e)
  })
}