


assertthat::assert_in <- function(x, categorias){

  x %in% categorias

}


assertthat::on_failure(assert_in) <- function(call, env) {

  paste0(
    deparse(call$x),
    " deveria ser um desses valores: ",
    eval(call$categorias) %>% str_flatten(collapse = ", ")
  )
}



