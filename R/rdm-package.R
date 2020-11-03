#' @keywords internal
#' @import exceptions
#' @importFrom glue glue
"_PACKAGE"


.onLoad <- function(...){
  assign(
    "lg",
    lgr::get_logger_glue("rdm"),
    envir = parent.env(environment())
  )
}
