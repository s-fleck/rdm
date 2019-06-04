#' @keywords internal
#' @importFrom glue glue
"_PACKAGE"


.onLoad <- function(...){
  assign(
    "lg",
    lgr::get_logger("rdm"),
    envir = parent.env(environment())
  )
}
