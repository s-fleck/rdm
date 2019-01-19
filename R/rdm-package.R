#' @keywords internal
#' @importFrom glue glue
"_PACKAGE"


.onLoad <- function(...){
  assign("lg", lgr::lgr$spawn("rdm", threshold = NA), envir = parent.env(environment()))
}
