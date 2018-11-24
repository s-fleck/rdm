#' Title
#'
#' @param infile
#'
#' @return
#' @export
#'
#' @examples
average_color <- function(infile){

  td <- tempdir()
  stderr_log <- file.path(td, "ffmpeg_stderr.log")
  stdout_log <- file.path(td, "ffmpeg_stdout.log")

  # rescale to 1x1 frames to get average color
  ret <- system2(
    "ffmpeg",
    glue("-i {infile} -vf scale=1:1 -pix_fmt bgr8 {td}/avg_color_tmp%03d.bmp"),
    stderr = stderr_log,
    stdout = stdout_log
  )

  tmpfiles <- list.files(td, "avg_color_tmp.*", full.names = TRUE)
  on.exit(file.remove(tmpfiles))
  res <- matrix(nrow = length(tmpfiles), ncol = 3)

  # extract color information with imagemagic
  for (i in seq_along(tmpfiles)){
    tf <- tmpfiles[[i]]
    ret <- system2("convert", glue("{tf} {td}/out.txt"))
    dd <- readLines(file.path(td, "out.txt"))[[2]]
    dd <- stringi::stri_extract_first_regex(dd, "\\(\\d{1,3},\\d{1,3},\\d{1,3}\\)")
    res[i, ] <- as.integer(stringi::stri_extract_all_regex(dd, "\\d{1,3}")[[1]])
  }

  colMeans(res)
}
