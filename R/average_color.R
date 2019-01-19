#' Average color of each frame of a video
#'
#' @param infile a video file that can be handled by ffmpeg
#' @param cores number of cores to use (passed on to [parallel::mclapply()])
#'
#' @return an `integer` matrix with 3 columns (read, green, blue)
#' @export
#'
average_frame_color <- function(
  infile
){
  td <- tempdir()
  stderr_log <- file.path(td, "ffmpeg_stderr.log")
  stdout_log <- file.path(td, "ffmpeg_stdout.log")

  # rescale to 1x1 bitmaps to get average color
  ret <- system2(
    "ffmpeg",
    glue("-i {infile} -vf scale=1:1 {td}/avg_color_tmp%03d.png"),
    stderr = stderr_log,
    stdout = stdout_log
  )

  tmpfiles <- list.files(td, "avg_color_tmp.*", full.names = TRUE)
  on.exit(file.remove(tmpfiles))
  res <- matrix(nrow = length(tmpfiles), ncol = 3)

  # extract color information with imagemagic
  do.call(rbind, future.apply::future_lapply(tmpfiles, extract_color_values))
}




extract_color_values <- function(
  infile
){
  tf  <- tempfile(fileext = ".txt")
  ret <- system2("convert", paste(infile, tf))
  on.exit(file.remove(tf))
  dd  <- readLines(tf)[[2]]
  dd  <- stringi::stri_extract_first_regex(dd, "\\(\\d{1,3},\\d{1,3},\\d{1,3}\\)")
  as.integer(stringi::stri_extract_all_regex(dd, "\\d{1,3}")[[1]])
}
