#' Title
#'
#' @param infiles
#' @param outfile
#' @param resolution
#' @param mirror
#' @param tune
#' @param batch_size
#' @param temp_dir
#' @param start_batch
#'
#' @return
#' @export
#'
#' @examples
rorschach_drama <- function(
  infiles,
  outfile,
  resolution = c(1920, 1080),
  mirror = "4",  # "v", "h", "4"
  tune = "film", # "film", "animation",
  batch_size = 50,
  temp_dir = tempdir(),
  start_batch = 1L
){
  stopifnot(
    all(file.exists(infiles)),
    dir.exists(dirname(outfile)),
    is.numeric(resolution) && identical(length(resolution) , 2L),
    is_scalar_character(mirror) && mirror %in% names(mirror_presets),
    is_scalar_character(tune),
    is_scalar_integerish(batch_size),
    dir.exists(tempdir),
    is_scalar_integerish(start_batch)
  )

  # init
    stderr_log <- file.path(temp_dir, "ffmpeg_stderr.log")
    stdout_log <- file.path(temp_dir, "ffmpeg_stdout.log")
    video_codec <- glue("libx264 -preset slow -tune {tune}")
    batches  <- suppressWarnings(vec_split_interval(infiles, batch_size))
    tempfiles <- file.path(temp_dir, sprintf("clip_%s.mkv", seq_along(batches)))

    # if resume...
    tempfiles <- tempfiles[start_batch:length(batches)]
    batches   <- batches[start_batch:length(batches)]

    yog$info(
      "Generating a %s Mirror Rorschach Drama (%sx%s)",
      c("h" = "horizontal", "v" = "vertical", "4" = "4-way")[[mirror]],
      resolution[[1]],
      resolution[[2]]
    )
    yog$debug("Codec settings '%s'", video_codec)
    yog$debug("Writing output to %s", outf)
    yog$debug("Logging stderr to %s", stderr_log)
    yog$debug("Logging stdout to %s", stdout_log)
    pb <- progress::progress_bar$new(
      total = length(batches),
      format = pb_format,
      show_after = 0
    )

    # mirror args
    mirror <- mirror_presets[[mirror]]  # mirror presets is a global variable





  yog$info("Processing %s batches", length(batches))
  pb$tick(0)
  cat("\n")

  for (i in seq_along(batches)){
    yog$info("Processing batch %s/%s", i, length(batches))
    yog$debug("Saving temporary file for batch %s to '%s'", i, outf)
    ids <- seq_along(batches[[i]]) - 1L

    scale <-  paste(
      glue("[{ids}:v:0]scale='if(gt(a*sar,{resolution[[1]]}/{resolution[[2]]}),{resolution[[1]]},{resolution[[2]]}*iw*sar/ih)':'if(gt(a*sar,16/9),{resolution[[1]]}*ih/iw/sar,{resolution[[2]]})', pad={resolution[[1]]}:{resolution[[2]]}:(ow-iw)/2:(oh-ih)/2,setsar=1[v{ids}]"),
      collapse = "; "
    )

    concat <- paste0(
      paste0("[v", ids, "]", collapse = ""), glue("concat=n={length(batches[[i]])}:v=1")
    )

    inf  <- paste("-i", batches[[i]], collapse = " ")
    outf <- tempfiles[[i]]

    assert(all(file.exists(batches[[i]])))

    args <- glue(
      '{inf} -y -filter_complex "
      {scale}; \
      {concat}[out];\
      [out]{mirror_4}[out]\
      " -map [out] {outf} -c:v {video_codec}'
    )

    ret <- system2(
      "ffmpeg",
      args,
      stderr = stderr_log,
      stdout = stdout_log
    )

    if (ret != 0){
      walk(tail(readLines(stderr_log)), yog$fatal)
      stop(yog$fatal("ffmpeg returned 1, please check log files"))
    }

    pb$tick()
  }


  listfile <- file.path(temp_dir, "list.txt")
  writeLines(paste0("file '", tempfiles, "'"), listfile)


  yog$info("Concatennating results to '%s'", outfile)

  ret <- system2(
    "ffmpeg", glue("-f concat -safe 0 -i {listfile} -c copy {outfile}"),
    stderr = stderr_log,
    stdout = stdout_log
  )

  if (ret != 0){
    flog.error("ffmpeg returned 1, please check log file")
  }

  outfile
}


mirror_presets <- list(
  h = "split [main][flip]; [flip]crop=iw/2:ih:0:0, hflip[flip]; [main][flip] overlay=W/2:0",
  v = "split [main][flip]; [flip]crop=iw:ih/2:0:0, hflip[flip]; [main][flip] overlay=0:H/2",
  "4" =
    "split=4 [in0][in1][in2][in3]; \
    [in1] crop=iw/2:ih/2:0:0, hflip [in1]; \
    [in2] crop=iw/2:ih/2:0:0, vflip [in2]; \
    [in3] crop=iw/2:ih/2:0:0, vflip, hflip [in3]; \
    [in0][in1] overlay=W/2:0   [mid0];\
    [mid0][in2] overlay=0:H/2  [mid1];\
    [mid1][in3] overlay=W/2:H/2 "
 )





#' Chop up a vector into equal sized chunks
#'
#' From: \url{https://gist.github.com/sckott/4632735}
#'
#' @param x An input vector.
#' @param interval The length of the resulting vectors.
#'
#' @family vector tools
#' @export
#' @examples
#' vec <- c("a","b","d","e","f","g","h")
#' vec_split_interval(vec, 3)
vec_split_interval <- function(x, interval){
  splt                 <- rep(FALSE, interval)
  splt[1]              <- TRUE
  a <- ceiling(length(x) / length(splt))
  splt  <-  rep(splt, a)
  splt <- cumsum(splt)

  split(x, splt)
}



pb_format <- "[:bar] :percent [:elapsedfull :eta]"
