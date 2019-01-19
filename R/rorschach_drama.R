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
  overwrite = FALSE,
  resolution = c(1920, 1080),
  mirror = "4",  # "v", "h", "4"
  tune = "film", # "film", "animation",
  batch_size = 50,
  temp_dir = tempdir(),
  rotate = glue("2*PI*t/10:ow={resolution[[1]]}:oh={resolution[[2]]}:c=none"),
  hue = "H=2*PI*t: s=sin(2*PI*t)+1",
  start_batch = 1L  # NULL vor no clip creation
){
  stopifnot(
    all(file.exists(infiles)),
    is_scalar_bool(overwrite),
    dir.exists(dirname(outfile)),
    is.numeric(resolution) && identical(length(resolution) , 2L),
    is_scalar_character(mirror) && mirror %in% names(mirror_presets),
    is_scalar_character(tune),
    is_scalar_integerish(batch_size),
    dir.exists(temp_dir),
    is_scalar_integerish(start_batch) || is.null(start_batch)
  )

  # init
  stderr_log <- file.path(temp_dir, "ffmpeg_stderr.log")
  stdout_log <- file.path(temp_dir, "ffmpeg_stdout.log")
  video_codec <- glue("libx264 -preset slow -tune {tune}")
  batches  <- suppressWarnings(vec_split_interval(infiles, batch_size))
  tempfiles <- file.path(temp_dir, sprintf("clip_%s.mkv", seq_along(batches)))
  res <- tempfiles

  # if resume...
  if (is.null(start_batch)){
    batches <- NULL

  } else {
    tempfiles <- tempfiles[start_batch:length(batches)]
    batches   <- batches[start_batch:length(batches)]

    lg$info(
      "Generating a %s Mirror Rorschach Drama (%sx%s)",
      c("h" = "horizontal", "v" = "vertical", "4" = "4-way", "16" = "16-way")[[mirror]],
      resolution[[1]],
      resolution[[2]]
    )
    lg$debug("Codec settings '%s'", video_codec)
    lg$debug("Logging stderr to %s", stderr_log)
    lg$debug("Logging stdout to %s", stdout_log)
    pb <- progress::progress_bar$new(
      total = length(batches) + start_batch - 1L,
      format = pb_format,
      show_after = 0
    )


    # mirror args
    mirror <- mirror_presets[[mirror]]  # mirror presets is a global variable
    lg$info("Processing %s batches", length(batches) + start_batch - 1L)
    pb$tick(start_batch)
    cat("\n")

    for (i in seq_along(batches)){
      inf  <- paste("-i", batches[[i]], collapse = " ")
      outf <- tempfiles[[i]]

      lg$debug("Processing batch %s/%s", i + start_batch - 1L, length(batches) + + start_batch - 1L)
      lg$debug("Saving temporary file for batch %s to '%s'", i, outf)
      ids <- seq_along(batches[[i]]) - 1L

      scale <-  paste(
        glue("[{ids}:v:0]scale='if(gt(a*sar,{resolution[[1]]}/{resolution[[2]]}),{resolution[[1]]},{resolution[[2]]}*iw*sar/ih)':'if(gt(a*sar,16/9),{resolution[[1]]}*ih/iw/sar,{resolution[[2]]})', pad={resolution[[1]]}:{resolution[[2]]}:(ow-iw)/2:(oh-ih)/2,setsar=1[v{ids}]"),
        collapse = "; "
      )

      concat <- paste0(
        paste0("[v", ids, "]", collapse = ""), glue("concat=n={length(batches[[i]])}:v=1")
      )


      assert(all(file.exists(batches[[i]])))

      args <- glue(
        '{inf} -y -filter_complex "
        color=size={resolution[[1]]*2}x{resolution[[2]]*2}:color=black [bgr];\
        {scale};\
        {concat}[out];\
        [out]{mirror}[out];\
        [out]rotate={rotate}[out];\
        [out]hue={hue}[out]
        " -map [out] {outf} -c:v {video_codec}'
      )

      ret <- system2(
        "ffmpeg",
        args,
        stderr = stderr_log,
        stdout = stdout_log
      )

      if (ret != 0){
        walk(tail(readLines(stderr_log)), lg$fatal)
        stop(lg$fatal("ffmpeg returned 1, please check log files"))
      }

      pb$tick()
    }
  }

  res
}



concatennate_media <- function(
  infiles,
  outfile,
  temp_dir = tempdir()
){
  # concatennate
  listfile <- file.path(temp_dir, "list.txt")
  writeLines(paste0("file '", infiles, "'"), listfile)
  stderr_log <- file.path(dirname(infiles)[[1]], "ffmpeg_stderr.log")
  stdout_log <- file.path(dirname(infiles)[[1]], "ffmpeg_stdout.log")


  if (file.exists(outfile)){
    if (overwrite){
      file.remove(outfile)
    } else {
      stop(FATAL("'%s' exsits and overwrite == FALSE", outfile))
    }
  }

  lg$info("Concatennating results to '%s'", outfile)

  ret <- system2(
    "ffmpeg", glue("-f concat -safe 0 -i {listfile} -c copy {outfile}"),
    stderr = stderr_log,
    stdout = stdout_log
  )

  if (ret != 0){
    lg$error("ffmpeg returned 1, please check log file")
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
    [mid1][in3] overlay=W/2:H/2 ",
  "16" =  # shortest=1 for the first overlay so that the nullsink does not go on forever
    "split=16 [in0][in1][in2][in3][in4][in5][in6][in7][in8][in9][ina][inb][inc][ind][ine][inf]; \
    [in0] crop=iw/2:ih/2:0:0 [in0];\
    [in1] crop=iw/2:ih/2:0:0, hflip [in1]; \
    [in2] crop=iw/2:ih/2:0:0, vflip [in2]; \
    [in3] crop=iw/2:ih/2:0:0, vflip, hflip [in3]; \
    [in4] crop=iw/2:ih/2:0:0 [in4];\
    [in5] crop=iw/2:ih/2:0:0, hflip [in5]; \
    [in6] crop=iw/2:ih/2:0:0, vflip [in6]; \
    [in7] crop=iw/2:ih/2:0:0, vflip, hflip [in7]; \
    [in8] crop=iw/2:ih/2:0:0 [in8];\
    [in9] crop=iw/2:ih/2:0:0, hflip [in9]; \
    [ina] crop=iw/2:ih/2:0:0, vflip [ina]; \
    [inb] crop=iw/2:ih/2:0:0, vflip, hflip [inb]; \
    [inc] crop=iw/2:ih/2:0:0 [inc];\
    [ind] crop=iw/2:ih/2:0:0, hflip [ind]; \
    [ine] crop=iw/2:ih/2:0:0, vflip [ine]; \
    [inf] crop=iw/2:ih/2:0:0, vflip, hflip [inf]; \
    [bgr][in1] overlay=0:2:shortest=1 [mid];\
    [mid][in2] overlay=w:0    [mid];\
    [mid][in3] overlay=2*w:0   [mid];\
    [mid][in0] overlay=3*w:0   [mid];\
    [mid][in7] overlay=0:h     [mid];\
    [mid][in8] overlay=w:h    [mid];\
    [mid][in5] overlay=2*w:h   [mid];\
    [mid][in6] overlay=3*w:h   [mid];\
    [mid][in9] overlay=0:2*h     [mid];\
    [mid][ina] overlay=w:2*h    [mid];\
    [mid][inb] overlay=2*w:2*h   [mid];\
    [mid][inc] overlay=3*w:2*h   [mid];\
    [mid][inf] overlay=0:3*h     [mid];\
    [mid][in4] overlay=w:3*h    [mid];\
    [mid][ind] overlay=2*w:3*h   [mid];\
    [mid][ine] overlay=3*w:3*h"
)



pb_format <- "[:bar] [:current/:total] [:elapsedfull] eta::eta"
