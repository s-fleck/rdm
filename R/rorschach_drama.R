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
  filters = list(
    glue("rotate=2*PI*t/10:ow={resolution[[1]]}:oh={resolution[[2]]}:c=none"),
    "hue=H=2*PI*t: s=sin(2*PI*t)+1"
  ),
  start_batch = 1L  # NULL for no clip creation
){
  stopifnot(
    all(file.exists(infiles)),
    is_scalar_bool(overwrite),
    dir.exists(dirname(outfile)),
    is.numeric(resolution) && identical(length(resolution) , 2L),
    is_scalar_character(mirror),
    is_scalar_character(tune),
    is_scalar_integerish(batch_size),
    dir.exists(temp_dir),
    is_scalar_integerish(start_batch) || is.null(start_batch)
  )

  # init
  stderr_log <- path.expand(file.path(temp_dir, "ffmpeg_stderr.log"))
  stdout_log <- path.expand(file.path(temp_dir, "ffmpeg_stdout.log"))
  video_codec <- glue("libx264 -preset slow -tune {tune}")
  batches  <- suppressWarnings(vec_split_interval(infiles, batch_size))
  tempfiles <- path.expand(file.path(temp_dir, sprintf("clip_%s.mkv", seq_along(batches))))
  res <- tempfiles

  # if resume...
  if (is.null(start_batch)){
    batches <- NULL

  } else {
    tempfiles <- tempfiles[start_batch:length(batches)]
    batches   <- batches[start_batch:length(batches)]

    lg$info(
      "generating a {type} mirror rorschach drama ({resolution[[1]]}x{resolution[[2]]})",
      type = c("h" = "horizontal", "v" = "vertical", "4" = "4-way", "16" = "16-way")[[mirror]]
    )
    lg$debug("using codec '{codec}'", codec = video_codec)
    lg$debug("routing ffmpeg stderr to '{path}'", path = stderr_log)
    lg$debug("routing ffmpeg stdout to '{path}'", path = stdout_log)

    pb <- progress::progress_bar$new(
      total = length(batches) + start_batch - 1L,
      format = "[:bar] [:current/:total] [:elapsedfull] eta::eta",
      show_after = 0
    )

    # mirror args
    if (mirror %in% names(MIRROR_PRESETS))
      mirror <- MIRROR_PRESETS[[mirror]]

    lg$info("processing {length(batches) + start_batch - 1L} batches")
    pb$tick(start_batch, tokens = list(chunk_eta = "NA"))
    cat("\n")

    tdiffs <- Sys.time() - Sys.time()

    for (i in seq_along(batches)){
      inf  <- paste("-i", batches[[i]], collapse = " ")
      outf <- tempfiles[[i]]
      batch_number <- i + start_batch - 1L  # for logging
      ids <- seq_along(batches[[i]]) - 1L

      lg$info(
        "processing batch {cur}/{all} ({length(ids)} files)",
        cur = batch_number,
        all = length(batches) + start_batch - 1L,
        file = outf
      )

      scale <-  paste(
        glue("[{ids}:v:0]scale='if(gt(a*sar,{resolution[[1]]}/{resolution[[2]]}),{resolution[[1]]},{resolution[[2]]}*iw*sar/ih)':'if(gt(a*sar,16/9),{resolution[[1]]}*ih/iw/sar,{resolution[[2]]})', pad={resolution[[1]]}:{resolution[[2]]}:(ow-iw)/2:(oh-ih)/2,setsar=1[v{ids}]"),
        collapse = "; "
      )

      concat <- paste0(
        paste0("[v", ids, "]", collapse = ""), glue("concat=n={length(batches[[i]])}:v=1")
      )

      assert(all(file.exists(batches[[i]])))
      outf_tmp <- paste0(tools::file_path_sans_ext(outf), "_tmp.mkv")

      stopifnot(
        is_scalar_character(inf),
        is_scalar_character(mirror),
        is_scalar_character(video_codec),
        is_scalar_character(outf_tmp)
      )


      args <- glue(
        '{inf} -y -filter_complex "
        color=size={resolution[[1]]*2}x{resolution[[2]]*2}:color=black [bgr];\
        {scale};\
        {concat}[out];\
        [out]{mirror}[out];\
        {paste("[out]", filters, "[out]", collapse = ";", sep = "")}
        " -map [out] {outf_tmp} -c:v {video_codec}'
      )

      t1 <- Sys.time()

      lg$debug("calling ffmpeg", args = gsub("\n", "", args))

      ret <- try({system2(
        "ffmpeg",
        args,
        stderr = stderr_log,
        stdout = stdout_log
      )})

      tdiffs[[i]] <- Sys.time() - t1

      if (is_try_error(ret) || !identical(as.integer(ret), 0L)){
        walk(tail(readLines(stderr_log)), lg$fatal)
        lg$fatal(
          "skipping batch {batch_number} because of ffmpeg error; please check log files.",
          temp_file = outf_tmp,
          args = args
        ) %>%
          stop()
      }

      file.rename(outf_tmp, outf)

      lg$debug("finished processing batch {batch_number}")
      pb$tick()
    }
  }

  res
}



#' Title
#'
#' @param infiles
#' @param outfile
#' @param temp_dir
#'
#' @return
#' @export
#'
#' @examples
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
      stop(FileExistsError(lg$fatal("'{outfile}' exsits and overwrite == FALSE")))
    }
  }

  lg$info("concatennating results to '{outfile}'")

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



MIRROR_PRESETS <- list(
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
    [mid][ine] overlay=3*w:3*h",
  "24" =
    "split=24 [in0][in1][in2][in3][in4][in5][in6][in7][in8][in9][ina][inb][inc][ind][ine][inf][ing][inh][ini][inj][ink][inl]; \
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
    [ing] crop=iw/2:ih/2:0:0 [ing];\
    [inh] crop=iw/2:ih/2:0:0, hflip [inh]; \
    [ini] crop=iw/2:ih/2:0:0, vflip [ini]; \
    [inj] crop=iw/2:ih/2:0:0, vflip, hflip [inj]; \
    [ink] crop=iw/2:ih/2:0:0 [ink];\
    [inl] crop=iw/2:ih/2:0:0, hflip [inl]; \
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
    [mid][ine] overlay=3*w:3*h [mid];\
    [mid][ing] overlay=4*w:1*h     [mid];\
    [mid][inh] overlay=4*w:2*h    [mid];\
    [mid][ini] overlay=4*w:3*h   [mid];\
    [mid][inj] overlay=5*w:1*h [mid];\
    [mid][ink] overlay=5*w:2*h     [mid];\
    [mid][inl] overlay=5*w:3*h"
)


make_mirror <- function(n){
  idx <- seq_len(n)

  streams <- glue("[in{idx}a][in{idx}b][in{idx}c][in{idx}d]") %>%
    paste(collapse = "")

  crop <- glue("
    [in{idx}b] crop=iw/2:ih/2:0:0 [in{idx}b];\
    [in{idx}a] crop=iw/2:ih/2:0:0, hflip [in{idx}a]; \
    [in{idx}c] crop=iw/2:ih/2:0:0, vflip [in{idx}c]; \
    [in{idx}d] crop=iw/2:ih/2:0:0, vflip, hflip [in{idx}d]; \
  ") %>%
    paste(collapse = "\n")

  dd <- list()
  w <- 0
  h <- 0

  for (i in idx){

    if (i %% 4L == 0L){
      w <-  w + 2L
      order <- c("a", "b", "c", "d")
    } else if (i %% 3L == 0L){
      h <- h + 2L
      w <- w - 2L
      order <- c("b", "a", "d", "c")
    } else if (i %% 2L == 0L){
      w <- w + 2L
      order <- c("c", "d", "a", "b")
    } else {
      order <- c("a", "b", "c", "d")
    }
    w
    h
    dd[[i]] <- glue("
      [bgr][in{i}{order[[1]]}] overlay=w*{w}:h*{h}:shortest=1 [bgr];\
      [bgr][in{i}{order[[2]]}] overlay=w*{w+1}:h*{h}    [bgr];\
      [bgr][in{i}{order[[3]]}] overlay=w*{w}:h*{h+1}   [bgr];\
      [bgr][in{i}{order[[4]]}] overlay=w*{w+1}:h*{h+1} [bgr];\
    ")
  }

  overlay <- paste(unlist(dd), collapse = "\n")

  res <- glue(
    "split={n*4} {streams};\n",
    crop, "\n",
    overlay
  )

  sub("\\[bgr\\];$", "", res)
}



pb_format <- "[:bar] [:current/:total] [:elapsedfull] eta::eta"
