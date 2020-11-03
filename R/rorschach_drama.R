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
  mirror = 4,  # "v", "h", "4"
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
    is_scalar_integerish(mirror),
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
      format = "[:bar] [:current/:total] [:elapsedfull] [eta: :chunk_eta / :total_eta]",
      show_after = 0
    )

    lg$info("processing {length(batches) + start_batch - 1L} batches")
    pb$tick(start_batch, tokens = list(chunk_eta = "NA"))

    cat("\n")

    t0 <- Sys.time()
    tdiffs <- t0 - t0

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
        glue("[{ids}:v:0]scale='if(gt(a*sar,{resolution[[1]]}/{resolution[[2]]}),{resolution[[1]]},{resolution[[2]]}*iw*sar/ih)':'if(gt(a*sar,16/9),{resolution[[1]]}*ih/iw/sar,{resolution[[2]]})', setsar=1[v{ids}]"),
        collapse = "; "
      )


      concat <- paste0(
        paste0("[v", ids, "]", collapse = ""), glue("concat=n={length(batches[[i]])}:v=1")
      )

      assert(all(file.exists(batches[[i]])))
      outf_tmp <- paste0(tools::file_path_sans_ext(outf), "_tmp.mkv")

      args <- glue(
        '{inf} -y -filter_complex "
        color=size={resolution[[1]]*2}x{resolution[[2]]*2}:color=black [bgr];\
        {scale};\
        {concat}[out];\
        [out]{make_mirror(mirror)}[out];\
        [out]{paste(filters, collapse = ";")}[out]
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


make_mirror <- function(n){
  is_scalar_integerish(n)
  n <- as.integer(n)

  idx <- seq_len(n)
  ipd <- pad_left(idx, 3, "0")

  streams <- glue("[in{ipd}]")

  crop <- character()
  over <- character()

  h <- 0
  w <- 0
  w_max = 1
  h_max = 0

  direction <- 1
  horizontal <- TRUE  # wether to advance horizontal or vertical
  flip_v <- FALSE
  flip_h <- FALSE


  for (i in idx){

    if (flip_v && flip_h){
      crop[[i]] <- glue("[in{ipd[[i]]}] crop=iw/2:ih/2:0:0, vflip, hflip [in{ipd[[i]]}]")

    } else if (flip_v){
      crop[[i]] <- glue("[in{ipd[[i]]}] crop=iw/2:ih/2:0:0, vflip [in{ipd[[i]]}]")

    } else if (flip_h){
      crop[[i]] <- glue("[in{ipd[[i]]}] crop=iw/2:ih/2:0:0, hflip [in{ipd[[i]]}]")

    } else {
      crop[[i]] <- glue("[in{ipd[[i]]}] crop=iw/2:ih/2:0:0 [in{ipd[[i]]}]")
    }

    if (i == 1){
      over[[i]] <- glue("[bgr][in{ipd[[i]]}] overlay=w*{w}:h*{h}:shortest=1 [bgr]")
    } else {
      over[[i]] <- glue("[bgr][in{ipd[[i]]}] overlay=w*{w}:h*{h} [bgr]")
    }

  # advance
    if (horizontal){
      w <- w + direction
      flip_h <- !flip_h
    } else {
      h <- h + direction
      flip_v <- !flip_v
    }

    # lower left corner
    if (w == -w_max && h == -h_max){
      w_max <- w_max + 1
      horizontal <- TRUE
      direction <- 1
    }

    # lower right corner
    if (w == w_max && h == -h_max){
      h_max <- h_max + 1
      horizontal <- FALSE
      direction <- 1
    }

    # top right
    if (w == w_max && h == h_max){
      horizontal <- TRUE
      direction <- -1
    }

    # top left
    if (w == -w_max && h == h_max){
      direction <- -1
      horizontal <- FALSE
    }
  }

  res <- glue(
    "split={n} {paste(streams, collapse = '')};",
    paste(crop, collapse = ";"), ";",
    paste(over, collapse = ";")
  )

  sub("\\[bgr\\]$", "", res)
}



pb_format <- "[:bar] [:current/:total] [:elapsedfull] eta::eta"
