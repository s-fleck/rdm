#' Title
#'
#' @param files
#' @param outdir
#' @param n
#' @param lengths
#' @param pad
#'
#' @return
#' @export
#'
#' @examples
sample_clips <- function(
  files,
  outdir,
  n,
  lengths = c(4, 8, 16),
  pad = 0
){
  n_per_file <- ceiling(n / length(files))
  n <- n_per_file * length(files)
  yog$info(glue(
    "Sampling {n} clips with an expected combined length of {explen} from:\n",
    "{paste0(' * ', files, collapse = '\n')}",
    n, explen = hms::as.hms(round(mean(lengths) * n))
  ))

  pb <- progress::progress_bar$new(total = n, format = pb_format)

  outfiles <- lapply(
    files,
    sample_clips_single,
    outdir = outdir,
    n = n_per_file,
    lengths = lengths,
    pad = pad,
    pb = pb
  )

  unlist(outfiles)
}




sample_clips_single <- function(
  file,
  outdir,
  n,
  lengths = c(4, 8, 16),
  pad = 0,
  pb = progress::progress_bar$new(total = n, format = pb_format)
){
  stopifnot(
    all(file.exists(file)),
    is_scalar_integerish(n),
    is.numeric(lengths),
    is.numeric(pad) && length(pad) %in% 1:2
  )

  if (length(pad) == 1){
    pad <- c(pad, pad)
  }

  m_len    <- clip_length(file)
  m_len    <- floor(m_len - sum(pad)) - max(lengths)
  s_pos    <- sample(seq(1, m_len, by = min(lengths)), n, replace = FALSE)
  c_lens   <- sample(lengths, n, replace = TRUE, prob = rev(lengths))
  c_names  <- sprintf("%s_%s_[%s].mkv", stringi::stri_rand_strings(1, 10), seq_along(s_pos), c_lens)

  for (i_pos in seq_along(s_pos)){
    pb$tick()
    name <- c_names[[i_pos]]
    args <- glue('-fflags +genpts -ss {s_pos[i_pos]} -i "{file}" -t {c_lens[i_pos]} -an -y -c copy {outdir}/{name}')
    res  <- system2("ffmpeg", args, stderr = TRUE, stdout = TRUE)
    if (!is.null(attr(res, "status"))){
      for(el in res){cat(el, "\n")}
      stop(res)
    }
  }

  file.path(outdir, c_names)
}



clip_length <- function(
  files
){
  clip_length_impl <- function(file){
    info <- system2("ffprobe", paste0('"', file, '"'), stdout = TRUE, stderr = TRUE)
    stopifnot(attr(info, "status") == 1)
    info <- grep("Duration:", info, value = TRUE)
    dur <- stringi::stri_extract_first_regex(info, "\\d{2}:\\d{2}:\\d{2}.\\d{2}")
    as.numeric(hms::as.hms(dur))
  }

  vapply(files, clip_length_impl, numeric(1))
}
