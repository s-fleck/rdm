sample_audio <- function(
  file,
  outdir,
  n,
  lengths = c(2, 4, 8, 16),
  pad = 0
){
  assert(all(file.exists(file)))
  assert(is_scalar_integerish(n))
  assert(is.numeric(lengths))
  assert(is.numeric(pad) && length(pad) %in% 1:2)

  if (length(pad) == 1){
    pad <- c(pad, pad)
  }

  lg$info("Sampling %s clips from '%s'", n, file)

  pb <- progress::progress_bar$new(total = n)

  m_len    <- clip_length(file)
  m_len    <- floor(m_len - sum(pad)) - max(lengths)
  s_pos    <- sample(seq_len(m_len), n)
  c_lens   <- sample(lengths, n, replace = TRUE)
  clipname <- stringi::stri_rand_strings(1, 10)


  for (i_pos in seq_along(s_pos)){
    pb$tick()
    name <- sprintf("%s_%s.wav", clipname, i_pos)
    args <- glue('-fflags +genpts -ss {s_pos[i_pos]} -i "{file}" -t {c_lens[i_pos]} -vn -y -c copy {outdir}/{name}')
    res  <- system2("ffmpeg", args, stderr = TRUE, stdout = TRUE)
    if (!is.null(attr(res, "status"))){
      for(el in res){cat(el, "\n")}
      stop(res)
    }
  }
}
