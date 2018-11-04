combine_audio_video <- function(
  video,
  audio,
  output
){
  assert(file.exists(video))
  assert(file.exists(audio))
  assert(dir.exists(dirname(output)))
  system2("ffmpeg", glue("-i {video} -i {audio} -c:v copy -c:a libvorbis -shortest {output}"))
}
