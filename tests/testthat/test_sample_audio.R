context("sample_audio")


test_that("sample_audio works as expected", {
  infiles <- list.files(
    "/media/syno16/music/_incoming/By_the_End_of_Tonight-A_Tribute_to_Tigers-(Temporary_Residence)-CD-2005-UKi/",
    "\\.mp3$",
    full.names = TRUE
  )

  sample_audio(infiles[[1]], outdir = "/home/hoelk/Videos/vis/aout/", 10)


})
