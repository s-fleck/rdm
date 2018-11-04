context("sample_clips")


td <- tempdir()

test_that("sample_clips works as expected", {
  infiles <-  file.path(
    "/media/syno16/shows/Twin Peaks/",
    c(
      "Season 02/Twin Peaks - S02E22 - Beyond Life and Death.mkv",
      "Season 03/Twin Peaks - S03E08 - The Return, Part 8.mkv",
      "Season 03/Twin Peaks - S03E17 - The Return, Part 17.mkv",
      "Season 03/Twin Peaks - S03E18 - The Return, Part 18.mkv"
    )
  )


  file.remove(list.files(td, ".mkv$", full.names = TRUE))

  expect_output(
    sample_clips(infiles[[1]], n = 10, lengths = c(2, 4, 8, 16), outdir = td)
  )

  outp <- list.files(td, ".mkv$", full.names = TRUE)
  expect_identical(length(outp),  10L)
})




test_that("rorschach_drama works as expected", {
  infiles <-  list.files(td, "\\.mkv$", full.names = TRUE)[1:4]

  rorschach_drama(
    infiles,
    resolution = c(1920, 1080) / 4,
    outfile = "blubb.mkv",
    batch_size = 2
  )

})
