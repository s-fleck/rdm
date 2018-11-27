context("average_color")


test_that("average_color works as expected", {
  tf <- rprojroot::find_testthat_root_file("testdata", "clip.mkv")
  skip_if_not(file.exists(tf))
  t <- average_frame_color(tf)
  expect_true(is.matrix(t))
})
