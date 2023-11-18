
test_that("nif summary works", {
  temp <- examplinib_sad_nif %>%
    summary()
  expect_s3_class(temp, "summary_nif")
  expect_equal(length(temp), 14)
})


test_that("nif_summary_plot works", {
  temp <- examplinib_sad_nif %>%
    summary() %>%
    plot()
  expect_equal(length(temp), 8)
})
