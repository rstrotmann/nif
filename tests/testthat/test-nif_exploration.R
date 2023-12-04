
test_that("nif summary works", {
  temp <- examplinib_sad_nif %>%
    summary()
  expect_s3_class(temp, "summary_nif")
  #expect_equal(length(temp), 14)
})


test_that("nif_summary_plot works", {
  summary_plot <- function() {
    examplinib_sad_nif %>%
    summary() %>%
    plot()
  }
  expect_no_error(summary_plot())
})
