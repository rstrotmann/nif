
test_that("nif summary works", {
  temp <- examplinib_sad_nif %>%
    summary()
  expect_s3_class(temp, "summary_nif")
  #expect_equal(length(temp), 14)
})


test_that("nif_summary_plot works", {
  # summary_plot <- function() {
  #   examplinib_sad_nif %>%
  #   summary() %>%
  #   plot()
  # }
  # expect_no_error(summary_plot())
  expect_no_error(
    examplinib_sad_nif %>%
      summary() %>%
      plot()
  )
})


test_that("nif_plot_id", {
  expect_no_error(
    nif_plot_id(examplinib_poc_nif, 1)
  )

  expect_no_error(
    nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023")
  )

  expect_no_error(
    nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")
  )

  expect_no_error(
    nif_plot_id(examplinib_poc_nif, 8, analyte="RS2023", imp="RS2023")
  )

  expect_no_error(
    nif_plot_id(examplinib_poc_nif, 8, analyte=c("RS2023", "RS2023487A"))
  )
})
















