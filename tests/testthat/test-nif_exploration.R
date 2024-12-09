
test_that("nif summary works", {
  temp <- examplinib_sad_nif %>%
    summary()
  expect_s3_class(temp, "summary_nif")
})


test_that("nif_summary_plot works", {
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


test_that("dose_plot_id works", {
  expect_no_error(dose_plot_id(examplinib_poc_nif, 18))
  expect_error(dose_plot_id(examplinib_poc_nif, 1000))
})















