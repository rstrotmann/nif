
test_that("nif summary works", {
  temp <- examplinib_sad_nif %>%
    summary()
  expect_s3_class(temp, "summary_nif")
  expect_output(print(summary(examplinib_sad_nif)))
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


test_that("covariate_hist works", {
  expect_no_error(examplinib_poc_nif %>%
    covariate_hist("AGE"))
  expect_no_error(examplinib_poc_nif %>%
                    covariate_hist("AGE", group = "SEX"))
})


test_that("covariate_barplot works", {
  expect_no_error(examplinib_poc_nif %>%
                    covariate_barplot("RACE"))
  expect_no_error(examplinib_poc_nif %>%
                    covariate_barplot("RACE", group = "SEX"))
})


test_that("x_by_y works", {
  expect_no_error(wt_by_sex(examplinib_poc_nif))
  expect_no_error(wt_by_race(examplinib_poc_nif))
  expect_no_error(wt_by_ht(examplinib_poc_nif))
  expect_no_error(ht_by_wt(examplinib_poc_nif))
  expect_no_error(bmi_by_age(examplinib_poc_nif))
})


test_that("time_by_ntime works", {
  expect_no_error(time_by_ntime(examplinib_sad_nif))
})


test_that("administration_summary works", {
  expect_equal(
    nrow(administration_summary(examplinib_poc_nif)),
    1
  )
})


test_that("mean_dose_plot works", {
  expect_no_error(
    mean_dose_plot(examplinib_poc_nif)
  )
})


test_that("subs_per_dose_level works", {
  expect_equal(
    subs_per_dose_level(examplinib_sad_nif)$N,
    c(3L, 3L, 3L, 3L, 6L, 3L, 18L, 6L, 3L)
  )
})


test_that("obs_per_dose_level works", {
  expect_equal(
    obs_per_dose_level(examplinib_sad_nif)$N,
    c(51L, 51L, 51L, 51L, 102L, 51L, 306L, 102L, 51L)
  )
})


# test_that("plot_old works", {
#   expect_no_error(p <- plot_old(examplinib_sad_nif, "RS2023"))
#   expect_no_error(p <- plot_old(examplinib_sad_nif, "RS2023", mean = TRUE))
# })




