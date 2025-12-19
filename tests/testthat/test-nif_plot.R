test_that("make_plot_data_set", {
  expect_no_error(
    temp <- make_plot_data_set(
      examplinib_fe_nif,
      color = "FASTED"
    )
  )
})


test_that("plot.nif", {
  expect_no_error(
    plot.nif(examplinib_fe_nif, facet = NULL)
  )

  expect_no_error(
    plot.nif(examplinib_sad_nif,
      log = T, mean = T, points = T,
      admin = "RS2023"
    )
  )

  expect_no_error(
    plot.nif(examplinib_sad_nif,
      log = T, mean = T, points = T, dose_norm = T,
      admin = "RS2023"
    )
  )

  expect_no_error(
    plot.nif(examplinib_sad_nif,
      mean = T, points = F, color = "RACE",
      log = T
    )
  )

  expect_no_error(
    plot(examplinib_sad_nif,
      mean = F, points = T, dose_norm = F,
      facet = "RACE", log = T, max_time = 72
    )
  )

  expect_no_error(
    plot(examplinib_fe_nif,
      points = T, color = c("FASTED"), log = T,
      mean = T, max_time = 12, admin = "RS2023"
    )
  )

  expect_no_error(
    plot(examplinib_fe_nif,
      points = T, facet = "FASTED", color = "SEX",
      log = T, mean = T, max_time = 24, admin = "RS2023"
    )
  )

  expect_no_error(
    plot(examplinib_fe_nif,
      points = T, color = c("FASTED", "SEX"),
      log = T, mean = T, max_time = 24, admin = "RS2023"
    )
  )

  expect_no_error(
    plot(examplinib_poc_nif,
      analyte = "RS2023", dose = 500, points = T,
      lines = F, log = T, color = "DI", mean = T, time = "TAD"
    )
  )

  expect_no_error(
    plot(examplinib_poc_nif,
      analyte = "RS2023", dose = 500, points = T,
      admin = "RS2023", max_time = 300
    )
  )

  expect_no_error(
    plot(examplinib_poc_nif,
      points = T, facet = "SEX", dose_norm = T,
      admin = "RS2023", max_time = 300
    )
  )

  expect_no_error(
    plot(examplinib_poc_nif,
      points = T, dose_norm = T, facet = NULL,
      color = c("SEX", "DOSE"), max_time = 300, time = "TAD",
      lines = F, size = 4, alpha = 0.5
    )
  )

  expect_no_error(
    plot(examplinib_poc_nif,
      points = T, dose_norm = T, facet = NULL,
      color = c("SEX", "DOSE"), max_time = 24
    )
  )

  expect_no_error(
    plot(examplinib_poc_nif,
      points = T, dose_norm = T, facet = "ANALYTE",
      color = c("SEX", "DOSE"), max_time = 24, scales = "free"
    )
  )
})
