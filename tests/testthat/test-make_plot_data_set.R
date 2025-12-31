test_that("make_plot_data_set handles basic functionality", {
  # Create a simple test NIF object
  test_nif <- tibble::tribble(
    ~ID, ~ANALYTE,  ~PARENT, ~EVID, ~DOSE, ~TIME, ~NTIME, ~TAFD, ~TAD, ~DV, ~DVCFB, ~CMT, ~AMT,
    1, "RS2023", "RS2023",     1,   100,     0,      0,     0,    0,  NA,     NA,    1,  100,
    1, "RS2023", "RS2023",     0,   100,     1,      1,     1,    1,  10,     10,    1,    0,
    1, "RS2023", "RS2023",     0,   100,     2,      2,     2,    2,  20,     20,    1,    0,
    2, "RS2023", "RS2023",     1,   200,     0,      0,     0,    0,  NA,     NA,    1,  200,
    2, "RS2023", "RS2023",     0,   200,     1,      1,     1,    1,  15,     15,    1,    0,
    2, "RS2023", "RS2023",     0,   200,     2,      2,     2,    2,  25,     25,    1,    0
  ) %>% as_nif()

  # Test basic functionality
  result <- make_plot_data_set(test_nif)
  expect_named(result, c("data", "group", "color", "facet"))
  expect_s3_class(result$data, "data.frame")
  expect_equal(result$group, "ID")
  expect_null(result$color)
  expect_equal(result$facet, "DOSE")
})

test_that("make_plot_data_set handles time parameters correctly", {
  test_nif <- tibble::tribble(
    ~ID, ~ANALYTE,  ~PARENT, ~EVID, ~DOSE, ~TIME, ~NTIME, ~TAFD, ~TAD, ~DV, ~DVCFB, ~CMT, ~AMT,
    1, "RS2023", "RS2023",     1,   100,     0,      0,     0,    0,  NA,     NA,    1,    100,
    1, "RS2023", "RS2023",     0,   100,     1,      1,     1,    1,  10,     10,    1,      0,
    1, "RS2023", "RS2023",     0,   100,     2,      2,     2,    2,  20,     20,    1,      0
  ) %>% as_nif()

  # Test different time parameters
  result_tafd <- make_plot_data_set(test_nif, time = "TAFD")
  result_tad <- make_plot_data_set(test_nif, time = "TAD")
  result_time <- make_plot_data_set(test_nif, time = "TIME")
  result_ntime <- make_plot_data_set(test_nif, time = "NTIME")

  expect_equal(result_tafd$data$active_time, c(0, 1, 2))
  expect_equal(result_tad$data$active_time, c(0, 1, 2))
  expect_equal(result_time$data$active_time, c(0, 1, 2))
  expect_equal(result_ntime$data$active_time, c(0, 1, 2))
})

test_that("make_plot_data_set handles color and facet parameters", {
  test_nif <- tibble::tribble(
    ~ID, ~ANALYTE,  ~PARENT, ~EVID, ~DOSE, ~TIME, ~NTIME, ~TAFD, ~TAD, ~DV, ~DVCFB,   ~RACE, ~SEX, ~CMT, ~AMT,
    1, "RS2023", "RS2023",     1,   100,     0,      0,     0,    0,  NA,     NA, "White",  "M",    1,  100,
    1, "RS2023", "RS2023",     0,   100,     1,      1,     1,    1,  10,     10, "White",  "M",    1,    0,
    1, "RS2023", "RS2023",     0,   100,     2,      2,     2,    2,  20,     20, "White",  "M",    1,    0,
    2, "RS2023", "RS2023",     1,   200,     0,      0,     0,    0,  NA,     NA, "Asian",  "F",    1,  100,
    2, "RS2023", "RS2023",     0,   200,     1,      1,     1,    1,  15,     15, "Asian",  "F",    1,    0,
    2, "RS2023", "RS2023",     0,   200,     2,      2,     2,    2,  25,     25, "Asian",  "F",    1,    0
  ) %>% as_nif()

  # Test single color
  result_color <- make_plot_data_set(test_nif, color = "RACE")
  expect_true("COLOR" %in% names(result_color$data))
  expect_equal(result_color$color, "RACE")

  # Test multiple colors
  result_colors <- make_plot_data_set(test_nif, color = c("RACE", "SEX"))
  expect_true("COLOR" %in% names(result_colors$data))
  expect_equal(result_colors$color, c("RACE", "SEX"))

  # Test facet
  result_facet <- make_plot_data_set(test_nif, facet = "RACE")
  expect_true("FACET" %in% names(result_facet$data))
  expect_equal(result_facet$facet, "RACE")
})

test_that("make_plot_data_set handles cfb and dose_norm parameters", {
  test_nif <- tibble::tribble(
    ~ID, ~ANALYTE,  ~PARENT, ~EVID, ~DOSE, ~TIME, ~NTIME, ~TAFD, ~TAD, ~DV, ~DVCFB, ~CMT, ~AMT,
    1, "RS2023", "RS2023",     1,   100,     0,      0,     0,    0,  NA,     NA,    1,  100,
    1, "RS2023", "RS2023",     0,   100,     1,      1,     1,    1,  10,      5,    1,    0,
    1, "RS2023", "RS2023",     0,   100,     2,      2,     2,    2,  20,     15,    1,    0
  ) %>% as_nif()

  # Test cfb
  result_cfb <- make_plot_data_set(test_nif, cfb = TRUE)
  expect_equal(result_cfb$data$DV[result_cfb$data$EVID == 0], c(5, 15))

  # Test dose_norm
  result_dose_norm <- make_plot_data_set(test_nif, dose_norm = TRUE)
  expect_equal(result_dose_norm$data$DV[result_dose_norm$data$EVID == 0], c(0.1, 0.2))
})

test_that("make_plot_data_set handles min_time and max_time parameters", {
  test_nif <- tibble::tribble(
    ~ID, ~ANALYTE,  ~PARENT, ~EVID, ~DOSE, ~TIME, ~NTIME, ~TAFD, ~TAD, ~DV, ~DVCFB, ~CMT, ~AMT,
    1, "RS2023", "RS2023",     1,   100,     0,      0,     0,    0,  NA,     NA,    1,    0,
    1, "RS2023", "RS2023",     0,   100,     1,      1,     1,    1,  10,     10,    1,    0,
    1, "RS2023", "RS2023",     0,   100,     2,      2,     2,    2,  20,     20,    1,    0,
    1, "RS2023", "RS2023",     0,   100,     3,      3,     3,    3,  30,     30,    1,    0
  ) %>% as_nif()

  # Test time filtering
  result_filtered <- make_plot_data_set(test_nif, min_time = 1, max_time = 2)
  expect_equal(nrow(result_filtered$data), 2) # Only rows with time 1 and 2
  expect_equal(result_filtered$data$active_time, c(1, 2))
})

test_that("make_plot_data_set handles multiple analytes", {
  test_nif <- tibble::tribble(
    ~ID, ~ANALYTE,  ~PARENT, ~EVID, ~DOSE, ~TIME, ~NTIME, ~TAFD, ~TAD, ~DV, ~DVCFB, ~CMT, ~AMT,
    1, "RS2023", "RS2023",     1,   100,     0,      0,     0,    0,  NA,     NA,    1,  100,
    1, "RS2023", "RS2023",     0,   100,     1,      1,     1,    1,  10,     10,    1,    0,
    1,     "M1", "RS2023",     0,   100,     1,      1,     1,    1,   5,      5,    2,    0,
    1,     "M2", "RS2023",     0,   100,     1,      1,     1,    1,   3,      3,    3,    0
  ) %>% as_nif()

  # Test multiple analytes
  result_multi <- make_plot_data_set(test_nif, analyte = c("RS2023", "M1"))
  expect_equal(unique(result_multi$data$ANALYTE), c("RS2023", "M1"))
  expect_true("ANALYTE" %in% result_multi$color)
})

test_that("make_plot_data_set handles invalid inputs", {
  test_nif <- tibble::tribble(
    ~ID, ~ANALYTE,  ~PARENT, ~EVID, ~DOSE, ~TIME, ~NTIME, ~TAFD, ~TAD, ~DV, ~DVCFB, ~CMT, ~AMT,
    1, "RS2023", "RS2023",     1,   100,     0,      0,     0,    0,  NA,     NA,    1,  100,
    1, "RS2023", "RS2023",     0,   100,     1,      1,     1,    1,  10,     10,    1,    0
  ) %>% as_nif()

  # Test invalid time parameter
  expect_error(make_plot_data_set(test_nif, time = "INVALID"))

  # Test invalid facet parameter
  expect_error(make_plot_data_set(test_nif, facet = "INVALID"))

  # Test invalid analyte
  # expect_error(make_plot_data_set(test_nif, analyte = "INVALID"))
})
