make_nca_power_model_input <- function() {
  tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES, ~SEX,
      100,    "cmax",       10,  "M",
      200,    "cmax",       20,  "F",
      400,    "cmax",       40,  "M",
      100, "auclast",      100,  "M",
      200, "auclast",      200,  "F",
      400, "auclast",      400,  "M"
  )
}


test_that("nca_power_model returns a named list of ggplot objects", {
  result <- nca_power_model(make_nca_power_model_input(), parameter = "cmax")

  expect_type(result, "list")
  expect_named(result, "cmax")
  expect_s3_class(result$cmax, "ggplot")
  expect_match(result$cmax$labels$caption, "slope = 1")
  expect_equal(result$cmax$labels$y, "cmax")
  expect_equal(result$cmax$labels$x, "dose (mg)")
})


test_that("nca_power_model returns one plot per parameter", {
  result <- nca_power_model(
    make_nca_power_model_input(),
    parameter = c("cmax", "auclast")
  )

  expect_named(result, c("cmax", "auclast"))
  expect_s3_class(result$cmax, "ggplot")
  expect_s3_class(result$auclast, "ggplot")
  expect_match(result$cmax$labels$caption, "slope = 1")
  expect_match(result$auclast$labels$caption, "slope = 1")
})


test_that("nca_power_model auto-selects standard parameters when NULL", {
  result <- suppressWarnings(
    nca_power_model(make_nca_power_model_input(), parameter = NULL)
  )

  # auclast is present but not in the standard auto-select list
  expect_named(result, "cmax")
  expect_s3_class(result$cmax, "ggplot")
})


test_that("nca_power_model returns empty list when NULL and no standard parameters", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
      100, "auclast",      100,
      200, "auclast",      200
  )

  result <- nca_power_model(nca_data, parameter = NULL)

  expect_type(result, "list")
  expect_equal(length(result), 0)
})


test_that("nca_power_model prefers PPSTRESN over PPORRES", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPSTRESN, ~PPORRES,
      100,    "cmax",        10,      999,
      200,    "cmax",        20,      999,
      400,    "cmax",        40,      999
  )

  result <- nca_power_model(nca_data, parameter = "cmax")

  expect_match(result$cmax$labels$caption, "slope = 1")
})


test_that("nca_power_model uses PPORRES when PPSTRESN is absent", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
      100,    "cmax",       10,
      200,    "cmax",       20,
      400,    "cmax",       40
  )

  result <- nca_power_model(nca_data, parameter = "cmax")

  expect_s3_class(result$cmax, "ggplot")
  expect_match(result$cmax$labels$caption, "slope = 1")
})


test_that("nca_power_model filters non-positive values before fitting", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
       50,    "cmax",        0,
      100,    "cmax",      -10,
      200,    "cmax",       20,
      400,    "cmax",       40
  )

  result <- suppressWarnings(
    nca_power_model(nca_data, parameter = "cmax")
  )

  expect_s3_class(result$cmax, "ggplot")
  expect_match(result$cmax$labels$caption, "slope = 1")
})


test_that("nca_power_model errors when no positive values remain", {
  nca_zeros <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
      100,    "cmax",        0,
      200,    "cmax",        0
  )
  expect_error(
    nca_power_model(nca_zeros, parameter = "cmax"),
    "0 \\(non-NA\\) cases"
  )

  nca_neg <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPORRES,
      100,    "cmax",      -10,
      200,    "cmax",      -20
  )
  expect_error(
    nca_power_model(nca_neg, parameter = "cmax"),
    "0 \\(non-NA\\) cases"
  )
})


test_that("nca_power_model errors for unknown parameter", {
  expect_error(
    nca_power_model(make_nca_power_model_input(), parameter = "NOPE"),
    "0 \\(non-NA\\) cases"
  )
})


test_that("nca_power_model supports grouping", {
  result <- nca_power_model(
    make_nca_power_model_input(),
    parameter = "cmax",
    group = "SEX"
  )

  expect_s3_class(result$cmax, "ggplot")
  expect_equal(result$cmax$labels$colour, "SEX")

  built <- ggplot2::ggplot_build(result$cmax)
  point_layer <- built$data[[3]]
  expect_true("colour" %in% names(point_layer))
  expect_equal(length(unique(point_layer$colour)), 2)
})


test_that("nca_power_model errors when group column is missing", {
  expect_error(
    nca_power_model(
      make_nca_power_model_input(),
      parameter = "cmax",
      group = "NOPE"
    ),
    "NOPE"
  )
})


test_that("nca_power_model applies title when provided", {
  result <- nca_power_model(
    make_nca_power_model_input(),
    parameter = "cmax",
    title = "Power model"
  )

  expect_equal(result$cmax$labels$title, "Power model")
})


test_that("nca_power_model validates input type and required fields", {
  expect_error(
    nca_power_model(NULL, parameter = "cmax"),
    "Input must be a data frame!"
  )

  expect_error(
    nca_power_model(list(DOSE = 1), parameter = "cmax"),
    "Input must be a data frame!"
  )

  expect_error(
    nca_power_model(
      data.frame(PPTESTCD = "cmax", PPORRES = 1),
      parameter = "cmax"
    ),
    "Missing fields in input: DOSE"
  )

  expect_error(
    nca_power_model(
      data.frame(DOSE = 100, PPORRES = 1),
      parameter = "cmax"
    ),
    "Missing fields in input: PPTESTCD"
  )

  expect_error(
    nca_power_model(
      data.frame(DOSE = 100, PPTESTCD = "cmax"),
      parameter = "cmax"
    ),
    "Neither PPSTRESN nor PPORRES found in input!"
  )
})


test_that("nca_power_model validates argument types and ranges", {
  nca_data <- make_nca_power_model_input()

  expect_error(
    nca_power_model(nca_data, parameter = 1),
    "parameter must be a character value"
  )

  expect_error(
    nca_power_model(nca_data, parameter = "cmax", group = 1),
    "group must be a character value"
  )

  expect_error(
    nca_power_model(nca_data, parameter = "cmax", title = 1),
    "title must be a character value"
  )

  expect_error(
    nca_power_model(nca_data, parameter = "cmax", size = "big"),
    "size must be a numeric value"
  )

  expect_error(
    nca_power_model(nca_data, parameter = "cmax", size = -1),
    "Size must be positive!"
  )

  expect_error(
    nca_power_model(nca_data, parameter = "cmax", alpha = "opaque"),
    "alpha must be a numeric value"
  )

  expect_error(
    nca_power_model(nca_data, parameter = "cmax", alpha = -0.1),
    "Alpha must be positive!"
  )
})


test_that("nca_power_model works with nca() output", {
  suppressWarnings({
    nca_res <- nca(examplinib_sad_nif, analyte = "RS2023", silent = TRUE)
  })

  result <- nca_power_model(nca_res, parameter = "cmax")

  expect_named(result, "cmax")
  expect_s3_class(result$cmax, "ggplot")
  expect_match(result$cmax$labels$caption, "slope =")
})


test_that("nca_power_model works with nca_from_pp-style PPSTRESN data", {
  nca_data <- tibble::tribble(
    ~DOSE, ~PPTESTCD, ~PPSTRESN,
      100,    "CMAX",        50,
      200,    "CMAX",       100,
      400,    "CMAX",       200
  )

  result <- nca_power_model(nca_data, parameter = "CMAX")

  expect_named(result, "CMAX")
  expect_s3_class(result$CMAX, "ggplot")
  expect_match(result$CMAX$labels$caption, "slope = 1")
})
