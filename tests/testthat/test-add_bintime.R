test_that("add_bintime works with basic input", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   12,    0,     "DRUG",  28,  0,    2,
    2,   18,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("BINTIME" %in% names(result))
  expect_true("BIN_LEFT" %in% names(result))
  expect_true("BIN_RIGHT" %in% names(result))
})

test_that("add_bintime returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   12,    0,     "DRUG",  28,  0,    2,
    2,   18,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_s3_class(result, "nif")
})

test_that("add_bintime bins span observation times", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   4,     0,     "DRUG",  20,  0,    2,
    1,   8,     0,     "DRUG",  30,  0,    2,
    1,   16,    0,     "DRUG",  25,  0,    2,
    1,   24,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   10,    0,     "DRUG",  28,  0,    2,
    2,   20,    0,     "DRUG",  20,  0,    2,
    2,   24,    0,     "DRUG",  10,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  obs <- result %>% filter(EVID == 0 & !is.na(BIN_LEFT))
  expect_true(all(obs$active_time >= obs$BIN_LEFT))
  expect_true(all(obs$active_time <= obs$BIN_RIGHT))
})

test_that("add_bintime BIN_LEFT < BIN_RIGHT for all bins", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   2,     0,     "DRUG",  10,  0,    2,
    1,   6,     0,     "DRUG",  20,  0,    2,
    1,   12,    0,     "DRUG",  30,  0,    2,
    1,   24,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   3,     0,     "DRUG",  12,  0,    2,
    2,   8,     0,     "DRUG",  22,  0,    2,
    2,   15,    0,     "DRUG",  28,  0,    2,
    2,   22,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  obs <- result %>% filter(!is.na(BIN_LEFT))
  expect_true(all(obs$BIN_LEFT < obs$BIN_RIGHT))
})

test_that("add_bintime BINTIME is numeric", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   12,    0,     "DRUG",  28,  0,    2,
    2,   18,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_type(result$BINTIME, "double")
  expect_type(result$BIN_LEFT, "double")
  expect_type(result$BIN_RIGHT, "double")
})

test_that("add_bintime preserves original columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~EXTRA,
    1,   0,     1,     "DRUG",  NA,  100,  1,    "A",
    1,   2,     0,     "DRUG",  10,  0,    2,    "B",
    1,   6,     0,     "DRUG",  20,  0,    2,    "C",
    1,   12,    0,     "DRUG",  30,  0,    2,    "D",
    1,   24,    0,     "DRUG",  15,  0,    2,    "E",
    2,   0,     1,     "DRUG",  NA,  100,  1,    "F",
    2,   3,     0,     "DRUG",  12,  0,    2,    "G",
    2,   8,     0,     "DRUG",  22,  0,    2,    "H",
    2,   15,    0,     "DRUG",  28,  0,    2,    "I",
    2,   22,    0,     "DRUG",  18,  0,    2,    "J"
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("EXTRA" %in% names(result))
  expect_equal(result$EXTRA, test_data$EXTRA)
  expect_true(all(c("ID", "TIME", "EVID", "DV", "AMT", "CMT") %in% names(result)))
})

test_that("add_bintime preserves row count", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   12,    0,     "DRUG",  28,  0,    2,
    2,   18,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_equal(nrow(result), nrow(test_data))
})

test_that("add_bintime removes .BINTIME_INDEX column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   12,    0,     "DRUG",  28,  0,    2,
    2,   18,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_false(".BINTIME_INDEX" %in% names(result))
})

test_that("add_bintime works with data that already has TAFD", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,
    1,   1,     0,     "DRUG",  10,  0,    2,    1,
    1,   5,     0,     "DRUG",  20,  0,    2,    5,
    1,   10,    0,     "DRUG",  30,  0,    2,    10,
    1,   20,    0,     "DRUG",  15,  0,    2,    20,
    2,   0,     1,     "DRUG",  NA,  100,  1,    0,
    2,   2,     0,     "DRUG",  12,  0,    2,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,    6,
    2,   12,    0,     "DRUG",  28,  0,    2,    12,
    2,   18,    0,     "DRUG",  18,  0,    2,    18
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("BINTIME" %in% names(result))
  expect_true("BIN_LEFT" %in% names(result))
  expect_true("BIN_RIGHT" %in% names(result))
})

test_that("add_bintime default method is fisher", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   2,     0,     "DRUG",  10,  0,    2,
    1,   6,     0,     "DRUG",  20,  0,    2,
    1,   12,    0,     "DRUG",  30,  0,    2,
    1,   24,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   3,     0,     "DRUG",  12,  0,    2,
    2,   8,     0,     "DRUG",  22,  0,    2,
    2,   15,    0,     "DRUG",  28,  0,    2,
    2,   22,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result_default <- add_bintime(test_data)
  result_fisher <- add_bintime(test_data, method = "fisher")

  expect_equal(result_default$BINTIME, result_fisher$BINTIME)
  expect_equal(result_default$BIN_LEFT, result_fisher$BIN_LEFT)
  expect_equal(result_default$BIN_RIGHT, result_fisher$BIN_RIGHT)
})

test_that("add_bintime works with different methods", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   3,     0,     "DRUG",  20,  0,    2,
    1,   6,     0,     "DRUG",  30,  0,    2,
    1,   10,    0,     "DRUG",  25,  0,    2,
    1,   15,    0,     "DRUG",  18,  0,    2,
    1,   24,    0,     "DRUG",  12,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   4,     0,     "DRUG",  22,  0,    2,
    2,   8,     0,     "DRUG",  28,  0,    2,
    2,   12,    0,     "DRUG",  20,  0,    2,
    2,   18,    0,     "DRUG",  14,  0,    2,
    2,   24,    0,     "DRUG",  8,   0,    2
  ) %>%
    nif()

  methods <- c("jenks", "kmeans", "pretty", "quantile", "hclust",
               "sd", "fisher")

  for (m in methods) {
    result <- add_bintime(test_data, method = m)
    expect_true("BINTIME" %in% names(result),
                info = paste("Method:", m))
    expect_true("BIN_LEFT" %in% names(result),
                info = paste("Method:", m))
    expect_true("BIN_RIGHT" %in% names(result),
                info = paste("Method:", m))
  }
})

test_that("add_bintime rejects invalid method", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2
  ) %>%
    nif()

  expect_error(
    add_bintime(test_data, method = "invalid_method"),
    "not implemented"
  )
})

test_that("add_bintime rejects non-character method", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2
  ) %>%
    nif()

  expect_error(add_bintime(test_data, method = 123))
  expect_error(add_bintime(test_data, method = TRUE))
})

test_that("add_bintime rejects non-nif input", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2
  )

  expect_error(
    add_bintime(test_data),
    "Input must be a nif object"
  )
})

test_that("add_bintime default time is TAFD", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,
    1,   1,     0,     "DRUG",  10,  0,    2,    1,
    1,   5,     0,     "DRUG",  20,  0,    2,    5,
    1,   10,    0,     "DRUG",  30,  0,    2,    10,
    1,   20,    0,     "DRUG",  15,  0,    2,    20,
    2,   0,     1,     "DRUG",  NA,  100,  1,    0,
    2,   2,     0,     "DRUG",  12,  0,    2,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,    6,
    2,   12,    0,     "DRUG",  28,  0,    2,    12,
    2,   18,    0,     "DRUG",  18,  0,    2,    18
  ) %>%
    nif()

  result_default <- add_bintime(test_data)
  result_tafd <- add_bintime(test_data, time = "TAFD")

  expect_equal(result_default$BINTIME, result_tafd$BINTIME)
})

test_that("add_bintime works with custom time field", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD, ~TAD,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,     0,
    1,   1,     0,     "DRUG",  10,  0,    2,    1,     1,
    1,   5,     0,     "DRUG",  20,  0,    2,    5,     5,
    1,   10,    0,     "DRUG",  30,  0,    2,    10,    10,
    1,   20,    0,     "DRUG",  15,  0,    2,    20,    20,
    2,   0,     1,     "DRUG",  NA,  100,  1,    0,     0,
    2,   2,     0,     "DRUG",  12,  0,    2,    2,     2,
    2,   6,     0,     "DRUG",  22,  0,    2,    6,     6,
    2,   12,    0,     "DRUG",  28,  0,    2,    12,    12,
    2,   18,    0,     "DRUG",  18,  0,    2,    18,    18
  ) %>%
    nif()

  result <- add_bintime(test_data, time = "TAD")

  expect_true("BINTIME" %in% names(result))
  expect_true("BIN_LEFT" %in% names(result))
  expect_true("BIN_RIGHT" %in% names(result))
})

test_that("add_bintime with group parameter produces correct output columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD, ~GRP,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,     "A",
    1,   1,     0,     "DRUG",  10,  0,    2,    1,     "A",
    1,   5,     0,     "DRUG",  20,  0,    2,    5,     "A",
    1,   10,    0,     "DRUG",  30,  0,    2,    10,    "A",
    1,   20,    0,     "DRUG",  15,  0,    2,    20,    "A",
    2,   0,     1,     "DRUG",  NA,  100,  1,    0,     "B",
    2,   2,     0,     "DRUG",  12,  0,    2,    2,     "B",
    2,   6,     0,     "DRUG",  22,  0,    2,    6,     "B",
    2,   12,    0,     "DRUG",  28,  0,    2,    12,    "B",
    2,   18,    0,     "DRUG",  18,  0,    2,    18,    "B"
  ) %>%
    nif()

  result <- add_bintime(test_data, group = "GRP")

  expect_true("BINTIME" %in% names(result))
  expect_true("BIN_LEFT" %in% names(result))
  expect_true("BIN_RIGHT" %in% names(result))
  expect_true("GRP" %in% names(result))
  expect_s3_class(result, "nif")
})

test_that("add_bintime with group preserves row count", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD, ~GRP,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,     "A",
    1,   1,     0,     "DRUG",  10,  0,    2,    1,     "A",
    1,   5,     0,     "DRUG",  20,  0,    2,    5,     "A",
    1,   10,    0,     "DRUG",  30,  0,    2,    10,    "A",
    1,   20,    0,     "DRUG",  15,  0,    2,    20,    "A",
    2,   0,     1,     "DRUG",  NA,  100,  1,    0,     "B",
    2,   2,     0,     "DRUG",  12,  0,    2,    2,     "B",
    2,   6,     0,     "DRUG",  22,  0,    2,    6,     "B",
    2,   12,    0,     "DRUG",  28,  0,    2,    12,    "B",
    2,   18,    0,     "DRUG",  18,  0,    2,    18,    "B"
  ) %>%
    nif()

  result <- add_bintime(test_data, group = "GRP")

  expect_equal(nrow(result), nrow(test_data))
})

test_that("add_bintime with multiple group variables works", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD, ~GRP1, ~GRP2,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,     "A",   "X",
    1,   1,     0,     "DRUG",  10,  0,    2,    1,     "A",   "X",
    1,   5,     0,     "DRUG",  20,  0,    2,    5,     "A",   "X",
    1,   10,    0,     "DRUG",  30,  0,    2,    10,    "A",   "X",
    1,   20,    0,     "DRUG",  15,  0,    2,    20,    "A",   "X",
    2,   0,     1,     "DRUG",  NA,  100,  1,    0,     "B",   "Y",
    2,   2,     0,     "DRUG",  12,  0,    2,    2,     "B",   "Y",
    2,   6,     0,     "DRUG",  22,  0,    2,    6,     "B",   "Y",
    2,   12,    0,     "DRUG",  28,  0,    2,    12,    "B",   "Y",
    2,   18,    0,     "DRUG",  18,  0,    2,    18,    "B",   "Y"
  ) %>%
    nif()

  result <- add_bintime(test_data, group = c("GRP1", "GRP2"))

  expect_true("BINTIME" %in% names(result))
  expect_true("GRP1" %in% names(result))
  expect_true("GRP2" %in% names(result))
  expect_s3_class(result, "nif")
})

test_that("add_bintime rejects missing group variable", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,
    1,   1,     0,     "DRUG",  10,  0,    2,    1,
    1,   5,     0,     "DRUG",  20,  0,    2,    5,
    1,   10,    0,     "DRUG",  30,  0,    2,    10,
    1,   20,    0,     "DRUG",  15,  0,    2,    20
  ) %>%
    nif()

  expect_error(
    add_bintime(test_data, group = "NONEXISTENT"),
    "not found in data set"
  )
})

test_that("add_bintime with group=NULL is same as no group", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   12,    0,     "DRUG",  28,  0,    2,
    2,   18,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result_null <- add_bintime(test_data, group = NULL)
  result_default <- add_bintime(test_data)

  expect_equal(result_null$BINTIME, result_default$BINTIME)
  expect_equal(result_null$BIN_LEFT, result_default$BIN_LEFT)
  expect_equal(result_null$BIN_RIGHT, result_default$BIN_RIGHT)
})

test_that("add_bintime works with multiple subjects", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   2,     0,     "DRUG",  10,  0,    2,
    1,   8,     0,     "DRUG",  20,  0,    2,
    1,   16,    0,     "DRUG",  30,  0,    2,
    1,   24,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   3,     0,     "DRUG",  12,  0,    2,
    2,   7,     0,     "DRUG",  22,  0,    2,
    2,   14,    0,     "DRUG",  28,  0,    2,
    2,   20,    0,     "DRUG",  18,  0,    2,
    3,   0,     1,     "DRUG",  NA,  100,  1,
    3,   1,     0,     "DRUG",  8,   0,    2,
    3,   5,     0,     "DRUG",  18,  0,    2,
    3,   11,    0,     "DRUG",  25,  0,    2,
    3,   22,    0,     "DRUG",  13,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("BINTIME" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
  expect_true(all(unique(result$ID) %in% c(1, 2, 3)))
})

test_that("add_bintime BINTIME falls within bin boundaries", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   4,     0,     "DRUG",  20,  0,    2,
    1,   8,     0,     "DRUG",  30,  0,    2,
    1,   16,    0,     "DRUG",  25,  0,    2,
    1,   24,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   10,    0,     "DRUG",  28,  0,    2,
    2,   20,    0,     "DRUG",  20,  0,    2,
    2,   24,    0,     "DRUG",  10,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  obs <- result %>% filter(!is.na(BINTIME))
  expect_true(all(obs$BINTIME >= obs$BIN_LEFT))
  expect_true(all(obs$BINTIME <= obs$BIN_RIGHT))
})

test_that("add_bintime bins are non-overlapping", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   4,     0,     "DRUG",  20,  0,    2,
    1,   8,     0,     "DRUG",  30,  0,    2,
    1,   16,    0,     "DRUG",  25,  0,    2,
    1,   24,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   10,    0,     "DRUG",  28,  0,    2,
    2,   20,    0,     "DRUG",  20,  0,    2,
    2,   24,    0,     "DRUG",  10,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  bins <- result %>%
    filter(!is.na(BIN_LEFT)) %>%
    select(BIN_LEFT, BIN_RIGHT) %>%
    distinct() %>%
    arrange(BIN_LEFT)

  if (nrow(bins) > 1) {
    for (i in seq_len(nrow(bins) - 1)) {
      expect_lte(bins$BIN_RIGHT[i], bins$BIN_LEFT[i + 1] + .Machine$double.eps)
    }
  }
})

test_that("add_bintime works with widely spaced time points", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   100,   0,     "DRUG",  20,  0,    2,
    1,   500,   0,     "DRUG",  30,  0,    2,
    1,   1000,  0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   50,    0,     "DRUG",  12,  0,    2,
    2,   200,   0,     "DRUG",  22,  0,    2,
    2,   600,   0,     "DRUG",  28,  0,    2,
    2,   900,   0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("BINTIME" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
})

test_that("add_bintime works with closely spaced time points", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   0.1,   0,     "DRUG",  10,  0,    2,
    1,   0.2,   0,     "DRUG",  20,  0,    2,
    1,   0.5,   0,     "DRUG",  30,  0,    2,
    1,   1.0,   0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   0.15,  0,     "DRUG",  12,  0,    2,
    2,   0.3,   0,     "DRUG",  22,  0,    2,
    2,   0.6,   0,     "DRUG",  28,  0,    2,
    2,   0.9,   0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("BINTIME" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
})

test_that("add_bintime rejects non-character group parameter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,
    1,   1,     0,     "DRUG",  10,  0,    2,    1,
    1,   5,     0,     "DRUG",  20,  0,    2,    5,
    1,   10,    0,     "DRUG",  30,  0,    2,    10,
    1,   20,    0,     "DRUG",  15,  0,    2,    20
  ) %>%
    nif()

  expect_error(add_bintime(test_data, group = 123))
  expect_error(add_bintime(test_data, group = TRUE))
})

test_that("add_bintime different methods produce different bins", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   3,     0,     "DRUG",  20,  0,    2,
    1,   6,     0,     "DRUG",  30,  0,    2,
    1,   10,    0,     "DRUG",  25,  0,    2,
    1,   15,    0,     "DRUG",  18,  0,    2,
    1,   24,    0,     "DRUG",  12,  0,    2,
    1,   36,    0,     "DRUG",  8,   0,    2,
    1,   48,    0,     "DRUG",  5,   0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   4,     0,     "DRUG",  22,  0,    2,
    2,   8,     0,     "DRUG",  28,  0,    2,
    2,   12,    0,     "DRUG",  20,  0,    2,
    2,   18,    0,     "DRUG",  14,  0,    2,
    2,   24,    0,     "DRUG",  8,   0,    2,
    2,   36,    0,     "DRUG",  6,   0,    2,
    2,   48,    0,     "DRUG",  3,   0,    2
  ) %>%
    nif()

  result_pretty <- add_bintime(test_data, method = "pretty")
  result_quantile <- add_bintime(test_data, method = "quantile")

  bins_pretty <- result_pretty %>%
    filter(!is.na(BIN_LEFT)) %>%
    select(BIN_LEFT, BIN_RIGHT) %>%
    distinct()
  bins_quantile <- result_quantile %>%
    filter(!is.na(BIN_LEFT)) %>%
    select(BIN_LEFT, BIN_RIGHT) %>%
    distinct()

  expect_false(identical(bins_pretty, bins_quantile))
})

test_that("add_bintime with group applies binning independently per group", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD, ~DOSE,
    1,   0,     1,     "DRUG",  NA,  100,  1,    0,     100,
    1,   1,     0,     "DRUG",  10,  0,    2,    1,     100,
    1,   5,     0,     "DRUG",  20,  0,    2,    5,     100,
    1,   10,    0,     "DRUG",  30,  0,    2,    10,    100,
    1,   20,    0,     "DRUG",  15,  0,    2,    20,    100,
    2,   0,     1,     "DRUG",  NA,  200,  1,    0,     200,
    2,   2,     0,     "DRUG",  24,  0,    2,    2,     200,
    2,   6,     0,     "DRUG",  44,  0,    2,    6,     200,
    2,   12,    0,     "DRUG",  56,  0,    2,    12,    200,
    2,   18,    0,     "DRUG",  36,  0,    2,    18,    200
  ) %>%
    nif()

  result_grouped <- add_bintime(test_data, group = "DOSE")
  result_ungrouped <- add_bintime(test_data)

  bins_grouped <- result_grouped %>%
    filter(!is.na(BIN_LEFT)) %>%
    select(DOSE, BIN_LEFT, BIN_RIGHT) %>%
    distinct()
  bins_ungrouped <- result_ungrouped %>%
    filter(!is.na(BIN_LEFT)) %>%
    select(BIN_LEFT, BIN_RIGHT) %>%
    distinct()

  expect_false(identical(
    sort(unique(bins_grouped$BIN_LEFT)),
    sort(unique(bins_ungrouped$BIN_LEFT))
  ))
})

test_that("add_bintime handles observations before first dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   -2,    0,     "DRUG",  5,   0,    2,
    1,   -1,    0,     "DRUG",  8,   0,    2,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    1,   10,    0,     "DRUG",  30,  0,    2,
    1,   20,    0,     "DRUG",  15,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   2,     0,     "DRUG",  12,  0,    2,
    2,   6,     0,     "DRUG",  22,  0,    2,
    2,   12,    0,     "DRUG",  28,  0,    2,
    2,   18,    0,     "DRUG",  18,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("BINTIME" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
})

test_that("add_bintime with single observation per subject works", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   5,     0,     "DRUG",  20,  0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   3,     0,     "DRUG",  15,  0,    2,
    3,   0,     1,     "DRUG",  NA,  100,  1,
    3,   8,     0,     "DRUG",  25,  0,    2,
    4,   0,     1,     "DRUG",  NA,  100,  1,
    4,   12,    0,     "DRUG",  18,  0,    2,
    5,   0,     1,     "DRUG",  NA,  100,  1,
    5,   1,     0,     "DRUG",  10,  0,    2
  ) %>%
    nif()

  result <- add_bintime(test_data)

  expect_true("BINTIME" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
})
