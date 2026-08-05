test_that("add_bintime1 works with basic input", {
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

  result <- add_bintime1(test_data, time = "TIME")

  expect_true("BINTIME" %in% names(result))
  expect_true("BIN_LEFT" %in% names(result))
  expect_true("BIN_RIGHT" %in% names(result))
  expect_false("active_time" %in% names(result))
  expect_false(".BINTIME_INDEX" %in% names(result))
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), nrow(test_data))
})


test_that("add_bintime1 uses shared bins across dose groups", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~DOSE,
    1,   0,     1,     "DRUG",  NA,  100,  1,    100,
    1,   1,     0,     "DRUG",  10,  0,    2,    100,
    1,   5,     0,     "DRUG",  20,  0,    2,    100,
    1,   10,    0,     "DRUG",  15,  0,    2,    100,
    1,   24,    0,     "DRUG",  5,   0,    2,    100,
    2,   0,     1,     "DRUG",  NA,  50,   1,    50,
    2,   1,     0,     "DRUG",  8,   0,    2,    50,
    2,   4,     0,     "DRUG",  12,  0,    2,    50,
    2,   12,    0,     "DRUG",  6,   0,    2,    50,
    2,   48,    0,     "DRUG",  1,   0,    2,    50,
    3,   0,     1,     "DRUG",  NA,  50,   1,    50,
    3,   2,     0,     "DRUG",  7,   0,    2,    50,
    3,   6,     0,     "DRUG",  11,  0,    2,    50,
    3,   24,    0,     "DRUG",  3,   0,    2,    50,
    3,   48,    0,     "DRUG",  0.5, 0,    2,    50
  ) %>%
    nif()

  result <- suppressWarnings(add_bintime1(test_data, time = "TIME"))

  # Same TIME maps to the same bin edges and label regardless of DOSE
  by_time <- result %>%
    filter(EVID == 0) %>%
    group_by(TIME) %>%
    summarise(
      n_left  = n_distinct(BIN_LEFT),
      n_right = n_distinct(BIN_RIGHT),
      n_label = n_distinct(BINTIME),
      .groups = "drop"
    )

  expect_true(all(by_time$n_left == 1))
  expect_true(all(by_time$n_right == 1))
  expect_true(all(by_time$n_label == 1))
})


test_that("add_bintime1 n sets the number of bin intervals", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   2,     0,     "DRUG",  20,  0,    2,
    1,   4,     0,     "DRUG",  30,  0,    2,
    1,   8,     0,     "DRUG",  25,  0,    2,
    1,   12,    0,     "DRUG",  18,  0,    2,
    1,   24,    0,     "DRUG",  12,  0,    2,
    1,   48,    0,     "DRUG",  5,   0,    2,
    2,   0,     1,     "DRUG",  NA,  100,  1,
    2,   1.5,   0,     "DRUG",  12,  0,    2,
    2,   3,     0,     "DRUG",  22,  0,    2,
    2,   6,     0,     "DRUG",  28,  0,    2,
    2,   10,    0,     "DRUG",  20,  0,    2,
    2,   18,    0,     "DRUG",  14,  0,    2,
    2,   36,    0,     "DRUG",  8,   0,    2,
    2,   48,    0,     "DRUG",  3,   0,    2
  ) %>%
    nif()

  result <- add_bintime1(test_data, time = "TIME", n = 3)

  n_bins <- result %>%
    filter(!is.na(BIN_LEFT)) %>%
    distinct(BIN_LEFT, BIN_RIGHT) %>%
    nrow()

  expect_equal(n_bins, 3)
})


test_that("add_bintime1 rejects invalid n", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2
  ) %>%
    nif()

  expect_error(add_bintime1(test_data, time = "TIME", n = 0))
  expect_error(add_bintime1(test_data, time = "TIME", n = 1.5))
  expect_error(add_bintime1(test_data, time = "TIME", n = -1))
})


test_that("add_bintime1 BIN_LEFT < BIN_RIGHT and times fall in bins", {
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

  result <- add_bintime1(test_data, time = "TIME")
  obs <- result %>% filter(!is.na(BIN_LEFT))

  expect_true(all(obs$BIN_LEFT < obs$BIN_RIGHT))
  expect_true(all(obs$TIME >= obs$BIN_LEFT))
  expect_true(all(obs$TIME <= obs$BIN_RIGHT))
})


# ---- failing tests ----

test_that("add_bintime1 handles all identical times", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   5,     1,     "DRUG",  NA,  100,  1,
    1,   5,     0,     "DRUG",  10,  0,    2,
    1,   5,     0,     "DRUG",  20,  0,    2,
    2,   5,     1,     "DRUG",  NA,  100,  1,
    2,   5,     0,     "DRUG",  12,  0,    2
  ) %>%
    nif()

  result <- tryCatch(
      add_bintime1(test_data, time = "TIME", silent = TRUE),
    error = identity
  )

  expect_false(inherits(result, "error"), info = conditionMessage(result))
  skip_if(inherits(result, "error"))

  expect_s3_class(result, "nif")
  expect_true(all(c("BINTIME", "BIN_LEFT", "BIN_RIGHT") %in% names(result)))
  expect_equal(nrow(result), nrow(test_data))

  obs <- result %>% filter(!is.na(BIN_LEFT))
  expect_equal(n_distinct(obs$BIN_LEFT), 1)
  expect_equal(n_distinct(obs$BIN_RIGHT), 1)
  expect_true(all(obs$TIME >= obs$BIN_LEFT))
  expect_true(all(obs$TIME <= obs$BIN_RIGHT))
})


test_that("add_bintime1 handles identical times with NAs", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~TAFD,
    1,   0,     1,     "DRUG",  NA,  100,  1,    NA,
    1,   5,     0,     "DRUG",  10,  0,    2,    5,
    1,   5,     0,     "DRUG",  20,  0,    2,    5,
    2,   0,     1,     "DRUG",  NA,  100,  1,    NA,
    2,   5,     0,     "DRUG",  12,  0,    2,    5
  ) %>%
    nif()

  result <- tryCatch(
    add_bintime1(test_data, time = "TAFD", silent = TRUE),
    error = identity
  )

  expect_false(inherits(result, "error"), info = conditionMessage(result))
  skip_if(inherits(result, "error"))

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), nrow(test_data))

  # rows with the single finite time get a bin; NA times stay NA
  finite <- result %>% filter(!is.na(TAFD))
  expect_true(all(!is.na(finite$BIN_LEFT)))
  expect_true(all(!is.na(finite$BIN_RIGHT)))
  expect_true(all(!is.na(finite$BINTIME)))

  missing <- result %>% filter(is.na(TAFD))
  expect_true(all(is.na(missing$BIN_LEFT)))
  expect_true(all(is.na(missing$BIN_RIGHT)))
  expect_true(all(is.na(missing$BINTIME)))
})


test_that("add_bintime1 handles constant NTIME (single nominal time)", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT, ~NTIME,
    1,   0,     1,     "DRUG",  NA,  100,  1,    12,
    1,   11.8,  0,     "DRUG",  10,  0,    2,    12,
    2,   0,     1,     "DRUG",  NA,  100,  1,    12,
    2,   12.1,  0,     "DRUG",  12,  0,    2,    12
  ) %>%
    nif()

  result <- tryCatch(
    add_bintime1(test_data, time = "NTIME", silent = TRUE),
    error = identity
  )
  expect_false(inherits(result, "error"), info = conditionMessage(result))
  skip_if(inherits(result, "error"))

  expect_s3_class(result, "nif")
  expect_equal(n_distinct(result$BIN_LEFT[!is.na(result$BIN_LEFT)]), 1)
})


test_that("add_bintime1 handles single-row nif with one time", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   8,     0,     "DRUG",  10,  0,    2
  ) %>%
    nif()

  result <- tryCatch(
    add_bintime1(test_data, time = "TIME", silent = TRUE),
    error = identity
  )
  expect_false(inherits(result, "error"), info = conditionMessage(result))
  skip_if(inherits(result, "error"))

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 1)
  expect_false(is.na(result$BIN_LEFT))
  expect_false(is.na(result$BIN_RIGHT))
  expect_false(is.na(result$BINTIME))
  expect_true(result$TIME >= result$BIN_LEFT)
  expect_true(result$TIME <= result$BIN_RIGHT)
})


test_that("add_bintime1 handles identical times with explicit n", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   24,    1,     "DRUG",  NA,  100,  1,
    1,   24,    0,     "DRUG",  10,  0,    2,
    1,   24,    0,     "DRUG",  20,  0,    2,
    2,   24,    0,     "DRUG",  15,  0,    2
  ) %>%
    nif()

  result <- tryCatch(
    add_bintime1(test_data, time = "TIME", n = 3, silent = TRUE),
    error = identity
  )
  expect_false(inherits(result, "error"), info = conditionMessage(result))
  skip_if(inherits(result, "error"))

  expect_s3_class(result, "nif")
  expect_equal(n_distinct(result$BIN_LEFT[!is.na(result$BIN_LEFT)]), 1)
})

