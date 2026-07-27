test_that("derive_baseline works with valid input", {
  # Create a simple test dataset
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~TAFD, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     -1,   0,    2,
    1,   0,     12,   "A",      0,     0,    0,    2,
    1,   1,     15,   "A",      0,     1,    0,    2,
    2,   -1,    20,   "A",      0,     -1,   0,    2,
    2,   0,     22,   "A",      0,     0,    0,    2,
    2,   1,     25,   "A",      0,     1,    0,    2
  )

  test_nif <- nif(test_data)

  # Run derive_baseline
  result <- derive_baseline(test_nif)

  # Check if DVBL column is created
  expect_true("DVBL" %in% names(result))

  # Check baseline values (should be median of pre-dose values)
  expect_equal(result$DVBL[result$ID == 1 & result$ANALYTE == "A"][1], 11) # ID 1 baseline (median of 10 and 12)
  expect_equal(result$DVBL[result$ID == 2 & result$ANALYTE == "A"][1], 21) # ID 2 baseline (median of 20 and 22)
})


test_that("derive_baseline handles empty baseline sets correctly", {
  # Create test data where baseline filter matches no rows
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~TAFD, ~AMT, ~CMT,
    1,   1,     10,   "A",      0,     1,    0,    2,
    1,   2,     12,   "A",      0,     2,    0,    2,
    1,   3,     15,   "A",      0,     3,    0,    2,
    2,   1,     20,   "A",      0,     1,    0,    2,
    2,   2,     22,   "A",      0,     2,    0,    2,
    2,   3,     25,   "A",      0,     3,    0,    2
  )

  test_nif <- nif(test_data)

  # Test with filter that matches no rows (all times are positive)
  result <- derive_baseline(test_nif, baseline_filter = "TAFD < 0")

  # Check that DVBL uses default_baseline (NA_real_) for all rows
  expect_true(all(is.na(result$DVBL)))
})


test_that("derive_baseline handles empty baseline sets with custom default_baseline", {
  # Create test data where baseline filter matches no rows
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   1,     10,   "A",      0,     0,    2,
    1,   2,     12,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches no rows and custom default
  result <- derive_baseline(
    test_nif,
    baseline_filter = "TAFD < 0",
    default_baseline = 0
  )

  # Check that DVBL uses custom default_baseline (0) for all rows
  expect_equal(result$DVBL, c(0, 0))
})


test_that("derive_baseline handles all NA values in filtered baseline", {
  # Create test data where baseline filter matches rows but all DV values are NA
  test_data <- tibble::tribble(
     ~ID, ~TIME, ~DV, ~ANALYTE, ~EVID, ~AMT, ~CMT,
       1,    -1,  NA,      "A",     0,    0,    2,
       1,     0,  NA,      "A",     0,    0,    2,
       1,     1,  15,      "A",     0,    0,    2,
       2,    -1,  NA,      "A",     0,    0,    2,
       2,     0,  NA,      "A",     0,    0,    2,
       2,     1,  25,      "A",     0,    0,    2
     ) |> mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches rows but all values are NA
  result <- derive_baseline(test_nif, baseline_filter = "TAFD <= 0", silent = F)

  # Check that DVBL uses default_baseline (NA_real_) for all rows
  expect_true(all(is.na(result$DVBL)))
})


test_that("derive_baseline handles empty vector from summary function", {
  # Create test data where baseline filter matches rows but after na.omit, vector is empty
  test_data <- tibble::tribble(
     ~ID, ~TIME, ~DV, ~ANALYTE, ~EVID, ~AMT, ~CMT,
       1,    -1,  NA,      "A",     0,    0,    2,
       1,     0,  NA,      "A",     0,    0,    2,
       1,     1,  15,      "A",     0,    0,    2
     ) |> mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches rows but all non-NA values are filtered out
  result <- derive_baseline(
    test_nif,
    baseline_filter = "TAFD <= 0",
    default_baseline = 999,
    silent = FALSE
  )

  # Check that DVBL uses default_baseline (999) when vector is empty
  expect_equal(result$DVBL[result$TAFD <= 0], c(999, 999))
  expect_equal(result$DVBL[result$TAFD == 1], 999)
})


test_that("derive_baseline works with different summary functions", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with mean
  result_mean <- derive_baseline(test_nif, summary_function = mean)
  expect_equal(result_mean$DVBL[result_mean$TAFD <= 0][1], 11) # Mean of 10 and 12

  # Test with min
  result_min <- derive_baseline(test_nif, summary_function = min)
  expect_equal(result_min$DVBL[result_min$TAFD <= 0][1], 10) # Min of 10 and 12

  # Test with max
  result_max <- derive_baseline(test_nif, summary_function = max)
  expect_equal(result_max$DVBL[result_max$TAFD <= 0][1], 12) # Max of 10 and 12
})


test_that("derive_baseline handles multiple analytes", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    1,   -1,    20,   "B",      0,     0,    3,
    1,   0,     22,   "B",      0,     0,    3,
    1,   1,     25,   "B",      0,     0,    3
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  result <- derive_baseline(test_nif)

  # Check baseline values for each analyte
  expect_equal(
    result$DVBL[result$ID == 1 & result$ANALYTE == "A" & result$TAFD <= 0][1],
    11
  )
  expect_equal(
    result$DVBL[result$ID == 1 & result$ANALYTE == "B" & result$TAFD <= 0][1],
    21
  )
})


test_that("derive_baseline handles specific analyte selection", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    1,   -1,    20,   "B",      0,     0,    3,
    1,   0,     22,   "B",      0,     0,    3,
    1,   1,     25,   "B",      0,     0,    3
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with specific analyte
  result <- derive_baseline(test_nif, analyte = "A")

  # Check that only analyte A has baseline calculated
  expect_false(any(is.na(result$DVBL[result$ANALYTE == "A" & result$TAFD <= 0])))
  # Analyte B should not have baseline (or should be NA)
  expect_true(all(is.na(result$DVBL[result$ANALYTE == "B"])))
})


test_that("derive_baseline handles custom baseline filter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    1,   2,     18,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with custom baseline filter
  result <- derive_baseline(test_nif, baseline_filter = "TAFD == 1")
  expect_equal(result$DVBL[result$TAFD == 1][1], 15) # Median of value at TAFD == 1
})


test_that("derive_baseline handles EVID filtering correctly", {
  # Create test data with both EVID 0 and 1
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    8,    "A",      0,     0,    2,
    1,   0,     10,   "A",      1,     100,  1,   # Dosing event
    1,   1,     12,   "A",      0,     0,    2,
    2,   -1,    18,   "A",      0,     0,    2,
    2,   0,     20,   "A",      1,     100,  1,   # Dosing event
    2,   1,     22,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Baseline should only use EVID == 0 rows
  result <- derive_baseline(test_nif, baseline_filter = "TAFD <= 0")

  # Check baseline values (should only use EVID == 0, so only TAFD == -1)
  expect_equal(result$DVBL[result$ID == 1 & result$TAFD <= 0 & result$EVID == 0][1], 8)
  expect_equal(result$DVBL[result$ID == 2 & result$TAFD <= 0 & result$EVID == 0][1], 18)
})


test_that("derive_baseline handles multiple baseline values, analytes, and subjects", {
  test_data <- tibble::tribble(
     ~ID, ~TIME, ~DV, ~ANALYTE, ~EVID, ~AMT, ~CMT,
       1,    -2,   8,      "A",     0,    0,    2,
       1,    -1,  10,      "A",     0,    0,    2,
       1,     0,  12,      "A",     0,    0,    2,
       1,     1,  15,      "A",     0,    0,    2,
       1,    -2,  18,      "B",     0,    0,    3,
       1,    -1,  20,      "B",     0,    0,    3,
       1,     0,  22,      "B",     0,    0,    3,
       1,     1,  25,      "B",     0,    0,    3,
       2,    -2,  28,      "A",     0,    0,    2,
       2,    -1,  30,      "A",     0,    0,    2,
       2,     0,  32,      "A",     0,    0,    2,
       2,     1,  35,      "A",     0,    0,    2,
       2,    -2,  38,      "B",     0,    0,    3,
       2,    -1,  40,      "B",     0,    0,    3,
       2,     0,  42,      "B",     0,    0,    3,
       2,     1,  45,      "B",     0,    0,    3
     ) |>
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  result <- derive_baseline(test_nif)

  expected <- tibble::tribble(
    ~ID, ~ANALYTE, ~DVBL,
    1,   "A",      10,   # median(8, 10, 12)
    1,   "B",      20,   # median(18, 20, 22)
    2,   "A",      30,   # median(28, 30, 32)
    2,   "B",      40    # median(38, 40, 42)
  )

  actual <- result |>
    as_tibble() |>
    distinct(ID, ANALYTE, DVBL) |>
    arrange(ID, ANALYTE)

  expect_equal(actual, expected)

  # DVBL constant within ID/ANALYTE and present on post-baseline rows
  expect_equal(
    result$DVBL[result$ID == 1 & result$ANALYTE == "A" & result$TIME == 1],
    10
  )
})


test_that("derive_baseline handles mixed valid and invalid row data", {
  # nif() rejects NA IDs, so start from valid data and inject invalid rows
  test_data <- tibble::tribble(
     ~ID, ~TIME, ~DV, ~ANALYTE, ~EVID, ~AMT, ~CMT,
       1,    -1,  10,      "A",     0,    0,    2,
       1,     0,  NA,      "A",     0,    0,    2,
       1,     1,  15,      "A",     0,    0,    2,
       2,    -1,  NA,      "A",     0,    0,    2,
       2,     0,  NA,      "A",     0,    0,    2,
       2,     1,  25,      "A",     0,    0,    2,
       4,    -1,  20,      "A",     0,    0,    2,
       4,     0,  22,      "A",     0,    0,    2,
       4,     1,  28,      "A",     0,    0,    2,
       4,     1,  50,      "B",     0,    0,    3,
       4,     2,  55,      "B",     0,    0,    3
     ) |>
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  invalid_rows <- tibble::tribble(
     ~ID, ~TIME, ~DV, ~ANALYTE, ~EVID, ~AMT, ~CMT, ~TAFD,
      NA,     0,  30,      "A",     0,    0,    2,     0,
       3,     0,  35,       NA,     0,    0,    2,     0
     )

  test_nif <- bind_rows(test_nif, invalid_rows)
  class(test_nif) <- c("nif", "data.frame")

  # Pass analyte explicitly so NA ANALYTE values do not enter validate_analyte
  expect_message(
    result <- derive_baseline(
      test_nif,
      analyte = c("A", "B"),
      silent = FALSE
    ),
    "Found NA values in ID column"
  )

  # ID 1: NA at TAFD == 0 omitted; baseline is median of remaining value (10)
  expect_equal(result$DVBL[result$ID == 1 & result$ANALYTE == "A"][1], 10)
  # ID 2: all baseline DV are NA → default baseline
  expect_true(all(is.na(result$DVBL[result$ID == 2 & result$ANALYTE == "A"])))
  # Rows with NA ID or NA ANALYTE get no calculated baseline
  expect_true(all(is.na(result$DVBL[is.na(result$ID) | is.na(result$ANALYTE)])))
  # ID 4 analyte A: median of 20 and 22
  expect_equal(result$DVBL[result$ID == 4 & result$ANALYTE == "A"][1], 21)
  # ID 4 analyte B: no rows with TAFD <= 0 → default baseline
  expect_true(all(is.na(result$DVBL[result$ID == 4 & result$ANALYTE == "B"])))
})


test_that("derive_baseline errors on invalid input", {
  base_data <- tibble::tribble(
     ~ID, ~TIME, ~DV, ~ANALYTE, ~EVID, ~AMT, ~CMT,
       1,    -1,  10,      "A",     0,    0,    2,
       1,     0,  12,      "A",     0,    0,    2
     ) |>
    mutate(TAFD = TIME)

  test_nif <- nif(base_data)

  test_nif_no_dv <- select(test_nif, -DV)
  expect_error(derive_baseline(test_nif_no_dv), "Missing required columns: DV")

  test_nif_char_dv <- mutate(test_nif, DV = as.character(DV))
  expect_error(derive_baseline(test_nif_char_dv), "DV column must contain numeric values")

  expect_error(derive_baseline(test_nif, analyte = "NONEXISTENT"), "not found in nif object")

  expect_error(derive_baseline(test_nif, baseline_filter = "system('ls')"), "Disallowed construct")

  expect_error(derive_baseline(test_nif, summary_function = "median"), "summary_function must be a function")

  expect_error(derive_baseline(test_nif, default_baseline = "NA"), "default_baseline must be a numeric value")
})

