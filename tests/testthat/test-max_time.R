test_that("max_time returns max time for observations only by default", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0,     1,     NA,  1,    # dosing event
    1,   1,     0,     10,  2,    # observation
    1,   5,     0,     20,  2,    # observation
    1,   10,    0,     15,  2,    # observation
    1,   12,    1,     NA,  1     # dosing event
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 10)
})

test_that("max_time ignores dosing events when only_observations = TRUE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0,     1,     NA,  1,    # dosing event
    1,   1,     0,     10,  2,    # observation
    1,   50,    1,     NA,  1,    # dosing event (should be ignored)
    1,   5,     0,     20,  2     # observation
  ) %>%
    new_nif()

  expect_equal(max_time(nif, only_observations = TRUE), 5)
})

test_that("max_time includes all events when only_observations = FALSE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0,     1,     NA,  1,    # dosing event
    1,   1,     0,     10,  2,    # observation
    1,   50,    1,     NA,  1,    # dosing event (should be included)
    1,   5,     0,     20,  2     # observation
  ) %>%
    new_nif()

  expect_equal(max_time(nif, only_observations = FALSE), 50)
})

test_that("max_time filters out observations with NA DV when only_observations = TRUE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,    # observation with DV
    1,   5,     0,     NA,  2,    # observation with NA DV (should be filtered)
    1,   10,    0,     15,  2     # observation with DV
  ) %>%
    new_nif()

  expect_equal(max_time(nif, only_observations = TRUE), 10)
})

test_that("max_time includes observations with NA DV when only_observations = FALSE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,    # observation with DV
    1,   5,     0,     NA,  2,    # observation with NA DV (should be included)
    1,   10,    0,     15,  2     # observation with DV
  ) %>%
    new_nif()

  expect_equal(max_time(nif, only_observations = FALSE), 10)
})

test_that("max_time filters by analyte when specified", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     10,  2,    "CMT2",
    1,   5,     0,     20,  2,    "CMT2",
    1,   10,    0,     15,  3,    "CMT3",
    1,   20,    0,     25,  3,    "CMT3"
  ) %>%
    new_nif()

  expect_equal(max_time(nif, analyte = "CMT2"), 5)
  expect_equal(max_time(nif, analyte = "CMT3"), 20)
})

test_that("max_time returns max across all analytes when analyte is NULL", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     10,  2,    "CMT2",
    1,   5,     0,     20,  2,    "CMT2",
    1,   10,    0,     15,  3,    "CMT3",
    1,   20,    0,     25,  3,    "CMT3"
  ) %>%
    new_nif()

  expect_equal(max_time(nif, analyte = NULL), 20)
  expect_equal(max_time(nif), 20)  # NULL is default
})

test_that("max_time works with multiple analytes specified", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     10,  2,    "CMT2",
    1,   5,     0,     20,  2,    "CMT2",
    1,   10,    0,     15,  3,    "CMT3",
    1,   20,    0,     25,  3,    "CMT3",
    1,   30,    0,     30,  4,    "CMT4"
  ) %>%
    new_nif()

  # When multiple analytes specified, should return max across those analytes
  result <- max_time(nif, analyte = c("CMT2", "CMT3"))
  expect_equal(result, 20)
})

test_that("max_time returns NA when no observations exist with only_observations = TRUE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0,     1,     NA,  1,    # only dosing events
    1,   24,    1,     NA,  1,
    1,   48,    1,     NA,  1
  ) %>%
    new_nif()

  expect_equal(max_time(nif, only_observations = TRUE), NA)
})

test_that("max_time returns max time when no observations exist but only_observations = FALSE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0,     1,     NA,  1,    # only dosing events
    1,   24,    1,     NA,  1,
    1,   48,    1,     NA,  1
  ) %>%
    new_nif()

  expect_equal(max_time(nif, only_observations = FALSE), 48)
})

test_that("max_time returns NA when analyte has no observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     10,  2,    "CMT2",
    1,   5,     0,     20,  2,    "CMT2"
  ) %>%
    new_nif()

  expect_equal(max_time(nif, analyte = "CMT4"), NA)
  expect_equal(max_time(nif, analyte = "NONEXISTENT"), NA)
})

test_that("max_time handles empty nif object", {
  nif <- new_nif()

  expect_equal(max_time(nif), NA)
})

test_that("max_time handles single observation", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   5,     0,     10,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 5)
})

test_that("max_time handles NA values in TIME", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,
    1,   NA,    0,     20,  2,    # NA time should be ignored
    1,   5,     0,     15,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 5)
})

test_that("max_time creates ANALYTE from CMT when missing", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,
    1,   5,     0,     20,  2,
    1,   10,    0,     15,  3
  ) %>%
    new_nif()

  # Should work even without ANALYTE column (ensure_analyte creates it)
  result <- max_time(nif)
  expect_equal(result, 10)
  expect_true("ANALYTE" %in% names(ensure_analyte(nif)))
})

test_that("max_time works with multiple subjects", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,
    1,   5,     0,     20,  2,
    2,   2,     0,     15,  2,
    2,   8,     0,     25,  2,
    3,   3,     0,     12,  2,
    3,   12,    0,     30,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 12)
})

test_that("max_time handles zero time observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0,     0,     0,   2,    # zero time observation
    1,   1,     0,     10,  2,
    1,   5,     0,     20,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 5)
})

test_that("max_time handles negative time observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   -1,    0,     5,   2,    # negative time observation
    1,   0,     0,     0,   2,
    1,   5,     0,     20,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 5)
})

test_that("max_time works with multiple analytes and filters correctly", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     10,  2,    "DRUG",
    1,   5,     0,     20,  2,    "DRUG",
    1,   10,    0,     15,  3,    "METABOLITE",
    1,   20,    0,     25,  3,    "METABOLITE",
    1,   30,     0,     30,  4,    "METABOLITE2"
  ) %>%
    new_nif()

  expect_equal(max_time(nif, analyte = "DRUG"), 5)
  expect_equal(max_time(nif, analyte = "METABOLITE"), 20)
  expect_equal(max_time(nif, analyte = "METABOLITE2"), 30)
  expect_equal(max_time(nif), 30)  # max across all
})

test_that("max_time handles all NA times in observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   NA,    0,     10,  2,
    1,   NA,    0,     20,  2
  ) %>%
    new_nif()

  # When all times are NA, max with na.rm=TRUE returns -Inf, but function should handle it
  result <- max_time(nif)
  expect_true(is.na(result) || is.infinite(result))
})

test_that("max_time works with decimal time values", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0.5,   0,     10,  2,
    1,   1.25,  0,     20,  2,
    1,   2.75,  0,     15,  2,
    1,   10.5,  0,     25,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 10.5)
})

test_that("max_time handles very large time values", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,
    1,   1000,  0,     20,  2,
    1,   500,   0,     15,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 1000)
})

test_that("max_time correctly filters when analyte column exists", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     10,  2,    "ANALYTE1",
    1,   5,     0,     20,  2,    "ANALYTE1",
    1,   10,    0,     15,  2,    "ANALYTE2"
  ) %>%
    new_nif()

  expect_equal(max_time(nif, analyte = "ANALYTE1"), 5)
  expect_equal(max_time(nif, analyte = "ANALYTE2"), 10)
})

test_that("max_time returns correct value when all observations have same time", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   5,     0,     10,  2,
    1,   5,     0,     20,  2,
    1,   5,     0,     15,  2
  ) %>%
    new_nif()

  expect_equal(max_time(nif), 5)
})

test_that("max_time uses custom time_field parameter", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~EVID, ~DV, ~CMT,
    1,   0,     0,    0,     10,  2,
    1,   5,     3,    0,     20,  2,
    1,   10,    8,    0,     15,  2
  ) %>%
    new_nif()

  # Should use TAD field instead of TIME
  expect_equal(max_time(nif, time_field = "TAD"), 8)
  # Default TIME field should still work
  expect_equal(max_time(nif, time_field = "TIME"), 10)
})

test_that("max_time combines analyte filter with only_observations = FALSE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   0,     1,     NA,  1,    "CMT1",    # dosing event
    1,   1,     0,     10,  2,    "CMT2",    # observation
    1,   5,     0,     20,  2,    "CMT2",    # observation
    1,   10,    1,     NA,  1,    "CMT1",    # dosing event
    1,   15,    0,     15,  3,    "CMT3"     # observation
  ) %>%
    new_nif()

  # With only_observations = FALSE, should include dosing events
  expect_equal(max_time(nif, analyte = "CMT1", only_observations = FALSE), 10)
  # With only_observations = TRUE, should exclude dosing events
  expect_equal(max_time(nif, analyte = "CMT1", only_observations = TRUE), NA)
})

test_that("max_time handles observations with missing DV when only_observations = TRUE", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,    # observation with DV
    1,   5,     0,     NA,  2,    # observation with NA DV (filtered out)
    1,   10,    0,     15,  2,    # observation with DV
    1,   15,    0,     NA,  2     # observation with NA DV (filtered out)
  ) %>%
    new_nif()

  expect_equal(max_time(nif, only_observations = TRUE), 10)
})

test_that("max_time handles mixed EVID and DV combinations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   0,     1,     NA,  1,    # dosing event
    1,   1,     0,     10,  2,    # observation with DV
    1,   5,     0,     NA,  2,    # observation with NA DV
    1,   10,    1,     NA,  1,    # dosing event
    1,   15,    0,     20,  2     # observation with DV
  ) %>%
    new_nif()

  # With only_observations = TRUE, should only include EVID==0 & !is.na(DV)
  expect_equal(max_time(nif, only_observations = TRUE), 15)
  # With only_observations = FALSE, should include all events
  expect_equal(max_time(nif, only_observations = FALSE), 15)
})

test_that("max_time works with vector of analytes and only_observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   0,     1,     NA,  1,    "CMT1",    # dosing event
    1,   1,     0,     10,  2,    "CMT2",    # observation
    1,   5,     0,     20,  2,    "CMT2",    # observation
    1,   10,    0,     15,  3,    "CMT3",    # observation
    1,   20,    0,     25,  3,    "CMT3",    # observation
    1,   30,    0,     30,  4,    "CMT4"     # observation
  ) %>%
    new_nif()

  # Multiple analytes with only_observations = TRUE
  result <- max_time(nif, analyte = c("CMT2", "CMT3"), only_observations = TRUE)
  expect_equal(result, 20)
  
  # Multiple analytes with only_observations = FALSE (should include dosing events)
  result2 <- max_time(nif, analyte = c("CMT1", "CMT2"), only_observations = FALSE)
  expect_equal(result2, 5)
})

test_that("max_time handles custom time_field with analyte filter", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   0,     0,    0,     10,  2,    "CMT2",
    1,   5,     3,    0,     20,  2,    "CMT2",
    1,   10,    8,    0,     15,  3,    "CMT3",
    1,   20,    15,   0,     25,  3,    "CMT3"
  ) %>%
    new_nif()

  # Filter by analyte and use custom time field
  expect_equal(max_time(nif, analyte = "CMT2", time_field = "TAD"), 3)
  expect_equal(max_time(nif, analyte = "CMT3", time_field = "TAD"), 15)
  expect_equal(max_time(nif, analyte = "CMT2", time_field = "TIME"), 5)
})

test_that("max_time errors when time_field does not exist", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT,
    1,   1,     0,     10,  2,
    1,   5,     0,     20,  2
  ) %>%
    new_nif()

  # When time_field doesn't exist, pull will error
  expect_error(max_time(nif, time_field = "NONEXISTENT"))
})

test_that("max_time handles edge case with single subject and single analyte", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   100,   0,     50,  2,    "DRUG"
  ) %>%
    new_nif()

  expect_equal(max_time(nif, analyte = "DRUG"), 100)
  expect_equal(max_time(nif, analyte = "DRUG", only_observations = FALSE), 100)
})

test_that("max_time handles case where analyte filter results in no valid observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     10,  2,    "CMT2",
    1,   5,     0,     20,  2,    "CMT2"
  ) %>%
    new_nif()

  # Filter for analyte that doesn't exist
  expect_equal(max_time(nif, analyte = "CMT99"), NA)
  
  # Filter for analyte that exists but has no valid observations (all NA DV)
  nif2 <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE,
    1,   1,     0,     NA,  2,    "CMT2",
    1,   5,     0,     NA,  2,    "CMT2"
  ) %>%
    new_nif()
  
  expect_equal(max_time(nif2, analyte = "CMT2", only_observations = TRUE), NA)
})

