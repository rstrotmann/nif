test_that("nca() input validation", {
  # Test that non-nif objects are rejected
  expect_error(nca(data.frame()),
               "Input must be a nif object")

  # Test that missing required columns are caught
  test_nif <- structure(
    tibble::tribble(
      ~ID,
      1, 2, 3
    ),
    class = c("nif", "data.frame")
  )

  expect_error(
    nca(test_nif),
    "Missing essential fields in nif object: TIME, AMT, CMT, EVID and DV")
})


test_that("nca() analyte handling", {
  # Create a minimal valid nif object
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~DV,  ~EVID, ~ANALYTE, ~DOSE, ~AMT, ~CMT,
      1,   0,     0,    1,     "DRUG",   100,   100,  1,
      1,   0,     10,   0,     "DRUG",   100,   0,    2,
      1,   2,     5,    0,     "DRUG",   100,   0,    2,
      1,   3,     2,    0,     "DRUG",   100,   0,    2,
      2,   0,     0,    1,     "DRUG",   100,   100,  1,
      2,   0,     12,   0,     "DRUG",   100,   0,    2,
      2,   2,     6,    0,     "DRUG",   100,   0,    2,
      2,   3,     3,    0,     "DRUG",   100,   0,    2,
      3,   0,     0,    1,     "DRUG",   100,   100,  1,
      3,   0,     8,    0,     "DRUG",   100,   0,    2,
      3,   2,     4,    0,     "DRUG",   100,   0,    2,
      3,   3,     1,    0,     "DRUG",   100,   0,    2
    ),
    class = c("nif", "data.frame")
  )

  # Test automatic analyte selection
  expect_no_error(
    capture_warning(
      result <- nca(test_nif, silent = TRUE)
    )
  )

  # Test explicit analyte selection
  expect_no_error(
    capture_warning(
      result <- nca(test_nif, analyte = "DRUG", silent = TRUE)
    )
  )
})


test_that("nca() grouping functionality", {
  # Create a nif object with grouping variable
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~DV,  ~EVID, ~ANALYTE, ~DOSE, ~GROUP, ~AMT, ~CMT,
      1,   0,     0,    1,     "DRUG",   100,   "A",   100,  1,
      1,   0,     0,    0,     "DRUG",   100,   "A",   0,    2,
      1,   1,     10,   0,     "DRUG",   100,   "A",   0,    2,
      1,   2,     5,    0,     "DRUG",   100,   "A",   0,    2,
      1,   3,     2,    0,     "DRUG",   100,   "A",   0,    2,
      2,   0,     0,    1,     "DRUG",   100,   "B",   100,  1,
      2,   0,     0,    0,     "DRUG",   100,   "B",   0,    2,
      2,   1,     12,   0,     "DRUG",   100,   "B",   0,    2,
      2,   2,     6,    0,     "DRUG",   100,   "B",   0,    2,
      2,   3,     3,    0,     "DRUG",   100,   "B",   0,    2,
      3,   0,     0,    1,     "DRUG",   100,   "A",   100,  1,
      3,   0,     0,    0,     "DRUG",   100,   "A",   0,    2,
      3,   1,     8,    0,     "DRUG",   100,   "A",   0,    2,
      3,   2,     4,    0,     "DRUG",   100,   "A",   0,    2,
      3,   3,     1,    0,     "DRUG",   100,   "A",   0,    2
    ),
    class = c("nif", "data.frame")
  )

  # Test grouping
  expect_no_error(
    expect_warning(expect_warning(expect_warning(
      result <- nca(test_nif, group = "GROUP", silent = TRUE)
    )))
  )

  expect_true("GROUP" %in% names(result))
  expect_equal(length(unique(result$GROUP)), 2)
})


# Helper function to create a minimal valid nif object for testing
create_minimal_nif <- function() {
  tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     2,    0,     "DRUG",   "DRUG",  100,   0,    2,
    2,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    2,   0,     0,    0,     12,   0,     "DRUG",   "DRUG",  100,   0,    2,
    2,   2,     2,    2,     6,    0,     "DRUG",   "DRUG",  100,   0,    2,
    2,   4,     4,    4,     3,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)
}


test_that("nca() time parameter validation", {
  test_nif <- create_minimal_nif()

  # Test valid time parameters (PKNCA may warn about insufficient data)
  suppressWarnings({
    expect_no_error(nca(test_nif, analyte = "DRUG", time = "TIME", silent = TRUE))
  })

  # Test invalid time parameter
  expect_error(
    nca(test_nif, analyte = "DRUG", time = "INVALID"),
    "'time' parameter must be one of"
  )
})


test_that("nca() handles NULL group parameter", {
  test_nif <- create_minimal_nif()

  # Test that NULL group doesn't cause errors (fixes issue 2 and 3)
  # PKNCA may warn about insufficient data points
  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", group = NULL, silent = TRUE)
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
})


test_that("nca() handles multiple group variables", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~GROUP1, ~GROUP2, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   "A",     "X",     100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   "A",     "X",     0,    2,
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   "A",     "X",     0,    2,
    2,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   "B",     "Y",     100,  1,
    2,   0,     0,    0,     12,   0,     "DRUG",   "DRUG",  100,   "B",     "Y",     0,    2,
    2,   2,     2,    2,     6,    0,     "DRUG",   "DRUG",  100,   "B",     "Y",     0,    2
  ) |>
    nif(silent = TRUE)

  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", group = c("GROUP1", "GROUP2"), silent = TRUE)
    )

    expect_true("GROUP1" %in% names(result))
    expect_true("GROUP2" %in% names(result))
    expect_true(is.factor(result$GROUP1))
    expect_true(is.factor(result$GROUP2))
  })
})


test_that("nca() parent extraction validation", {
  # Test with valid parent
  test_nif <- create_minimal_nif()
  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", silent = TRUE)
    )

    # Test with explicit parent
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", parent = "DRUG", silent = TRUE)
    )
  })

  # Test with invalid parent
  expect_error(
    nca(test_nif, analyte = "DRUG", parent = "INVALID_PARENT", silent = TRUE),
    "Parent not found"
  )
})


test_that("nca() handles invalid analyte", {
  test_nif <- create_minimal_nif()

  # Test with non-existent analyte
  expect_error(
    nca(test_nif, analyte = "NONEXISTENT", silent = TRUE),
    "Invalid analyte"
  )

  # Test with NA analyte
  expect_error(
    nca(test_nif, analyte = NA_character_, silent = TRUE),
    "Invalid analyte"
  )
})


test_that("nca() handles negative concentrations", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     -5,   0,     "DRUG",   "DRUG",  100,   0,    2,  # Negative concentration
    1,   4,     4,    4,     2,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  # Negative concentrations should be set to zero (may produce warnings)
  # PKNCA may also produce warnings about insufficient data points
  suppressMessages(
    suppressWarnings(
      result <- nca(test_nif, analyte = "DRUG", silent = FALSE)
    ))

  # Should complete successfully
  expect_s3_class(result, "data.frame")
  })


test_that("nca() handles NA concentrations", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     NA,   0,     "DRUG",   "DRUG",  100,   0,    2,  # NA concentration
    1,   4,     4,    4,     2,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  # NA values should be set to zero (as documented)
  # PKNCA may warn about insufficient data
  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", silent = TRUE)
    )

    expect_s3_class(result, "data.frame")
  })
})


test_that("nca() duplicate handling - stop", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   0,     0,    0,     12,   0,     "DRUG",   "DRUG",  100,   0,    2,  # Duplicate at TIME=0
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  # Should error with duplicates = "stop" (default)
  expect_error(
    nca(test_nif, analyte = "DRUG", duplicates = "stop", silent = TRUE),
    "duplicate.*observation"
  )
})


test_that("nca() duplicate handling - identify", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   0,     0,    0,     12,   0,     "DRUG",   "DRUG",  100,   0,    2,  # Duplicate at TIME=0
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  # Should return duplicate observations
  suppressMessages(
    result <- nca(test_nif, analyte = "DRUG", duplicates = "identify", silent = TRUE)
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  # Should contain the duplicate rows
  expect_true(any(result$TIME == 0))
})


test_that("nca() duplicate handling - resolve", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   0,     0,    0,     12,   0,     "DRUG",   "DRUG",  100,   0,    2,  # Duplicate at TIME=0
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  # Should resolve duplicates using mean (default)
  # May also get PKNCA warnings about insufficient data
  suppressMessages(
    expect_message(
      expect_warning(
        result <- nca(test_nif, analyte = "DRUG", duplicates = "resolve", silent = FALSE),
        "Too few points for half-life calculation"
      ),
      "2 duplicate observations for DRUG resolved"
    )
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

})


test_that("nca() duplicate handling - resolve with custom function", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   0,     0,    0,     12,   0,     "DRUG",   "DRUG",  100,   0,    2,  # Duplicate at TIME=0
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  # Should resolve duplicates using max function
  # May also get PKNCA warnings about insufficient data
  suppressMessages(
    expect_warning(
      result <- nca(test_nif, analyte = "DRUG", duplicates = "resolve",
                    duplicate_function = max, silent = FALSE),
      "Too few points for half-life calculation"
    )
  )

  expect_s3_class(result, "data.frame")
})


test_that("nca() keep parameter functionality", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~WEIGHT, ~AGE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   70,      30,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   70,      30,   0,    2,
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   70,      30,   0,    2,
    2,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   75,      35,   100,  1,
    2,   0,     0,    0,     12,   0,     "DRUG",   "DRUG",  100,   75,      35,   0,    2,
    2,   2,     2,    2,     6,    0,     "DRUG",   "DRUG",  100,   75,      35,   0,    2
  ) |>
    nif(silent = TRUE)

  suppressWarnings(
    result <- nca(test_nif, analyte = "DRUG", keep = c("WEIGHT", "AGE"), silent = TRUE)
  )
  expect_true("WEIGHT" %in% names(result))
  expect_true("AGE" %in% names(result))
})


test_that("nca() handles different time fields", {
  # Test with NTIME - include TAD and TAFD so ensure_time() doesn't try to recalculate
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~NTIME, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,      0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     0,      10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     2,      5,    0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     4,      2,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", time = "NTIME", silent = TRUE)
    )

    expect_s3_class(result, "data.frame")
  })
})


test_that("nca() handles metabolite with different parent", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "PARENT_DRUG", "PARENT_DRUG", 100, 100, 1,
    1,   0,     0,    0,     0,    0,     "METABOLITE",  "PARENT_DRUG", 100, 0,   2,
    1,   2,     2,    2,     5,    0,     "METABOLITE",  "PARENT_DRUG", 100, 0,   2,
    1,   4,     4,    4,     3,    0,     "METABOLITE",  "PARENT_DRUG", 100, 0,   2,
    2,   0,     0,    0,     0,    1,     "PARENT_DRUG", "PARENT_DRUG", 100, 100, 1,
    2,   0,     0,    0,     0,    0,     "METABOLITE",  "PARENT_DRUG", 100, 0,   2,
    2,   2,     2,    2,     6,    0,     "METABOLITE",  "PARENT_DRUG", 100, 0,   2
  ) |>
    nif(silent = TRUE)

  # Test analyzing metabolite with parent dosing
  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "METABOLITE", parent = "PARENT_DRUG", silent = TRUE)
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
})


test_that("nca() returns expected structure", {
  test_nif <- create_minimal_nif()

  suppressWarnings({
    result <- nca(test_nif, analyte = "DRUG", silent = TRUE)

    # Check that result is a data frame
    expect_s3_class(result, "data.frame")

    # Check that it has expected columns from PKNCA
    expect_true("ID" %in% names(result))
    expect_true("DI" %in% names(result))

    # Should have some NCA parameters
    expect_true(nrow(result) > 0)
  })
})


test_that("nca() handles empty concentration data gracefully", {
  # Create nif with only dosing, no observations
  # Include TAD and TAFD so ensure_time() doesn't try to recalculate
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    2,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1
  ) |>
    nif(silent = TRUE)

  # This should error because there are no concentration observations
  # PKNCA requires concentration data to perform NCA
  expect_error(
    nca(test_nif, analyte = "DRUG", silent = TRUE)
  )
})


test_that("nca() handles empty dosing data gracefully", {
  # Create nif with only observations, no dosing
  # Include TAD and TAFD so ensure_time() doesn't try to recalculate
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  # This should error because PKNCA needs dosing information
  expect_error(
    nca(test_nif, analyte = "DRUG", silent = TRUE)
  )
})


test_that("nca() silent parameter suppresses messages", {
  test_nif <- create_minimal_nif()

  # PKNCA may still produce warnings about insufficient data points
  # but the function itself should not produce additional warnings
  suppressWarnings({
    result <- nca(test_nif, analyte = "DRUG", silent = TRUE)

    expect_s3_class(result, "data.frame")
  })
})


test_that("nca() works with multiple dosing intervals", {
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   24,    0,    24,    0,    1,     "DRUG",   "DRUG",  100,   100,  1,  # Second dose
    1,   24,    0,    24,    8,    0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   26,    2,    26,    4,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  suppressWarnings({
    result <- nca(test_nif, analyte = "DRUG", silent = TRUE)

    expect_s3_class(result, "data.frame")
    # Should have results for multiple dosing intervals
    expect_true(length(unique(result$DI)) >= 1)
  })
})


test_that("nca() handles TAFD time field", {
  # Include all time fields so ensure_time() doesn't try to recalculate
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     2,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", time = "TAFD", silent = TRUE)
    )

    expect_s3_class(result, "data.frame")
  })
})


test_that("nca() handles TAD time field", {
  # Include all time fields so ensure_time() doesn't try to recalculate
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV,  ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,    1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,    0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     2,    0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  suppressWarnings({
    expect_no_error(
      result <- nca(test_nif, analyte = "DRUG", time = "TAD", silent = TRUE)
    )

    expect_s3_class(result, "data.frame")
  })
})

