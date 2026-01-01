test_that("nif_viewer handles invalid inputs", {
  # Test non-nif object
  expect_error(nif_viewer(data.frame()), "Input must be a nif object")

  # Test empty dataset
  empty_nif <- nif(data.frame())
  expect_error(
    nif_viewer(empty_nif),
    "Missing required columns: ID, TIME, AMT, DV, EVID, USUBJID, ANALYTE, PARENT"
  )
})


test_that("nif_viewer handles missing required columns", {
  # Create minimal nif object missing required columns
  minimal_nif <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     0,    1,    0,     0
  ))

  # Test missing USUBJID
  expect_error(
    nif_viewer(minimal_nif),
    "Missing required columns: USUBJID, ANALYTE, PARENT"
  )
})


test_that("nif_viewer handles invalid data types", {
  # Create nif object with invalid data types
  invalid_nif <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~USUBJID, ~ANALYTE, ~PARENT,
    1,   "0",   "0",  1,    "0",   "0",  "1",      "TEST",   "TEST"  # Should be numeric
  ))

  expect_error(
    nif_viewer(invalid_nif),
    "TIME, AMT, and DV columns must be numeric"
  )
})


test_that("nif_viewer handles missing values", {
  # Create nif object with missing values
  missing_nif <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~PARENT,
    1,   0,     0,    1,    0,     0,   "1",      "TEST",   "TEST",
    NA,  1,     0,    1,    NA,    0,   "2",      "TEST",   "TEST"
  ))

  # Should show warning but not error
  expect_warning(
    nif_viewer(missing_nif),
    "Dataset contains missing values in ID, TIME, or EVID columns"
  )
})
