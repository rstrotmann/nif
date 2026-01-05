
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

