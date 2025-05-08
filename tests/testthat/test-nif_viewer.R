test_that("nif_viewer handles invalid inputs", {
  # Test non-nif object
  expect_error(nif_viewer(data.frame()), "Input must be a nif object")

  # Test empty dataset
  empty_nif <- new_nif(data.frame())
  expect_error(
    nif_viewer(empty_nif),
    "Missing required columns: ID, TIME, AMT, DV, EVID, USUBJID, ANALYTE, PARENT")
})


test_that("nif_viewer handles missing required columns", {
  # Create minimal nif object missing required columns
  minimal_nif <- new_nif(data.frame(
    ID = 1,
    TIME = 0,
    AMT = 0,
    DV = 0,
    EVID = 0
  ))

  # Test missing USUBJID
  expect_error(
    nif_viewer(minimal_nif),
    "Missing required columns: USUBJID, ANALYTE, PARENT")
})


test_that("nif_viewer handles invalid data types", {
  # Create nif object with invalid data types
  invalid_nif <- new_nif(data.frame(
    ID = 1,
    TIME = "0",  # Should be numeric
    AMT = "0",   # Should be numeric
    DV = "0",    # Should be numeric
    EVID = "0",  # Should be numeric
    USUBJID = "1",
    ANALYTE = "TEST",
    PARENT = "TEST"
  ))

  expect_error(
    nif_viewer(invalid_nif),
    "TIME, AMT, and DV columns must be numeric")
})


test_that("nif_viewer handles missing values", {
  # Create nif object with missing values
  missing_nif <- new_nif(data.frame(
    ID = c(1, NA),
    TIME = c(0, 1),
    AMT = c(0, 0),
    DV = c(0, 0),
    EVID = c(0, NA),
    USUBJID = c("1", "2"),
    ANALYTE = c("TEST", "TEST"),
    PARENT = c("TEST", "TEST")
  ))

  # Should show warning but not error
  expect_warning(nif_viewer(missing_nif),
    "Dataset contains missing values in ID, TIME, or EVID columns")
})

