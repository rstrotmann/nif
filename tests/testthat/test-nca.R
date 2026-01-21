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



