test_that("nif() creates empty nif object with correct structure", {
  result <- nif()

  # Check dimensions
  expect_equal(dim(result), c(0, 7))

  # Check column names include REF and all minimal fields
  expect_true("REF" %in% names(result))
  expect_true(all(c("ID", "TIME", "AMT", "CMT", "EVID", "DV") %in% names(result)))

  # Check class
  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")

  # Check REF is first column
  expect_equal(names(result)[1], "REF")

  # Check column order (REF should be first)
  expect_equal(names(result)[1], "REF")
})


test_that("nif() with valid data frame creates proper nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10,
    1,   2,     0,    2,    0,     20,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    2,    0,     15
  )

  result <- nif(test_data)

  # Check class
  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check data is arranged correctly (by ID, TIME, EVID descending)
  expect_equal(result$ID, c(1, 1, 1, 2, 2))
  expect_equal(result$TIME[1:3], c(0, 1, 2))
  expect_equal(result$EVID[1], 1)  # EVID=1 should come before EVID=0 at same TIME

  # Check all original columns are preserved
  expect_true(all(c("ID", "TIME", "AMT", "CMT", "EVID", "DV") %in% names(result)))

  # Check data values are preserved
  expect_equal(result$ID, test_data$ID[order(test_data$ID, test_data$TIME, -test_data$EVID)])
})


test_that("nif() with data frame including extra columns preserves them", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~USUBJID,    ~STUDYID,  ~DOSE,
    1,   0,     100,  1,    1,     NA,   "SUBJ-001",  "STUDY1",  100,
    1,   1,     0,    2,    0,     10,   "SUBJ-001",  "STUDY1",  100,
    2,   0,     100,  1,    1,     NA,   "SUBJ-002",  "STUDY1",  100
  )

  result <- nif(test_data)

  # Check extra columns are preserved
  expect_true("USUBJID" %in% names(result))
  expect_true("STUDYID" %in% names(result))
  expect_true("DOSE" %in% names(result))

  # Check values are preserved
  expect_equal(result$USUBJID, c("SUBJ-001", "SUBJ-001", "SUBJ-002"))
})


test_that("nif() with empty data frame handles correctly", {
  # Empty data frame with columns
  empty_df <- data.frame(
    ID = integer(0),
    TIME = numeric(0),
    AMT = numeric(0),
    CMT = numeric(0),
    EVID = integer(0),
    DV = numeric(0)
  )

  result <- nif(empty_df)

  # Check class
  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")

  # Check dimensions
  expect_equal(nrow(result), 0)

  # Check columns are preserved
  expect_true(all(c("ID", "TIME", "AMT", "CMT", "EVID", "DV") %in% names(result)))

  # Empty data frame without columns
  empty_df2 <- data.frame()
  expect_error(
    result2 <- nif(empty_df2),
    "Missing essential fields: ID, TIME, AMT, CMT, EVID and DV")
})


test_that("nif() with missing minimal fields handles gracefully", {
  # Missing some minimal fields
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,
    # DV is missing
    1,   0,     100,  1,    1,
    1,   1,     0,    2,    0
  )

  expect_error(result <- nif(test_data),
               "Missing essential fields: DV")

  # Missing all minimal fields
  test_data2 <- tibble::tribble(
    ~USUBJID,    ~STUDYID,
    "SUBJ-001",  "STUDY1"
  )

  # Should not error, just creates nif with available columns
  expect_error(result2 <- nif(test_data2),
               "Missing essential fields: ID, TIME, AMT, CMT, EVID and DV")
})


test_that("nif() validates numeric types for minimal fields", {
  # Non-numeric ID
  test_data <- tibble::tribble(
    ~ID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "one",  0,     100,  1,    1,     NA
  )

  expect_error(
    nif(test_data),
    "Non-numeric essential fields: ID"
  )

  # Non-numeric TIME
  test_data2 <- tibble::tribble(
    ~ID, ~TIME,    ~AMT, ~CMT, ~EVID, ~DV,
    1,   "zero",   100,  1,    1,     NA
  )

  expect_error(
    nif(test_data2),
    "Non-numeric essential fields: TIME"
  )

  # Non-numeric AMT
  test_data3 <- tibble::tribble(
    ~ID, ~TIME, ~AMT,    ~CMT, ~EVID, ~DV,
    1,   0,     "hundred", 1,    1,     NA
  )

  expect_error(
    nif(test_data3),
    "Non-numeric essential fields: AMT"
  )

  # Multiple non-numeric fields
  test_data4 <- tibble::tribble(
    ~ID,    ~TIME,    ~AMT, ~CMT, ~EVID, ~DV,
    "one",  "zero",   100,  1,    1,     NA
  )

  expect_error(
    nif(test_data4),
    "Non-numeric essential fields: ID and TIME"
  )
})



test_that("nif() arranges data correctly with arrange_and_add_ref", {
  # Test data with unsorted rows
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    2,   0,     100,  1,    1,     NA,
    1,   2,     0,    2,    0,     20,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10,
    2,   1,     0,    2,    0,     15
  )

  result <- nif(test_data)

  # Check sorting: ID, TIME, EVID (descending)
  expect_equal(result$ID, c(1, 1, 1, 2, 2))
  expect_equal(result$TIME[1:3], c(0, 1, 2))
  expect_equal(result$EVID[1], 1)  # EVID=1 before EVID=0 at same TIME
  expect_equal(result$EVID[2], 0)

  # Check REF is sequential
  expect_equal(result$REF, 1:5)
})


test_that("nif() handles DTC column in arrangement", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DTC,                        ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     "2024-01-01 10:00:00",       0,    2,    0,     10,
    1,   0,     "2024-01-01 08:00:00",       100,  1,    1,     NA,
    1,   0,     "2024-01-01 09:00:00",       0,    2,    0,     5
  ) %>%
    mutate(DTC = as.POSIXct(DTC))

  result <- nif(test_data)

  # Should be sorted by ID, TIME, DTC, EVID
  expect_equal(result$DTC[1], as.POSIXct("2024-01-01 08:00:00"))
  expect_equal(result$EVID[1], 1)  # EVID=1 at earliest DTC
  expect_equal(result$EVID[2], 0)  # EVID=0 at same DTC
})


test_that("nif() handles EVID sorting correctly", {
  # Test that EVID=1 comes before EVID=0 at same TIME
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     0,    2,    0,     10,  # observation
    1,   0,     100,  1,    1,     NA, # administration
    1,   1,     0,    2,    0,     20
  )

  result <- nif(test_data)

  # EVID=1 should come before EVID=0 at TIME=0
  expect_equal(result$EVID[1], 1)
  expect_equal(result$EVID[2], 0)
  expect_equal(result$TIME[1:2], c(0, 0))
})


test_that("nif() handles NA values in minimal fields", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   NA,    100,  1,    1,     NA,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10,
    NA,  0,     100,  1,    1,     NA
  )

  expect_error(
    result <- nif(test_data),
    "ID colum must not contain NA values!")
})


test_that("nif() handles duplicate IDs and TIMEs", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   0,     100,  1,    1,     NA,  # duplicate
    1,   1,     0,    2,    0,     10
  )

  result <- nif(test_data)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 3)
  expect_equal(result$REF, 1:3)
})


test_that("nif() works with tibble input", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10
  )

  result <- nif(test_data)

  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")
})


test_that("nif() works with base data.frame input", {
  test_data <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    AMT = c(100, 0),
    CMT = c(1, 2),
    EVID = c(1, 0),
    DV = c(NA, 10)
  )

  result <- nif(test_data)

  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")
})


test_that("nif() validates input type", {
  # Non-data.frame, non-sdtm object
  expect_error(
    nif(list(a = 1, b = 2)),
    "obj must be a data frame or sdtm object"
  )

  expect_error(
    nif("not a data frame"),
    "obj must be a data frame or sdtm object"
  )

  expect_error(
    nif(123),
    "obj must be a data frame or sdtm object"
  )
})


test_that("nif() handles sdtm object input", {
  # This should call nif_auto
  if (exists("examplinib_sad")) {
    expect_no_error(
      result <- nif(examplinib_sad, RS2023 ~ EXAMPLINIB, silent = TRUE)
    )
    expect_s3_class(result, "nif")
  }
})


test_that("nif() preserves data types", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~USUBJID,    ~STUDYID,
    1,   0.0,   100,  1,    1,     NA,   "SUBJ-001",  "STUDY1",
    1,   1.5,   0,    2,    0,     10.5, "SUBJ-001",  "STUDY1"
  )

  result <- nif(test_data)

  # Check numeric types are preserved
  expect_type(result$ID, "double")
  expect_type(result$TIME, "double")
  expect_type(result$AMT, "double")

  # Check character types are preserved
  expect_type(result$USUBJID, "character")
  expect_type(result$STUDYID, "character")
})


test_that("nif() REF column is integer type", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10
  )

  result <- nif(test_data)

  # REF should be integer or numeric
  expect_true(is.numeric(result$REF))
  expect_equal(result$REF, 1:2)
})


test_that("nif() REF column starts at 1 and is sequential", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10,
    1,   2,     0,    2,    0,     20,
    2,   0,     100,  1,    1,     NA
  )

  result <- nif(test_data)

  expect_equal(result$REF[1], 1)
  expect_equal(result$REF, 1:nrow(result))
  expect_equal(max(result$REF), nrow(result))
})


test_that("nif() orders columns correctly", {
  test_data <- tibble::tribble(
    ~DV,  ~CMT, ~EVID, ~AMT, ~TIME, ~ID, ~USUBJID,    ~STUDYID,
    NA,   1,    1,     100,  0,     1,   "SUBJ-001",  "STUDY1",
    10,   2,    0,     0,    1,     1,   "SUBJ-001",  "STUDY1"
  )

  result <- nif(test_data)

  # REF should be first
  expect_equal(names(result)[1], "REF")

  # Standard columns should be in expected order (after REF)
  expect_true(which(names(result) == "ID") < which(names(result) == "USUBJID"))
})


test_that("nif() handles factors that should be numeric", {
  # Factors that look numeric should still error
  test_data <- data.frame(
    ID = factor(c(1, 2)),
    TIME = c(0, 1),
    AMT = c(100, 0),
    CMT = c(1, 2),
    EVID = c(1, 0),
    DV = c(NA, 10)
  )

  expect_error(
    nif(test_data),
    "Non-numeric essential fields: ID"
  )
})


test_that("nif() handles silent parameter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA_integer_
  )

  # Should accept silent parameter without error
  expect_no_error(nif(test_data, silent = TRUE))
  expect_no_error(nif(test_data, silent = FALSE))
  expect_no_error(nif(test_data, silent = NULL))
})


test_that("nif() handles ... arguments", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA_integer_
  )

  # Should accept ... arguments without error (for data.frame path)
  expect_no_error(nif(test_data, some_arg = "value"))
})


test_that("nif() output works with other nif functions", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    2,    0,     15
  )

  result <- nif(test_data)

  # Should work with subjects() function
  expect_no_error(subjects_result <- subjects(result))
  expect_s3_class(subjects_result, "data.frame")
  expect_equal(nrow(subjects_result), 2)

  # Should work with other nif functions that validate nif objects
  expect_no_error(validate_nif(result))
})


test_that("nif() preserves additional columns order", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,  ~CUSTOM1, ~CUSTOM2,
    1,   0,     100,  1,    1,     NA,   "A",     "B",
    1,   1,     0,    2,    0,     10,   "A",     "B"
  )

  result <- nif(test_data)

  # Custom columns should be preserved
  expect_true("CUSTOM1" %in% names(result))
  expect_true("CUSTOM2" %in% names(result))
  expect_equal(result$CUSTOM1, c("A", "A"))
})


test_that("nif() handles larger data frames", {
  # Create larger test data (100 rows)
  test_data <- data.frame(
    ID = rep(1:10, each = 10),
    TIME = rep(0:9, times = 10),
    AMT = c(rep(100, 10), rep(0, 90)),
    CMT = c(rep(1, 10), rep(2, 90)),
    EVID = c(rep(1, 10), rep(0, 90)),
    DV = c(rep(NA, 10), runif(90, 0, 100))
  )

  result <- nif(test_data)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 100)
  expect_equal(result$REF, 1:100)
  expect_equal(max(result$REF), 100)

  # Verify data is sorted correctly
  expect_equal(result$ID[1:10], rep(1, 10))
  expect_equal(result$EVID[1], 1)  # First row should be EVID=1
})

