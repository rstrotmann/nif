test_that("arrange_and_add_ref works with all standard columns", {
  # Create test data with all columns
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DTC,                        ~EVID, ~DV,  ~AMT,
    2,   1.0,   "2024-01-01 10:00:00",       0,     10,   0,
    1,   0.0,   "2024-01-01 08:00:00",       1,     NA,   100,
    1,   0.0,   "2024-01-01 08:00:00",       0,     5,    0,
    2,   0.0,   "2024-01-01 09:00:00",       1,     NA,   100,
    1,   2.0,   "2024-01-01 10:00:00",       0,     15,   0,
    2,   0.0,   "2024-01-01 09:00:00",       0,     8,    0
  ) %>%
    mutate(DTC = as.POSIXct(DTC))

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: ID, TIME, DTC, EVID (descending)
  # Expected order:
  # ID=1, TIME=0, DTC="2024-01-01 08:00:00", EVID=1 (REF=1)
  # ID=1, TIME=0, DTC="2024-01-01 08:00:00", EVID=0 (REF=2)
  # ID=1, TIME=2, DTC="2024-01-01 10:00:00", EVID=0 (REF=3)
  # ID=2, TIME=0, DTC="2024-01-01 09:00:00", EVID=1 (REF=4)
  # ID=2, TIME=0, DTC="2024-01-01 09:00:00", EVID=0 (REF=5)
  # ID=2, TIME=1, DTC="2024-01-01 10:00:00", EVID=0 (REF=6)
  expect_equal(result$ID[1:3], c(1, 1, 1))
  expect_equal(result$ID[4:6], c(2, 2, 2))
  expect_equal(result$EVID[1], 1)  # EVID=1 should come before EVID=0 at same TIME
  expect_equal(result$EVID[2], 0)

  # Check all original columns are preserved
  expect_true(all(c("ID", "TIME", "DTC", "EVID", "DV", "AMT") %in% names(result)))
})


test_that("arrange_and_add_ref works without DTC column", {
  # Create test data without DTC
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,  ~AMT,
    2,   1.0,   0,     10,   0,
    1,   0.0,   1,     NA,   100,
    1,   0.0,   0,     5,    0,
    2,   0.0,   1,     NA,   100,
    1,   2.0,   0,     15,   0,
    2,   0.0,   0,     8,    0
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: ID, TIME, EVID (descending)
  # Expected order:
  # ID=1, TIME=0, EVID=1 (REF=1)
  # ID=1, TIME=0, EVID=0 (REF=2)
  # ID=1, TIME=2, EVID=0 (REF=3)
  # ID=2, TIME=0, EVID=1 (REF=4)
  # ID=2, TIME=0, EVID=0 (REF=5)
  # ID=2, TIME=1, EVID=0 (REF=6)
  expect_equal(result$ID[1:3], c(1, 1, 1))
  expect_equal(result$ID[4:6], c(2, 2, 2))
  expect_equal(result$EVID[1], 1)
  expect_equal(result$EVID[2], 0)

  # Check DTC is not in result
  expect_false("DTC" %in% names(result))
})


test_that("arrange_and_add_ref works without TIME column", {
  # Create test data without TIME
  test_data <- tibble::tribble(
    ~ID, ~DTC,                        ~EVID, ~DV,  ~AMT,
    2,   "2024-01-01 10:00:00",       0,     10,   0,
    1,   "2024-01-01 08:00:00",       1,     NA,   100,
    1,   "2024-01-01 08:00:00",       0,     5,    0,
    2,   "2024-01-01 09:00:00",       1,     NA,   100
  ) %>%
    mutate(DTC = as.POSIXct(DTC))

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: ID, DTC, EVID (descending)
  expect_equal(result$ID[1:2], c(1, 1))
  expect_equal(result$ID[3:4], c(2, 2))
  expect_equal(result$EVID[1], 1)  # EVID=1 should come before EVID=0 at same DTC
  expect_equal(result$EVID[2], 0)

  # Check TIME is not in result
  expect_false("TIME" %in% names(result))
})


test_that("arrange_and_add_ref works without ID column", {
  # Create test data without ID
  test_data <- tibble::tribble(
    ~TIME, ~DTC,                        ~EVID, ~DV,  ~AMT,
    1.0,   "2024-01-01 10:00:00",       0,     10,   0,
    0.0,   "2024-01-01 08:00:00",       1,     NA,   100,
    0.0,   "2024-01-01 08:00:00",       0,     5,    0,
    2.0,   "2024-01-01 10:00:00",       0,     15,   0
  ) %>%
    mutate(DTC = as.POSIXct(DTC))

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: TIME, DTC, EVID (descending)
  expect_equal(result$TIME[1:2], c(0.0, 0.0))
  expect_equal(result$TIME[3:4], c(1.0, 2.0))
  expect_equal(result$EVID[1], 1)  # EVID=1 should come before EVID=0 at same TIME/DTC
  expect_equal(result$EVID[2], 0)

  # Check ID is not in result
  expect_false("ID" %in% names(result))
})


test_that("arrange_and_add_ref works without EVID column", {
  # Create test data without EVID
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DTC,                        ~DV,  ~AMT,
    2,   1.0,   "2024-01-01 10:00:00",       10,   0,
    1,   0.0,   "2024-01-01 08:00:00",       5,    0,
    1,   2.0,   "2024-01-01 10:00:00",       15,   0,
    2,   0.0,   "2024-01-01 09:00:00",       8,    0
  ) %>%
    mutate(DTC = as.POSIXct(DTC))

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: ID, TIME, DTC
  expect_equal(result$ID[1:2], c(1, 1))
  expect_equal(result$ID[3:4], c(2, 2))
  expect_equal(result$TIME[1], 0.0)
  expect_equal(result$TIME[2], 2.0)

  # Check EVID is not in result
  expect_false("EVID" %in% names(result))
})


test_that("arrange_and_add_ref works with only ID column", {
  # Create test data with only ID
  test_data <- tibble::tribble(
    ~ID, ~DV,  ~AMT,
    2,   10,   0,
    1,   5,    0,
    3,   15,   0
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: ID
  expect_equal(result$ID, c(1, 2, 3))
})


test_that("arrange_and_add_ref works with empty data frame", {
  # Create empty data frame
  test_data <- tibble::tibble(
    ID = integer(0),
    TIME = numeric(0),
    EVID = integer(0)
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF is empty
  expect_equal(nrow(result), 0)
  expect_equal(length(result$REF), 0)
})


test_that("arrange_and_add_ref works with single row", {
  # Create single row data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    1,   0.0,   1,     NA
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF value
  expect_equal(result$REF, 1)
  expect_equal(nrow(result), 1)
})


test_that("arrange_and_add_ref handles NA values correctly", {
  # Create test data with NA values
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    1,   NA,    0,     10,
    1,   0.0,   1,     NA,
    1,   1.0,   0,     20,
    NA,  0.0,   0,     5,
    2,   0.0,   1,     NA
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # NA values should be sorted appropriately (NA comes last in ascending order)
  # Check that rows with NA ID come after rows with valid ID
  expect_false(any(is.na(result$ID[1:4])))
  if (any(is.na(result$ID))) {
    expect_true(which(is.na(result$ID)) > 4)
  }
})


test_that("arrange_and_add_ref preserves additional columns", {
  # Create test data with additional columns
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,  ~AMT, ~CMT, ~PARENT,
    2,   1.0,   0,     10,   0,    2,    "A",
    1,   0.0,   1,     NA,   100,  1,    "A",
    1,   0.0,   0,     5,    0,    2,    "A"
  )

  result <- arrange_and_add_ref(test_data)

  # Check all original columns are preserved
  expect_true(all(c("ID", "TIME", "EVID", "DV", "AMT", "CMT", "PARENT") %in% names(result)))

  # Check REF is first
  expect_equal(names(result)[1], "REF")

  # Check values are preserved
  expect_equal(result$PARENT, c("A", "A", "A"))
  expect_equal(result$CMT, c(1, 2, 2))
})


test_that("arrange_and_add_ref handles duplicate times correctly", {
  # Create test data with duplicate times
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    1,   0.0,   0,     5,
    1,   0.0,   1,     NA,
    1,   0.0,   0,     10,
    1,   1.0,   0,     15
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # At same TIME, EVID=1 should come before EVID=0
  expect_equal(result$EVID[1], 1)
  expect_equal(result$EVID[2:3], c(0, 0))
})


test_that("arrange_and_add_ref handles multiple subjects correctly", {
  # Create test data with multiple subjects
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    3,   0.0,   1,     NA,
    1,   0.0,   1,     NA,
    2,   0.0,   1,     NA,
    1,   1.0,   0,     10,
    3,   1.0,   0,     30,
    2,   1.0,   0,     20,
    1,   2.0,   0,     15,
    2,   2.0,   0,     25,
    3,   2.0,   0,     35
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: ID first, then TIME, then EVID
  expect_equal(result$ID, c(1, 1, 1, 2, 2, 2, 3, 3, 3))
  expect_equal(result$TIME[1:3], c(0.0, 1.0, 2.0))
  expect_equal(result$TIME[4:6], c(0.0, 1.0, 2.0))
  expect_equal(result$TIME[7:9], c(0.0, 1.0, 2.0))
  expect_equal(result$EVID[1], 1)  # EVID=1 at TIME=0
  expect_equal(result$EVID[2], 0)  # EVID=0 at TIME=1
})


test_that("arrange_and_add_ref handles DTC ordering correctly", {
  # Create test data with DTC to test datetime ordering
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DTC,                        ~EVID, ~DV,
    1,   0.0,   "2024-01-01 10:00:00",       0,     10,
    1,   0.0,   "2024-01-01 08:00:00",       1,     NA,
    1,   0.0,   "2024-01-01 09:00:00",       0,     5,
    1,   0.0,   "2024-01-01 08:00:00",       0,     3
  ) %>%
    mutate(DTC = as.POSIXct(DTC))

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering: ID, TIME, DTC, EVID
  # At same ID and TIME, should order by DTC, then EVID
  expect_equal(result$DTC[1], as.POSIXct("2024-01-01 08:00:00"))
  expect_equal(result$EVID[1], 1)  # EVID=1 at earliest DTC
  expect_equal(result$EVID[2], 0)  # EVID=0 at same DTC
  expect_equal(result$DTC[3], as.POSIXct("2024-01-01 09:00:00"))
  expect_equal(result$DTC[4], as.POSIXct("2024-01-01 10:00:00"))
})


test_that("arrange_and_add_ref works with data frame input", {
  # Test with base data.frame
  test_data <- data.frame(
    ID = c(2, 1, 1),
    TIME = c(1.0, 0.0, 2.0),
    EVID = c(0, 1, 0),
    DV = c(10, NA, 15)
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering
  expect_equal(result$ID, c(1, 1, 2))
  expect_equal(result$TIME[1:2], c(0.0, 2.0))
})


test_that("arrange_and_add_ref works with tibble input", {
  # Test with tibble
  test_data <- tibble::tibble(
    ID = c(2, 1, 1),
    TIME = c(1.0, 0.0, 2.0),
    EVID = c(0, 1, 0),
    DV = c(10, NA, 15)
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering
  expect_equal(result$ID, c(1, 1, 2))
})


test_that("arrange_and_add_ref handles EVID descending order correctly", {
  # Create test data to specifically test EVID descending order
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    1,   0.0,   0,     5,   # Observation
    1,   0.0,   1,     NA,  # Dose event - should come first
    1,   1.0,   0,     10,
    1,   1.0,   1,     NA   # Dose event - should come first
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # At same ID and TIME, EVID=1 should come before EVID=0
  expect_equal(result$EVID[1], 1)  # TIME=0, EVID=1
  expect_equal(result$EVID[2], 0)  # TIME=0, EVID=0
  expect_equal(result$EVID[3], 1)  # TIME=1, EVID=1
  expect_equal(result$EVID[4], 0)  # TIME=1, EVID=0
})


test_that("arrange_and_add_ref returns data frame with correct class", {
  # Create test data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV,
    1,   0.0,   1,     NA,
    1,   1.0,   0,     10
  )

  result <- arrange_and_add_ref(test_data)

  # Check it's a data frame
  expect_s3_class(result, "data.frame")

  # Check REF is first column
  expect_equal(names(result)[1], "REF")
})


test_that("arrange_and_add_ref handles all columns missing except one", {
  # Create test data with only one of the arrange columns
  test_data <- tibble::tribble(
    ~TIME, ~DV,
    2.0,   20,
    0.0,   5,
    1.0,   10
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Check ordering by TIME
  expect_equal(result$TIME, c(0.0, 1.0, 2.0))
})


test_that("arrange_and_add_ref handles no arrange columns", {
  # Create test data with none of the arrange columns
  test_data <- tibble::tribble(
    ~DV,  ~AMT, ~CMT,
    10,   0,    2,
    5,    100,  1,
    15,   0,    2
  )

  result <- arrange_and_add_ref(test_data)

  # Check REF column exists and is first
  expect_true("REF" %in% names(result))
  expect_equal(names(result)[1], "REF")

  # Check REF values are sequential
  expect_equal(result$REF, 1:nrow(result))

  # Data should remain in original order (no arrange columns present)
  expect_equal(nrow(result), 3)
})

