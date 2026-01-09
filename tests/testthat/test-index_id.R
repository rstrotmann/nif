# Comprehensive tests for index_id() function

test_that("index_id() converts USUBJID to sequential numeric IDs", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-001", 1, 0, 1, 50, 0,
    "SUBJ-002", 0, 1, 1, NA, 100,
    "SUBJ-002", 1, 0, 1, 45, 0,
    "SUBJ-003", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should create ID column
  expect_true("ID" %in% names(result))
  expect_true(is.numeric(result$ID))

  # Each unique USUBJID should get a unique ID
  expect_equal(length(unique(result$ID)), 3)
  expect_equal(sort(unique(result$ID)), c(1, 2, 3))
})


test_that("index_id() assigns same ID to same USUBJID", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-001", 1, 0, 1, 50, 0,
    "SUBJ-001", 2, 0, 1, 25, 0,
    "SUBJ-002", 0, 1, 1, NA, 100,
    "SUBJ-001", 3, 0, 1, 12, 0
  )

  result <- index_id(test_data)

  # All rows with same USUBJID should have same ID
  subj1_ids <- result$ID[result$USUBJID == "SUBJ-001"]
  expect_true(all(subj1_ids == subj1_ids[1]))
  expect_equal(length(unique(subj1_ids)), 1)

  subj2_ids <- result$ID[result$USUBJID == "SUBJ-002"]
  expect_true(all(subj2_ids == subj2_ids[1]))
  expect_equal(length(unique(subj2_ids)), 1)

  # Different USUBJIDs should have different IDs
  expect_false(subj1_ids[1] == subj2_ids[1])
})


test_that("index_id() preserves all original columns", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT, ~STUDYID, ~SEX,
    "SUBJ-001", 0, 1, 1, NA, 100, "STUDY1", "M",
    "SUBJ-001", 1, 0, 1, 50, 0, "STUDY1", "M",
    "SUBJ-002", 0, 1, 1, NA, 100, "STUDY1", "F"
  )

  result <- index_id(test_data)

  # All original columns should be preserved
  expect_true(all(c("USUBJID", "TIME", "EVID", "CMT", "DV", "AMT", "STUDYID", "SEX") %in% names(result)))
  expect_equal(ncol(result), ncol(test_data) + 1)  # +1 for ID column
})


test_that("index_id() has right row order", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-003", 0, 1, 1, NA, 100,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-002", 0, 1, 1, NA, 100,
    "SUBJ-001", 1, 0, 1, 50, 0
  )

  result <- index_id(test_data)

  # Row order should be by USUBJID
  expect_equal(result$USUBJID, sort(test_data$USUBJID))
  expect_equal(result$TIME, test_data |> arrange(USUBJID) |> pull(TIME))
})


test_that("index_id() handles single subject", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-001", 1, 0, 1, 50, 0,
    "SUBJ-001", 2, 0, 1, 25, 0
  )

  result <- index_id(test_data)

  # Single subject should get ID = 1
  expect_true(all(result$ID == 1))
  expect_equal(unique(result$ID), 1)
})


test_that("index_id() handles multiple subjects with many rows", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-001", 1, 0, 1, 50, 0,
    "SUBJ-001", 2, 0, 1, 25, 0,
    "SUBJ-002", 0, 1, 1, NA, 100,
    "SUBJ-002", 1, 0, 1, 45, 0,
    "SUBJ-003", 0, 1, 1, NA, 100,
    "SUBJ-003", 1, 0, 1, 40, 0,
    "SUBJ-003", 2, 0, 1, 20, 0,
    "SUBJ-003", 3, 0, 1, 10, 0
  )

  result <- index_id(test_data)

  # Should have 3 unique IDs
  expect_equal(length(unique(result$ID)), 3)
  expect_equal(sort(unique(result$ID)), c(1, 2, 3))

  # Each subject should have consistent ID
  expect_equal(unique(result$ID[result$USUBJID == "SUBJ-001"]), 1)
  expect_equal(unique(result$ID[result$USUBJID == "SUBJ-002"]), 2)
  expect_equal(unique(result$ID[result$USUBJID == "SUBJ-003"]), 3)
})


test_that("index_id() overwrites existing ID column", {
  test_data <- tibble::tribble(
    ~USUBJID, ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 999, 0, 1, 1, NA, 100,
    "SUBJ-001", 999, 1, 0, 1, 50, 0,
    "SUBJ-002", 888, 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # ID should be recalculated based on USUBJID, not preserve old values
  expect_true(all(result$ID[result$USUBJID == "SUBJ-001"] == result$ID[result$USUBJID == "SUBJ-001"][1]))
  expect_true(all(result$ID[result$USUBJID == "SUBJ-002"] == result$ID[result$USUBJID == "SUBJ-002"][1]))
  expect_false(any(result$ID == 999))
  expect_false(any(result$ID == 888))
})


test_that("index_id() handles USUBJID with special characters", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001-A", 0, 1, 1, NA, 100,
    "SUBJ-001-A", 1, 0, 1, 50, 0,
    "SUBJ-002_B", 0, 1, 1, NA, 100,
    "SUBJ-003.C", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should handle special characters in USUBJID
  expect_equal(length(unique(result$ID)), 3)
  expect_true(all(result$ID[result$USUBJID == "SUBJ-001-A"] == result$ID[result$USUBJID == "SUBJ-001-A"][1]))
})


test_that("index_id() handles numeric USUBJID", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    1001, 0, 1, 1, NA, 100,
    1001, 1, 0, 1, 50, 0,
    1002, 0, 1, 1, NA, 100,
    1003, 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should handle numeric USUBJID
  expect_equal(length(unique(result$ID)), 3)
  expect_true(all(result$ID[result$USUBJID == 1001] == result$ID[result$USUBJID == 1001][1]))
})


test_that("index_id() handles missing USUBJID (NA values)", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    NA_character_, 0, 1, 1, NA, 100,
    "SUBJ-002", 0, 1, 1, NA, 100,
    NA_character_, 1, 0, 1, 50, 0
  )

  result <- index_id(test_data)

  # Should handle NA values
  expect_true("ID" %in% names(result))
  # NA USUBJIDs should get the same ID (they're all NA)
  na_ids <- result$ID[is.na(result$USUBJID)]
  if (length(na_ids) > 0) {
    expect_true(all(na_ids == na_ids[1]))
  }
})


test_that("index_id() handles empty data frame", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT
  ) |>
    filter(FALSE)

  result <- index_id(test_data)

  # Should return empty data frame with ID column
  expect_equal(nrow(result), 0)
  expect_true("ID" %in% names(result))
})


test_that("index_id() handles data with STUDYID column", {
  test_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "STUDY1-001", "STUDY1", 0, 1, 1, NA, 100,
    "STUDY1-001", "STUDY1", 1, 0, 1, 50, 0,
    "STUDY2-001", "STUDY2", 0, 1, 1, NA, 100,
    "STUDY1-002", "STUDY1", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should work with STUDYID present
  expect_true("ID" %in% names(result))
  expect_true("STUDYID" %in% names(result))
  expect_equal(length(unique(result$ID)), 3)
})


test_that("index_id() produces sequential IDs starting from 1", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-A", 0, 1, 1, NA, 100,
    "SUBJ-B", 0, 1, 1, NA, 100,
    "SUBJ-C", 0, 1, 1, NA, 100,
    "SUBJ-D", 0, 1, 1, NA, 100,
    "SUBJ-E", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # IDs should be sequential starting from 1
  unique_ids <- sort(unique(result$ID))
  expect_equal(unique_ids, 1:5)
  expect_equal(min(result$ID), 1)
  expect_equal(max(result$ID), 5)
})


test_that("index_id() maintains ID consistency across multiple calls", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-002", 0, 1, 1, NA, 100,
    "SUBJ-003", 0, 1, 1, NA, 100
  )

  result1 <- index_id(test_data)
  result2 <- index_id(test_data)

  # Same input should produce same ID mapping
  expect_equal(result1$ID, result2$ID)
})


test_that("index_id() handles mixed case USUBJID", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "subj-001", 1, 0, 1, 50, 0,
    "SubJ-002", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Case-sensitive: different cases should get different IDs
  expect_equal(length(unique(result$ID)), 3)
  expect_false(result$ID[result$USUBJID == "SUBJ-001"][1] ==
               result$ID[result$USUBJID == "subj-001"][1])
})


test_that("index_id() works when USUBJID column is missing", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    1, 0, 1, 1, NA, 100,
    1, 1, 0, 1, 50, 0
  )

  expect_no_error(
    index_id(test_data)
  )
})

