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


test_that("index_id() uses SUBJID when USUBJID is missing", {
  test_data <- tibble::tribble(
    ~SUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "001", 0, 1, 1, NA, 100,
    "001", 1, 0, 1, 50, 0,
    "002", 0, 1, 1, NA, 100,
    "003", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should create ID column based on SUBJID
  expect_true("ID" %in% names(result))
  expect_true(is.numeric(result$ID))
  expect_equal(length(unique(result$ID)), 3)
  expect_equal(sort(unique(result$ID)), c(1, 2, 3))

  # Same SUBJID should get same ID
  expect_true(all(result$ID[result$SUBJID == "001"] == result$ID[result$SUBJID == "001"][1]))
})


test_that("index_id() uses ID column when USUBJID and SUBJID are missing", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    101, 0, 1, 1, NA, 100,
    101, 1, 0, 1, 50, 0,
    102, 0, 1, 1, NA, 100,
    103, 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should create ID column based on original ID
  expect_true("ID" %in% names(result))
  expect_true(is.numeric(result$ID))
  expect_equal(length(unique(result$ID)), 3)
  expect_equal(sort(unique(result$ID)), c(1, 2, 3))

  # Same original ID should get same new ID
  expect_true(all(result$ID[test_data$ID == 101] == result$ID[test_data$ID == 101][1]))
})


test_that("index_id() prefers USUBJID over SUBJID when both are present", {
  test_data <- tibble::tribble(
    ~USUBJID, ~SUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", "001", 0, 1, 1, NA, 100,
    "SUBJ-001", "001", 1, 0, 1, 50, 0,
    "SUBJ-002", "001", 0, 1, 1, NA, 100,  # Same SUBJID but different USUBJID
    "SUBJ-003", "002", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should use USUBJID, not SUBJID
  # SUBJ-001 and SUBJ-002 have same SUBJID but should get different IDs
  expect_equal(length(unique(result$ID)), 3)
  expect_false(result$ID[result$USUBJID == "SUBJ-001"][1] ==
               result$ID[result$USUBJID == "SUBJ-002"][1])
})


test_that("index_id() prefers USUBJID over ID when both are present", {
  test_data <- tibble::tribble(
    ~USUBJID, ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 999, 0, 1, 1, NA, 100,
    "SUBJ-001", 999, 1, 0, 1, 50, 0,
    "SUBJ-002", 999, 0, 1, 1, NA, 100,  # Same ID but different USUBJID
    "SUBJ-003", 888, 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should use USUBJID, not original ID
  expect_equal(length(unique(result$ID)), 3)
  expect_false(result$ID[result$USUBJID == "SUBJ-001"][1] ==
               result$ID[result$USUBJID == "SUBJ-002"][1])
})


test_that("index_id() prefers SUBJID over ID when both are present", {
  test_data <- tibble::tribble(
    ~SUBJID, ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "001", 999, 0, 1, 1, NA, 100,
    "001", 999, 1, 0, 1, 50, 0,
    "002", 999, 0, 1, 1, NA, 100,  # Same ID but different SUBJID
    "003", 888, 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should use SUBJID, not original ID
  expect_equal(length(unique(result$ID)), 3)
  expect_false(result$ID[result$SUBJID == "001"][1] ==
               result$ID[result$SUBJID == "002"][1])
})


test_that("index_id() combines STUDYID with USUBJID to create unique IDs", {
  test_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "001", "STUDY1", 0, 1, 1, NA, 100,
    "001", "STUDY1", 1, 0, 1, 50, 0,
    "001", "STUDY2", 0, 1, 1, NA, 100,  # Same USUBJID, different STUDYID
    "002", "STUDY1", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Same USUBJID in different studies should get different IDs
  expect_equal(length(unique(result$ID)), 3)

  study1_id <- unique(result$ID[result$USUBJID == "001" & result$STUDYID == "STUDY1"])
  study2_id <- unique(result$ID[result$USUBJID == "001" & result$STUDYID == "STUDY2"])
  expect_false(study1_id == study2_id)
})


test_that("index_id() combines STUDYID with SUBJID when USUBJID is missing", {
  test_data <- tibble::tribble(
    ~SUBJID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "001", "STUDY1", 0, 1, 1, NA, 100,
    "001", "STUDY1", 1, 0, 1, 50, 0,
    "001", "STUDY2", 0, 1, 1, NA, 100,  # Same SUBJID, different STUDYID
    "002", "STUDY1", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Same SUBJID in different studies should get different IDs
  expect_equal(length(unique(result$ID)), 3)

  study1_id <- unique(result$ID[result$SUBJID == "001" & result$STUDYID == "STUDY1"])
  study2_id <- unique(result$ID[result$SUBJID == "001" & result$STUDYID == "STUDY2"])
  expect_false(study1_id == study2_id)
})


test_that("index_id() combines STUDYID with ID when USUBJID and SUBJID are missing", {
  test_data <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    101, "STUDY1", 0, 1, 1, NA, 100,
    101, "STUDY1", 1, 0, 1, 50, 0,
    101, "STUDY2", 0, 1, 1, NA, 100,  # Same ID, different STUDYID
    102, "STUDY1", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Same original ID in different studies should get different new IDs
  expect_equal(length(unique(result$ID)), 3)

  study1_id <- unique(result$ID[test_data$ID == 101 & test_data$STUDYID == "STUDY1"])
  study2_id <- unique(result$ID[test_data$ID == 101 & test_data$STUDYID == "STUDY2"])
  expect_false(study1_id == study2_id)
})


test_that("index_id() handles NA STUDYID values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", "STUDY1", 0, 1, 1, NA, 100,
    "SUBJ-001", NA_character_, 1, 0, 1, 50, 0,
    "SUBJ-002", "STUDY1", 0, 1, 1, NA, 100,
    "SUBJ-002", NA_character_, 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should handle NA STUDYID
  expect_true("ID" %in% names(result))
  expect_true("STUDYID" %in% names(result))

  # NA STUDYID should be treated as a separate study
  study1_id <- unique(result$ID[result$USUBJID == "SUBJ-001" & result$STUDYID == "STUDY1"])
})


test_that("index_id() arranges rows by STUDYID and USUBJID when both are present", {
  test_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-003", "STUDY2", 0, 1, 1, NA, 100,
    "SUBJ-001", "STUDY1", 0, 1, 1, NA, 100,
    "SUBJ-002", "STUDY1", 0, 1, 1, NA, 100,
    "SUBJ-001", "STUDY1", 1, 0, 1, 50, 0,
    "SUBJ-001", "STUDY2", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should be arranged by STUDYID first, then USUBJID
  expect_equal(result$STUDYID[1:2], c("STUDY1", "STUDY1"))
  expect_equal(result$USUBJID[1:2], c("SUBJ-001", "SUBJ-001"))
})


test_that("index_id() arranges rows by USUBJID when STUDYID is missing", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-003", 0, 1, 1, NA, 100,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-002", 0, 1, 1, NA, 100,
    "SUBJ-001", 1, 0, 1, 50, 0
  )

  result <- index_id(test_data)

  # Should be arranged by USUBJID
  expect_equal(result$USUBJID, sort(test_data$USUBJID))
})


test_that("index_id() arranges rows by SUBJID when USUBJID is missing", {
  test_data <- tibble::tribble(
    ~SUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "003", 0, 1, 1, NA, 100,
    "001", 0, 1, 1, NA, 100,
    "002", 0, 1, 1, NA, 100,
    "001", 1, 0, 1, 50, 0
  )

  result <- index_id(test_data)

  # Should be arranged by SUBJID
  expect_equal(result$SUBJID, sort(test_data$SUBJID))
})


test_that("index_id() throws error when none of USUBJID, SUBJID, or ID are present", {
  test_data <- tibble::tribble(
    ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    0, 1, 1, NA, 100,
    1, 0, 1, 50, 0
  )

  expect_error(
    index_id(test_data),
    "Input must have at least one of USUBJID, SUBJID or ID columns!"
  )
})


test_that("index_id() handles multiple studies with same USUBJID pattern", {
  test_data <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "STUDY1-001", "STUDY1", 0, 1, 1, NA, 100,
    "STUDY1-001", "STUDY1", 1, 0, 1, 50, 0,
    "STUDY2-001", "STUDY2", 0, 1, 1, NA, 100,
    "STUDY1-002", "STUDY1", 0, 1, 1, NA, 100,
    "STUDY2-002", "STUDY2", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should create unique IDs for each STUDYID-USUBJID combination
  expect_equal(length(unique(result$ID)), 4)

  # Verify same USUBJID in different studies get different IDs
  study1_001_id <- unique(result$ID[result$USUBJID == "STUDY1-001"])
  study2_001_id <- unique(result$ID[result$USUBJID == "STUDY2-001"])
  expect_false(study1_001_id == study2_001_id)
})


test_that("index_id() handles single row data frame", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Single row should get ID = 1
  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
  expect_true("ID" %in% names(result))
})


test_that("index_id() handles data with all three identifier columns present", {
  test_data <- tibble::tribble(
    ~USUBJID, ~SUBJID, ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", "001", 101, 0, 1, 1, NA, 100,
    "SUBJ-001", "001", 101, 1, 0, 1, 50, 0,
    "SUBJ-002", "002", 102, 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should use USUBJID (highest priority)
  expect_equal(length(unique(result$ID)), 2)
  expect_true(all(result$ID[result$USUBJID == "SUBJ-001"] == result$ID[result$USUBJID == "SUBJ-001"][1]))
})


test_that("index_id() handles very long USUBJID values", {
  long_id <- paste(rep("A", 200), collapse = "")
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    long_id, 0, 1, 1, NA, 100,
    long_id, 1, 0, 1, 50, 0,
    "SUBJ-002", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should handle long USUBJID values
  expect_equal(length(unique(result$ID)), 2)
  expect_true(all(result$ID[result$USUBJID == long_id] == result$ID[result$USUBJID == long_id][1]))
})


test_that("index_id() handles empty strings in USUBJID", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "", 0, 1, 1, NA, 100,
    "", 1, 0, 1, 50, 0,
    "SUBJ-001", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should handle empty strings
  expect_equal(length(unique(result$ID)), 2)
  expect_true(all(result$ID[result$USUBJID == ""] == result$ID[result$USUBJID == ""][1]))
})


test_that("index_id() maintains row count", {
  test_data <- tibble::tribble(
    ~USUBJID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "SUBJ-001", 0, 1, 1, NA, 100,
    "SUBJ-001", 1, 0, 1, 50, 0,
    "SUBJ-002", 0, 1, 1, NA, 100,
    "SUBJ-002", 1, 0, 1, 45, 0,
    "SUBJ-003", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should preserve all rows
  expect_equal(nrow(result), nrow(test_data))
})


test_that("index_id() handles data with only STUDYID and SUBJID", {
  test_data <- tibble::tribble(
    ~SUBJID, ~STUDYID, ~TIME, ~EVID, ~CMT, ~DV, ~AMT,
    "001", "STUDY1", 0, 1, 1, NA, 100,
    "001", "STUDY1", 1, 0, 1, 50, 0,
    "001", "STUDY2", 0, 1, 1, NA, 100,
    "002", "STUDY1", 0, 1, 1, NA, 100
  )

  result <- index_id(test_data)

  # Should combine STUDYID with SUBJID
  expect_equal(length(unique(result$ID)), 3)
  expect_true("STUDYID" %in% names(result))
  expect_true("SUBJID" %in% names(result))
})

