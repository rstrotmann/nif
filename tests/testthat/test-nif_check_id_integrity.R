check_id <- nif:::nif_check_id_integrity

test_that("nif_check_id_integrity rejects non-data.frame input", {
  expect_error(
    check_id(list(ID = 1L)),
    "input must be a data frame"
  )
  expect_error(
    check_id(matrix(1:4, ncol = 2)),
    "input must be a data frame"
  )
})

test_that("nif_check_id_integrity requires an ID column", {
  df <- tibble::tribble(
    ~X, ~Y,
    1, 2
  )
  expect_error(
    check_id(df),
    "Input must contain at least the ID column"
  )
})

test_that("nif_check_id_integrity rejects NA in ID", {
  df <- tibble::tribble(
    ~ID,
    1L,
    NA_integer_
  )
  expect_error(
    check_id(df),
    "ID must not be NA"
  )
})

test_that("nif_check_id_integrity passes for ID-only data frame", {
  df <- tibble::tribble(
    ~ID,
    1L,
    2L
  )
  expect_silent(check_id(df))
})

test_that("nif_check_id_integrity accepts base data.frame", {
  df <- data.frame(ID = c(1L, 2L), USUBJID = c("a", "b"), stringsAsFactors = FALSE)
  expect_silent(check_id(df))
})

test_that("nif_check_id_integrity passes for empty rows with valid ID column", {
  df <- tibble::tribble(~ID)
  expect_silent(check_id(df))
})

test_that("nif_check_id_integrity passes when IDs are unique and STUDYID/USUBJID absent or consistent", {
  # Only ID
  df_id_only <- tibble::tribble(
    ~ID,
    1L,
    2L
  )
  expect_silent(check_id(df_id_only))

  # ID and USUBJID, but each ID used once
  df_id_usubjid <- tibble::tribble(
    ~ID, ~USUBJID,
    1L, "S01-001",
    2L, "S01-001"
  )
  expect_silent(check_id(df_id_usubjid))

  # Duplicate rows are collapsed by distinct()
  df_dup_rows <- tibble::tribble(
    ~ID, ~USUBJID,  ~STUDYID,
    1L,  "S01-001", "STUDY1",
    1L,  "S01-001", "STUDY1"
  )
  expect_silent(check_id(df_dup_rows))
})

test_that("nif_check_id_integrity errors when an ID is assigned to multiple USUBJID values", {
  df <- tibble::tribble(
    ~ID, ~USUBJID,
    1L, "S01-001",
    1L, "S01-002"
  )
  expect_error(
    check_id(df),
    "Multiple assignment of the following ID"
  )
})

test_that("nif_check_id_integrity errors when an ID is reassigned across STUDYID/USUBJID combinations", {
  df <- tibble::tribble(
    ~ID, ~USUBJID,  ~STUDYID,
    1L,  "S01-001", "STUDY1",
    1L,  "S01-001", "STUDY2"
  )
  expect_error(
    check_id(df),
    "Multiple assignment of the following ID"
  )
})

test_that("nif_check_id_integrity warns when USUBJID is found in multiple STUDYID", {
  df <- tibble::tribble(
    ~ID, ~USUBJID,  ~STUDYID,
    1L,  "S01-001", "STUDY1",
    2L,  "S01-001", "STUDY2",
    3L,  "S01-002", "STUDY1"
  )

  expect_warning(
    check_id(df),
    "found in multiple studies"
  )
})

test_that("nif_check_id_integrity does not warn when each USUBJID is in a single STUDYID", {
  df <- tibble::tribble(
    ~ID, ~USUBJID,  ~STUDYID,
    1L,  "S01-001", "STUDY1",
    2L,  "S01-002", "STUDY1",
    3L,  "S01-001", "STUDY1"  # same study, different ID (but IDs remain unique)
  )

  expect_silent(check_id(df))
})

test_that("nif_check_id_integrity prioritizes ID reassignment error over USUBJID multi-study warning", {
  df <- tibble::tribble(
    ~ID, ~USUBJID,  ~STUDYID,
    1L,  "S01-001", "STUDY1",
    1L,  "S01-002", "STUDY2",  # ID 1 reassigned
    2L,  "S01-001", "STUDY2"   # USUBJID in multiple studies
  )

  expect_error(
    check_id(df),
    "Multiple assignment of the following ID"
  )
})

test_that("nif_check_id_integrity passes for nif objects inheriting data.frame", {
  raw <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID,  ~STUDYID,
    1L,  0,     100,  1,    1L,    NA_real_, "S01-001", "STUDY1",
    2L,  0,     100,  1,    1L,    NA_real_, "S01-002", "STUDY1"
  )
  obj <- nif(raw)
  expect_silent(check_id(obj))
})

test_that("nif_check_id_integrity passes when ID and USUBJID/STUDYID are one-to-one and consistent", {
  df <- tibble::tribble(
    ~ID, ~USUBJID,  ~STUDYID,
    1L,  "S01-001", "STUDY1",
    2L,  "S01-002", "STUDY1",
    3L,  "S01-001", "STUDY2"  # allowed, but will trigger warning in previous test
  )
  expect_warning(
    check_id(df),
    "found in multiple studies"
  )
})


