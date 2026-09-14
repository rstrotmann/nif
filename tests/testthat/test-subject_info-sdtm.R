sdtm_from_dm <- function(dm) {
  sdtm(list(dm = dm))
}


full_dm <- function() {
  tibble::tribble(
    ~DOMAIN, ~STUDYID,    ~USUBJID, ~SUBJID,          ~ARM, ~ARMCD,       ~ACTARM, ~ACTARMCD, ~SITEID, ~COUNTRY,     ~RFSTDTC,     ~RFENDTC, ~SEX, ~AGE,  ~RACE, ~ETHNIC, ~EXTRA,
      "DM",  "STUDY1",  "SUBJ-001",   "001", "Treatment 1", "ARM1", "Treatment 1",    "ARM1",   "001",    "USA", "2023-01-01", "2023-02-01",  "M",   45, "WHITE",  "NHOL", "drop",
      "DM",  "STUDY1",  "SUBJ-002",   "002", "Treatment 1", "ARM1", "Treatment 1",    "ARM1",   "001",    "USA", "2023-01-02", "2023-02-02",  "F",   38, "BLACK",  "NHOL", "drop",
      "DM",  "STUDY1",  "SUBJ-003",   "003", "Treatment 2", "ARM2", "Treatment 2",    "ARM2",   "002",     "DE", "2023-01-03", "2023-02-03",  "M",   51, "ASIAN",  "HISP", "drop"
  )
}


expected_dm_cols <- c(
  "SUBJID", "USUBJID", "ARM", "ARMCD", "ACTARM", "ACTARMCD",
  "SITEID", "COUNTRY", "RFSTDTC", "RFENDTC", "SEX", "AGE", "RACE", "ETHNIC"
)


# ---- Input validation --------------------------------------------------------

test_that("subject_info.sdtm rejects non-sdtm input", {
  expect_error(
    subject_info.sdtm(mtcars, "SUBJ-001"),
    "Input must be a sdtm object"
  )
})


test_that("subject_info.sdtm requires a dm domain", {
  obj <- sdtm(list(
    pc = tibble::tribble(
        ~USUBJID, ~PCTESTCD,
      "SUBJ-001",    "DRUG"
    )
  ))

  expect_error(
    subject_info.sdtm(obj, "SUBJ-001"),
    "Expected domain missing in sdtm object: dm"
  )
})


test_that("subject_info.sdtm requires the dm domain name to be lowercase", {
  obj <- sdtm(list(DM = full_dm()))

  expect_error(
    subject_info.sdtm(obj, "SUBJ-001"),
    "Expected domain missing in sdtm object: dm"
  )
})


test_that("subject_info.sdtm validates id", {
  obj <- sdtm_from_dm(full_dm())

  expect_error(subject_info.sdtm(obj, NULL), "id must not be NULL")
  expect_error(subject_info.sdtm(obj, 1), "id must be a character value")
  expect_error(
    subject_info.sdtm(obj, factor("SUBJ-001")),
    "id must be a character value"
  )
  expect_error(subject_info.sdtm(obj, ""), "id must be a non-empty string")
  expect_error(
    subject_info.sdtm(obj, c("SUBJ-001", "")),
    "id must be a non-empty string"
  )
  expect_error(
    subject_info.sdtm(obj, NA_character_),
    "id must not contain NA"
  )
  expect_error(
    subject_info.sdtm(obj, c("SUBJ-001", NA_character_)),
    "id must not contain NA"
  )
})


test_that("subject_info.sdtm errors when dm has no USUBJID column", {
  obj <- sdtm_from_dm(tibble::tribble(
    ~SUBJID, ~SEX,
      "001",  "M"
  ))

  expect_error(subject_info.sdtm(obj, "001"), "USUBJID")
})


# ---- Filtering ---------------------------------------------------------------

test_that("subject_info.sdtm returns the matching subject from dm", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, "SUBJ-002")

  expect_equal(nrow(out), 1L)
  expect_equal(out$USUBJID, "SUBJ-002")
  expect_equal(out$SUBJID, "002")
  expect_equal(out$SEX, "F")
  expect_equal(out$AGE, 38)
  expect_equal(out$COUNTRY, "USA")
  expect_equal(out$ARMCD, "ARM1")
})


test_that("subject_info() dispatches to subject_info.sdtm", {
  obj <- sdtm_from_dm(full_dm())

  expect_identical(
    subject_info(obj, "SUBJ-001"),
    subject_info.sdtm(obj, "SUBJ-001")
  )
})


test_that("subject_info.sdtm returns multiple requested subjects", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, c("SUBJ-001", "SUBJ-003"))

  expect_equal(nrow(out), 2L)
  expect_equal(out$USUBJID, c("SUBJ-001", "SUBJ-003"))
  expect_equal(out$SEX, c("M", "M"))
  expect_equal(out$AGE, c(45, 51))
})


test_that("subject_info.sdtm preserves dm row order, not the order of id", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, c("SUBJ-003", "SUBJ-001"))

  expect_equal(out$USUBJID, c("SUBJ-001", "SUBJ-003"))
})


test_that("subject_info.sdtm returns zero rows for an unknown USUBJID", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, "NOPE")

  expect_equal(nrow(out), 0L)
  expect_equal(names(out), expected_dm_cols)
})


test_that("subject_info.sdtm keeps known ids and silently drops unknown ids", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, c("SUBJ-001", "NOPE"))

  expect_equal(out$USUBJID, "SUBJ-001")
})


test_that("subject_info.sdtm matches USUBJID case-sensitively", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, "subj-001")

  expect_equal(nrow(out), 0L)
})


test_that("subject_info.sdtm with character(0) id returns zero rows and the selected columns", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, character(0))

  expect_equal(nrow(out), 0L)
  expect_equal(names(out), expected_dm_cols)
})


test_that("subject_info.sdtm keeps duplicate dm rows for the same USUBJID", {
  obj <- sdtm_from_dm(tibble::tribble(
      ~USUBJID, ~SEX, ~AGE,
    "SUBJ-001",  "M",   45,
    "SUBJ-001",  "M",   46
  ))

  out <- subject_info.sdtm(obj, "SUBJ-001")

  expect_equal(nrow(out), 2L)
  expect_equal(out$AGE, c(45, 46))
})


test_that("subject_info.sdtm works on an empty dm domain", {
  obj <- sdtm_from_dm(full_dm()[integer(0), ])

  out <- subject_info.sdtm(obj, "SUBJ-001")

  expect_equal(nrow(out), 0L)
  expect_equal(names(out), expected_dm_cols)
})


# ---- Column selection --------------------------------------------------------

test_that("subject_info.sdtm selects baseline dm columns in a fixed order", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, "SUBJ-001")

  expect_equal(names(out), expected_dm_cols)
})


test_that("subject_info.sdtm drops dm columns that are not in the baseline set", {
  obj <- sdtm_from_dm(full_dm())

  out <- subject_info.sdtm(obj, "SUBJ-001")

  expect_false("DOMAIN" %in% names(out))
  expect_false("STUDYID" %in% names(out))
  expect_false("EXTRA" %in% names(out))
})


test_that("subject_info.sdtm keeps only the baseline columns that exist in dm", {
  obj <- sdtm_from_dm(tibble::tribble(
      ~USUBJID, ~SEX, ~AGE,
    "SUBJ-001",  "M",   45
  ))

  out <- subject_info.sdtm(obj, "SUBJ-001")

  expect_equal(names(out), c("USUBJID", "SEX", "AGE"))
  expect_equal(out$USUBJID, "SUBJ-001")
  expect_equal(out$SEX, "M")
  expect_equal(out$AGE, 45)
})


test_that("subject_info.sdtm preserves column types from dm", {
  obj <- sdtm_from_dm(tibble::tribble(
      ~USUBJID, ~SEX, ~AGE,
    "SUBJ-001",  "M",  45L
  ))

  out <- subject_info.sdtm(obj, "SUBJ-001")

  expect_type(out$USUBJID, "character")
  expect_type(out$SEX, "character")
  expect_type(out$AGE, "integer")
})


test_that("subject_info.sdtm ignores non-dm domains", {
  obj <- sdtm(list(
    dm = tibble::tribble(
        ~USUBJID, ~SEX,
      "SUBJ-001",  "M"
    ),
    ex = tibble::tribble(
        ~USUBJID, ~EXDOSE,
      "SUBJ-001",     100
    )
  ))

  out <- subject_info.sdtm(obj, "SUBJ-001")

  expect_equal(names(out), c("USUBJID", "SEX"))
  expect_false("EXDOSE" %in% names(out))
})


# ---- Return class and print --------------------------------------------------

test_that("subject_info.sdtm returns a subject_info tibble", {
  out <- subject_info.sdtm(sdtm_from_dm(full_dm()), "SUBJ-001")

  expect_s3_class(out, "subject_info")
  expect_s3_class(out, "tbl_df")
  expect_true(tibble::is_tibble(out))
})


test_that("print.subject_info shows a header and dm fields for sdtm output", {
  out <- subject_info.sdtm(sdtm_from_dm(full_dm()), "SUBJ-001")

  printed <- capture.output(result <- print(out))

  expect_true(any(grepl("Subject information", printed)))
  expect_true(any(grepl("SUBJ-001", printed)))
  expect_true(any(grepl("USUBJID", printed)))
  expect_true(any(grepl("Treatment 1", printed)))
  expect_identical(result, out)
})


test_that("subject_info.sdtm works with examplinib_fe", {
  ids <- subjects(examplinib_fe)$USUBJID[1:2]

  out <- subject_info(examplinib_fe, ids)

  expect_s3_class(out, "subject_info")
  expect_equal(nrow(out), 2L)
  expect_equal(out$USUBJID, ids)
  expect_equal(names(out), expected_dm_cols)
})
