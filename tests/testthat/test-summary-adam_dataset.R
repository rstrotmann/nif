adam_dataset_adpc <- function() {
  new_dataset(tibble::tribble(
    ~STUDYID, ~DOMAIN,   ~USUBJID, ~PARAMCD, ~PARAM, ~SAFFL, ~FASFL, ~AVAL,
        "S1",  "ADPC", "SUBJ-001",   "CMAX", "Cmax",    "Y",    "Y",    10,
        "S1",  "ADPC", "SUBJ-001",    "AUC",  "AUC",    "Y",    "Y",    20,
        "S1",  "ADPC", "SUBJ-002",   "CMAX", "Cmax",    "Y",    "N",    12
  ))
}


adam_dataset_adsl <- function() {
  new_dataset(tibble::tribble(
    ~STUDYID, ~DOMAIN,   ~USUBJID, ~SAFFL,
        "S1",  "ADSL", "SUBJ-001",    "Y",
        "S1",  "ADSL", "SUBJ-002",    "N"
  ))
}


# ---- Input validation --------------------------------------------------------

test_that("summary.adam_dataset rejects non-adam_dataset input", {
  expect_error(
    summary.adam_dataset(mtcars),
    "Input must be an adam_dataset object"
  )
  expect_error(
    summary.adam_dataset(tibble::tibble(USUBJID = "U1")),
    "Input must be an adam_dataset object"
  )
})


# ---- Return structure --------------------------------------------------------

test_that("summary.adam_dataset returns a summary_dataset list with expected names", {
  s <- summary(adam_dataset_adpc())

  expect_s3_class(s, "summary_dataset")
  expect_type(s, "list")
  expect_equal(
    names(s),
    c("data", "domain", "subjects", "study", "flags", "params")
  )
})


test_that("summary() dispatches to summary.adam_dataset", {
  obj <- adam_dataset_adpc()

  expect_identical(summary(obj), summary.adam_dataset(obj))
})


test_that("summary.adam_dataset stores the original dataset in data", {
  obj <- adam_dataset_adpc()
  s <- summary(obj)

  expect_identical(s$data, obj)
  expect_s3_class(s$data, "adam_dataset")
})


test_that("summary.adam_dataset ignores unused dots", {
  expect_no_error(summary(adam_dataset_adpc(), extra = TRUE))
})


# ---- Domain, study, subjects -------------------------------------------------

test_that("summary.adam_dataset extracts unique domain, study, and subjects", {
  s <- summary(adam_dataset_adpc())

  expect_equal(s$domain, "ADPC")
  expect_equal(s$study, "S1")
  expect_equal(s$subjects, c("SUBJ-001", "SUBJ-002"))
})


test_that("summary.adam_dataset preserves first-seen order of unique USUBJID", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~STUDYID,
          "U2",     "S1",
          "U1",     "S1",
          "U2",     "S1"
  ))

  expect_equal(summary(obj)$subjects, c("U2", "U1"))
})


test_that("summary.adam_dataset uses the string NULL when DOMAIN is absent", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~STUDYID,
          "U1",     "S1"
  ))

  s <- summary(obj)

  expect_equal(s$domain, "NULL")
  expect_type(s$domain, "character")
})


test_that("summary.adam_dataset keeps only the first unique DOMAIN (ifelse)", {
  obj <- new_dataset(tibble::tribble(
    ~DOMAIN, ~USUBJID, ~STUDYID,
     "ADSL",     "U1",     "S1",
     "ADAE",     "U1",     "S1"
  ))

  s <- summary(obj)

  expect_equal(s$domain, "ADSL")
  expect_length(s$domain, 1L)
})


test_that("summary.adam_dataset stores all unique STUDYID values", {
  obj <- new_dataset(tibble::tribble(
    ~STUDYID, ~DOMAIN, ~USUBJID,
        "S1",  "ADSL",     "U1",
        "S2",  "ADSL",     "U2"
  ))

  expect_equal(summary(obj)$study, c("S1", "S2"))
})


test_that("summary.adam_dataset is NULL for subjects and study when those columns are absent", {
  obj <- new_dataset(tibble::tribble(
    ~AVAL,
       1
  ))

  s <- summary(obj)

  expect_null(s$subjects)
  expect_null(s$study)
  expect_equal(s$domain, "NULL")
})


test_that("summary.adam_dataset includes NA among unique USUBJID and STUDYID", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~STUDYID, ~DOMAIN,
          "U1",     "S1",  "ADSL",
    NA_character_, NA_character_,  "ADSL"
  ))

  s <- summary(obj)

  expect_equal(s$subjects, c("U1", NA_character_))
  expect_equal(s$study, c("S1", NA_character_))
})


test_that("summary.adam_dataset on a zero-row dataset has empty subjects and NA domain", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~STUDYID, ~DOMAIN, ~SAFFL,
          "U1",     "S1",  "ADSL",    "Y"
  )[integer(0), ])

  s <- summary(obj)

  expect_equal(s$subjects, character(0))
  expect_equal(s$study, character(0))
  expect_equal(s$domain, NA_character_)
  expect_equal(s$flags, "SAFFL")
})


# ---- Parameters --------------------------------------------------------------

test_that("summary.adam_dataset collects distinct PARAMCD and PARAM", {
  s <- summary(adam_dataset_adpc())

  expect_equal(
    as.data.frame(s$params),
    data.frame(
      PARAMCD = c("CMAX", "AUC"),
      PARAM = c("Cmax", "AUC"),
      stringsAsFactors = FALSE
    )
  )
})


test_that("summary.adam_dataset params are NULL when PARAM is absent", {
  s <- summary(adam_dataset_adsl())

  expect_null(s$params)
})


test_that("summary.adam_dataset params are NULL when only PARAMCD is present", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~PARAMCD, ~STUDYID,
          "U1",   "CMAX",     "S1"
  ))

  expect_null(summary(obj)$params)
})


test_that("summary.adam_dataset params include PARAM only when PARAMCD is missing", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~PARAM, ~STUDYID,
          "U1", "Cmax",     "S1",
          "U1",  "AUC",     "S1"
  ))

  expect_equal(
    as.data.frame(summary(obj)$params),
    data.frame(PARAM = c("Cmax", "AUC"), stringsAsFactors = FALSE)
  )
})


test_that("summary.adam_dataset deduplicates repeated PARAM rows", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~PARAMCD, ~PARAM, ~STUDYID,
          "U1",   "CMAX", "Cmax",     "S1",
          "U1",   "CMAX", "Cmax",     "S1",
          "U2",   "CMAX", "Cmax",     "S1"
  ))

  expect_equal(nrow(summary(obj)$params), 1L)
  expect_equal(summary(obj)$params$PARAMCD, "CMAX")
})


# ---- Flags -------------------------------------------------------------------

test_that("summary.adam_dataset lists columns whose names end in FL", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~FL, ~SAFFL, ~ANL01FL, ~AVAL, ~DTYPE, ~FLAG, ~STUDYID,
          "U1", "Y",    "Y",      "Y",     1,    "X",   "N",     "S1"
  ))

  expect_equal(summary(obj)$flags, c("FL", "SAFFL", "ANL01FL"))
})


test_that("summary.adam_dataset flags are empty when no FL columns exist", {
  obj <- new_dataset(tibble::tribble(
      ~USUBJID, ~STUDYID, ~DOMAIN,
          "U1",     "S1",  "ADSL"
  ))

  expect_equal(summary(obj)$flags, character(0))
})


test_that("summary.adam_dataset works on datasets retrieved with dataset()", {
  adam_obj <- adam(list(
    adpc = tibble::tribble(
      ~STUDYID, ~DOMAIN, ~USUBJID, ~PARAMCD, ~PARAM,
          "S1",  "ADPC",     "U1",   "CMAX", "Cmax"
    )
  ))

  s <- summary(dataset(adam_obj, "adpc"))

  expect_s3_class(s, "summary_dataset")
  expect_equal(s$domain, "ADPC")
  expect_equal(s$subjects, "U1")
  expect_equal(s$params$PARAMCD, "CMAX")
})
