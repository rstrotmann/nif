printed_lines <- function(x, ...) {
  capture.output(print(x, ...))
}


test_dm <- function() {
  tibble::tribble(
      ~USUBJID, ~DOMAIN, ~AGE,
    "SUBJ-001",    "DM",   25,
    "SUBJ-002",    "DM",   30
  )
}


test_dm_study <- function() {
  tibble::tribble(
    ~STUDYID, ~DOMAIN,    ~USUBJID, ~AGE,
        "S1",    "DM",  "SUBJ-001",   25,
        "S1",    "DM",  "SUBJ-002",   30
  )
}


test_pc_dtc <- function() {
  tibble::tribble(
    ~STUDYID, ~DOMAIN,    ~USUBJID,              ~PCDTC, ~PCSTRESN,
        "S1",    "PC",  "SUBJ-001", "2020-01-02T08:00",        10,
        "S1",    "PC",  "SUBJ-001", "2020-01-03T12:00",        20
  )
}


# ---- new_sdtm_domain() -------------------------------------------------------

test_that("new_sdtm_domain() returns a tibble with sdtm_domain class and metadata", {
  out <- as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = "Food effect study",
    studyid = "S1"
  )

  expect_s3_class(out, "sdtm_domain")
  expect_s3_class(out, "tbl_df")
  expect_equal(
    class(out),
    c("sdtm_domain", "tbl_df", "tbl", "data.frame")
  )
  expect_equal(attr(out, "name"), "dm")
  expect_equal(attr(out, "trial_title"), "Food effect study")
  expect_equal(attr(out, "studyid"), "S1")
  expect_equal(out, test_dm(), ignore_attr = TRUE)
})


test_that("new_sdtm_domain() defaults name, trial_title, and studyid to empty string", {
  out <- as_domain_test(test_dm())

  expect_equal(attr(out, "name"), "")
  expect_equal(attr(out, "trial_title"), "")
  expect_equal(attr(out, "studyid"), "")
})


test_that("new_sdtm_domain() wraps a 0-row data frame", {
  empty <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~AGE
  )
  out <- as_domain_test(empty, name = "dm")

  expect_s3_class(out, "sdtm_domain")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("USUBJID", "DOMAIN", "AGE"))
  expect_equal(attr(out, "name"), "dm")
})


test_that("new_sdtm_domain() wraps a plain data.frame as a tibble", {
  df <- as.data.frame(test_dm())
  out <- as_domain_test(df, name = "dm", studyid = "S1")

  expect_s3_class(out, "tbl_df")
  expect_equal(out, test_dm(), ignore_attr = TRUE)
})


# ---- sdtm_domain() -----------------------------------------------------------

test_that("sdtm_domain() returns a tibble with class and rows from the input", {
  out <- sdtm_domain(test_dm_study(), trial_title = "Food effect study")

  expect_s3_class(out, "sdtm_domain")
  expect_s3_class(out, "tbl_df")
  expect_equal(
    class(out),
    c("sdtm_domain", "tbl_df", "tbl", "data.frame")
  )
  expect_equal(out, test_dm_study(), ignore_attr = TRUE)
})


test_that("sdtm_domain() sets lowercase name from DOMAIN and studyid from STUDYID", {
  out <- sdtm_domain(test_dm_study(), trial_title = "Food effect study")

  expect_equal(attr(out, "name"), "dm")
  expect_equal(attr(out, "studyid"), "S1")
  expect_equal(attr(out, "trial_title"), "Food effect study")
})


test_that("sdtm_domain() lowercases mixed-case DOMAIN values", {
  data <- tibble::tribble(
      ~USUBJID, ~DOMAIN, ~AGE,
    "SUBJ-001",    "Dm",   25
  )
  out <- sdtm_domain(data)

  expect_equal(attr(out, "name"), "dm")
})


test_that("sdtm_domain() does not warn when DOMAIN values differ only by case", {
  data <- tibble::tribble(
      ~USUBJID, ~DOMAIN, ~AGE,
    "SUBJ-001",    "DM",   25,
    "SUBJ-002",    "dm",   30
  )

  expect_no_warning(out <- sdtm_domain(data))
  expect_equal(attr(out, "name"), "dm")
})


test_that("sdtm_domain() defaults trial_title to empty string", {
  out <- sdtm_domain(test_dm_study())

  expect_equal(attr(out, "trial_title"), "")
})


test_that("sdtm_domain() sets name to empty string when DOMAIN is missing", {
  data <- tibble::tribble(
      ~USUBJID, ~AGE,
    "SUBJ-001",   25,
    "SUBJ-002",   30
  )
  out <- sdtm_domain(data)

  expect_equal(attr(out, "name"), "")
  expect_equal(out, data, ignore_attr = TRUE)
})


test_that("sdtm_domain() sets studyid to empty string when STUDYID is missing", {
  out <- sdtm_domain(test_dm())

  expect_equal(attr(out, "studyid"), "")
  expect_no_warning(sdtm_domain(test_dm()))
})


test_that("sdtm_domain() warns and comma-collapses multiple DOMAIN values", {
  data <- tibble::tribble(
      ~USUBJID, ~DOMAIN, ~AGE,
    "SUBJ-001",    "DM",   25,
    "SUBJ-002",    "PC",   30
  )

  expect_warning(
    out <- sdtm_domain(data),
    "Multiple DOMAIN values in input!"
  )
  expect_equal(attr(out, "name"), "dm,pc")
})


test_that("sdtm_domain() warns and comma-collapses multiple STUDYID values", {
  data <- tibble::tribble(
    ~STUDYID, ~DOMAIN,    ~USUBJID, ~AGE,
        "S1",    "DM",  "SUBJ-001",   25,
        "S2",    "DM",  "SUBJ-002",   30
  )

  expect_warning(
    out <- sdtm_domain(data),
    "Multiple STUDYID values in input!"
  )
  expect_equal(attr(out, "studyid"), "S1,S2")
})


test_that("sdtm_domain() warns once each when DOMAIN and STUDYID both vary", {
  data <- tibble::tribble(
    ~STUDYID, ~DOMAIN,    ~USUBJID, ~AGE,
        "S1",    "DM",  "SUBJ-001",   25,
        "S2",    "PC",  "SUBJ-002",   30
  )

  warnings <- character()
  withCallingHandlers(
    out <- sdtm_domain(data),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(
    warnings,
    c("Multiple DOMAIN values in input!", "Multiple STUDYID values in input!")
  )
  expect_equal(attr(out, "name"), "dm,pc")
  expect_equal(attr(out, "studyid"), "S1,S2")
})


test_that("sdtm_domain() does not warn when DOMAIN and STUDYID are constant", {
  expect_no_warning(sdtm_domain(test_dm_study()))
})


test_that("sdtm_domain() wraps a 0-row table with DOMAIN and STUDYID columns", {
  empty <- tibble::tribble(
    ~STUDYID, ~DOMAIN, ~USUBJID, ~AGE
  )
  out <- sdtm_domain(empty, trial_title = "Food effect study")

  expect_s3_class(out, "sdtm_domain")
  expect_equal(nrow(out), 0)
  expect_equal(attr(out, "trial_title"), "Food effect study")
})


test_that("sdtm_domain() wraps an empty data frame with no columns", {
  out <- sdtm_domain(tibble::tibble())

  expect_s3_class(out, "sdtm_domain")
  expect_equal(nrow(out), 0)
  expect_equal(ncol(out), 0)
  expect_equal(attr(out, "name"), "")
  expect_equal(attr(out, "studyid"), "")
})


test_that("sdtm_domain() wraps a plain data.frame as a tibble", {
  df <- as.data.frame(test_dm_study())
  out <- sdtm_domain(df, trial_title = "Food effect study")

  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "name"), "dm")
  expect_equal(attr(out, "studyid"), "S1")
  expect_equal(out, test_dm_study(), ignore_attr = TRUE)
})


test_that("sdtm_domain() re-wraps an existing sdtm_domain from the columns", {
  inner <- as_domain_test(
    test_dm_study(),
    name = "pc",
    trial_title = "Old title",
    studyid = "OLD"
  )
  out <- sdtm_domain(inner, trial_title = "Food effect study")

  expect_s3_class(out, "sdtm_domain")
  expect_equal(attr(out, "name"), "dm")
  expect_equal(attr(out, "studyid"), "S1")
  expect_equal(attr(out, "trial_title"), "Food effect study")
})


test_that("sdtm_domain() rejects NULL and non-data-frame input", {
  expect_error(sdtm_domain(NULL), "data must not be NULL")
  expect_error(sdtm_domain(list(USUBJID = "SUBJ-001")), "data must be a data.frame")
  expect_error(sdtm_domain("DM"), "data must be a data.frame")
  expect_error(sdtm_domain(1:3), "data must be a data.frame")
})


# ---- print.sdtm_domain() -----------------------------------------------------

test_that("print.sdtm_domain prints domain, study, and title", {
  lines <- printed_lines(as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = "Food effect study",
    studyid = "S1"
  ))

  expect_match(lines[[1]], "SDTM domain", fixed = TRUE)
  expect_equal(lines[[2]], "Domain DM, study S1")
  expect_equal(lines[[3]], "Food effect study")
})


test_that("print.sdtm_domain skips an empty title", {
  lines <- printed_lines(as_domain_test(
    test_dm(),
    name = "pc",
    trial_title = "",
    studyid = "S1"
  ))

  expect_equal(lines[[2]], "Domain PC, study S1")
  expect_equal(lines[[3]], "")
  expect_false("Food effect study" %in% lines)
})


test_that("print.sdtm_domain skips a whitespace-only title", {
  lines <- printed_lines(as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = "   ",
    studyid = "S1"
  ))

  expect_equal(lines[[2]], "Domain DM, study S1")
  expect_equal(lines[[3]], "")
})


test_that("print.sdtm_domain skips a NULL title", {
  lines <- printed_lines(as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = NULL,
    studyid = "S1"
  ))

  expect_equal(lines[[2]], "Domain DM, study S1")
  expect_equal(lines[[3]], "")
})


test_that("print.sdtm_domain skips an NA title", {
  lines <- printed_lines(as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = NA_character_,
    studyid = "S1"
  ))

  expect_equal(lines[[2]], "Domain DM, study S1")
  expect_equal(lines[[3]], "")
})


test_that("print.sdtm_domain uppercases the domain name", {
  lines <- printed_lines(sdtm_domain(test_dm_study()))

  expect_equal(lines[[2]], "Domain DM, study S1")
})


test_that("print() dispatches to print.sdtm_domain", {
  obj <- sdtm_domain(test_dm_study(), trial_title = "Food effect study")
  lines <- printed_lines(obj)

  expect_match(lines[[1]], "SDTM domain", fixed = TRUE)
  expect_equal(lines[[2]], "Domain DM, study S1")
  expect_equal(lines[[3]], "Food effect study")
})


test_that("print.sdtm_domain returns the object invisibly", {
  obj <- sdtm_domain(test_dm_study())
  vis <- NULL
  capture.output({
    vis <- withVisible(print(obj))
  })

  expect_false(vis$visible)
  expect_identical(vis$value, obj)
})


# ---- dplyr reconstruct -------------------------------------------------------

test_that("dplyr filter, mutate, and select keep class and metadata", {
  obj <- as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = "Food effect study",
    studyid = "S1"
  )

  filtered <- dplyr::filter(obj, .data$USUBJID == "SUBJ-001")
  mutated <- dplyr::mutate(obj, AGE = .data$AGE + 1)
  selected <- dplyr::select(obj, "USUBJID")

  expect_s3_class(filtered, "sdtm_domain")
  expect_s3_class(mutated, "sdtm_domain")
  expect_s3_class(selected, "sdtm_domain")

  expect_equal(attr(filtered, "name"), "dm")
  expect_equal(attr(filtered, "trial_title"), "Food effect study")
  expect_equal(attr(filtered, "studyid"), "S1")
  expect_equal(attr(mutated, "name"), "dm")
  expect_equal(attr(selected, "name"), "dm")
  expect_equal(nrow(filtered), 1)
  expect_equal(mutated$AGE, c(26, 31))
})


test_that("dplyr arrange, rename, slice, and relocate keep class and metadata", {
  obj <- as_domain_test(
    test_dm_study(),
    name = "dm",
    trial_title = "Food effect study",
    studyid = "S1"
  )

  arranged <- dplyr::arrange(obj, dplyr::desc(.data$AGE))
  renamed <- dplyr::rename(obj, subject = "USUBJID")
  sliced <- dplyr::slice(obj, 1)
  relocated <- dplyr::relocate(obj, "AGE")

  expect_s3_class(arranged, "sdtm_domain")
  expect_s3_class(renamed, "sdtm_domain")
  expect_s3_class(sliced, "sdtm_domain")
  expect_s3_class(relocated, "sdtm_domain")

  expect_equal(attr(arranged, "name"), "dm")
  expect_equal(attr(renamed, "trial_title"), "Food effect study")
  expect_equal(attr(sliced, "studyid"), "S1")
  expect_equal(attr(relocated, "name"), "dm")
  expect_equal(arranged$AGE, c(30, 25))
  expect_equal(names(renamed)[names(renamed) == "subject"], "subject")
  expect_equal(nrow(sliced), 1)
  expect_equal(names(relocated)[[1]], "AGE")
})


test_that("dplyr verbs on sdtm_domain() output keep constructor metadata", {
  obj <- sdtm_domain(test_dm_study(), trial_title = "Food effect study")
  filtered <- dplyr::filter(obj, .data$AGE == 25)

  expect_s3_class(filtered, "sdtm_domain")
  expect_equal(attr(filtered, "name"), "dm")
  expect_equal(attr(filtered, "studyid"), "S1")
  expect_equal(attr(filtered, "trial_title"), "Food effect study")
  expect_equal(nrow(filtered), 1)
})


# ---- hash.sdtm_domain() ------------------------------------------------------

test_that("hash.sdtm_domain() ignores name, trial_title, and studyid", {
  obj <- as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = "Food effect study",
    studyid = "S1"
  )
  other <- as_domain_test(
    test_dm(),
    name = "pc",
    trial_title = "A different title",
    studyid = "S2"
  )

  expect_equal(hash(obj), hash(other))
})


test_that("hash.sdtm_domain() changes when rows change", {
  obj <- as_domain_test(test_dm(), name = "dm", studyid = "S1")
  other <- as_domain_test(
    tibble::tribble(
        ~USUBJID, ~DOMAIN, ~AGE,
      "SUBJ-001",    "DM",   25,
      "SUBJ-002",    "DM",   31
    ),
    name = "dm",
    studyid = "S1"
  )

  expect_false(identical(hash(obj), hash(other)))
})


test_that("hash.sdtm_domain() changes when column values or names change", {
  obj <- as_domain_test(test_dm(), name = "dm")
  extra_col <- as_domain_test(
    tibble::tribble(
        ~USUBJID, ~DOMAIN, ~AGE, ~SEX,
      "SUBJ-001",    "DM",   25,  "M",
      "SUBJ-002",    "DM",   30,  "F"
    ),
    name = "dm"
  )
  reordered <- as_domain_test(
    tibble::tribble(
      ~AGE, ~DOMAIN,    ~USUBJID,
        25,    "DM",  "SUBJ-001",
        30,    "DM",  "SUBJ-002"
    ),
    name = "dm"
  )

  expect_false(identical(hash(obj), hash(extra_col)))
  expect_false(identical(hash(obj), hash(reordered)))
})


test_that("hash() dispatches to hash.sdtm_domain and returns a string", {
  obj <- sdtm_domain(test_dm_study())

  expect_type(hash(obj), "character")
  expect_equal(length(hash(obj)), 1)
  expect_equal(hash(obj), hash.sdtm_domain(obj))
})


test_that("hash.sdtm_domain() matches sdtm_domain() and new_sdtm_domain() on the same rows", {
  from_public <- sdtm_domain(test_dm_study(), trial_title = "Food effect study")
  from_new <- as_domain_test(
    test_dm_study(),
    name = "other",
    trial_title = "Other",
    studyid = "X"
  )

  expect_equal(hash(from_public), hash(from_new))
})


# ---- last_dtc.sdtm_domain() --------------------------------------------------

test_that("last_dtc.sdtm_domain() returns the latest datetime in the domain", {
  obj <- sdtm_domain(test_pc_dtc())
  out <- last_dtc(obj)

  expect_s3_class(out, "POSIXct")
  expect_equal(length(out), 1)
  expect_equal(as.Date(out), as.Date("2020-01-03"))
})


test_that("last_dtc.sdtm_domain() returns NULL when there is no datetime column", {
  obj <- sdtm_domain(test_dm_study())

  expect_null(last_dtc(obj))
})


test_that("last_dtc() dispatches to last_dtc.sdtm_domain", {
  obj <- sdtm_domain(test_pc_dtc())

  expect_equal(last_dtc(obj), last_dtc.sdtm_domain(obj))
})


test_that("last_dtc.sdtm_domain() errors when DOMAIN is missing", {
  obj <- as_domain_test(
    tibble::tribble(
        ~USUBJID, ~AGE,
      "SUBJ-001",   25
    ),
    name = "dm"
  )

  expect_error(
    last_dtc(obj),
    "The data frame must have a DOMAIN column"
  )
})
