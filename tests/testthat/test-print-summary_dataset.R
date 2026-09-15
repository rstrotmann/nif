as_summary_dataset <- function(
    domain = "ADPC",
    study = "S1",
    subjects = c("U1", "U2"),
    flags = c("SAFFL", "FASFL"),
    params = NULL
) {
  structure(
    list(
      data = NULL,
      domain = domain,
      subjects = subjects,
      study = study,
      flags = flags,
      params = params
    ),
    class = "summary_dataset"
  )
}


printed_lines <- function(x, ...) {
  capture.output(print(x, ...))
}


default_params <- function() {
  tibble::tribble(
    ~PARAMCD, ~PARAM,
      "CMAX", "Cmax",
       "AUC",  "AUC"
  )
}


# ---- Header and identity block -----------------------------------------------

test_that("print.summary_dataset prints the ADaM dataset summary header", {
  lines <- printed_lines(as_summary_dataset())

  expect_match(lines[[1]], "ADaM dataset summary", fixed = TRUE)
})


test_that("print.summary_dataset reports domain, study, and subject count", {
  lines <- printed_lines(as_summary_dataset())

  expect_true("Domain: ADPC" %in% lines)
  expect_true("Study: S1" %in% lines)
  expect_true("2 subjects" %in% lines)
})


test_that("print.summary_dataset uses length(subjects) even when some IDs are NA", {
  lines <- printed_lines(as_summary_dataset(
    subjects = c("U1", NA_character_),
    flags = "SAFFL"
  ))

  expect_true("2 subjects" %in% lines)
})


test_that("print.summary_dataset reports 1 subject without pluralizing", {
  lines <- printed_lines(as_summary_dataset(
    subjects = "U1",
    flags = "SAFFL"
  ))

  expect_true("1 subjects" %in% lines)
})


test_that("print.summary_dataset reports 0 subjects when subjects is empty or NULL", {
  empty <- printed_lines(as_summary_dataset(
    subjects = character(0),
    flags = "SAFFL"
  ))
  missing <- printed_lines(as_summary_dataset(
    subjects = NULL,
    flags = "SAFFL"
  ))

  expect_true("0 subjects" %in% empty)
  expect_true("0 subjects" %in% missing)
})


test_that("print.summary_dataset shows Domain: NULL when domain is the string NULL", {
  lines <- printed_lines(as_summary_dataset(
    domain = "NULL",
    flags = "SAFFL"
  ))

  expect_true("Domain: NULL" %in% lines)
})


test_that("print.summary_dataset leaves Domain empty when domain is R NULL", {
  lines <- printed_lines(as_summary_dataset(
    domain = NULL,
    flags = "SAFFL"
  ))

  expect_true("Domain: " %in% lines)
  expect_false(any(grepl("Domain: NULL", lines)))
})


test_that("print.summary_dataset shows Domain: NA when domain is missing", {
  lines <- printed_lines(as_summary_dataset(
    domain = NA_character_,
    flags = "SAFFL"
  ))

  expect_true("Domain: NA" %in% lines)
})


test_that("print.summary_dataset leaves Study empty when study is NULL", {
  lines <- printed_lines(as_summary_dataset(
    study = NULL,
    flags = "SAFFL"
  ))

  expect_true("Study: " %in% lines)
})


test_that("print.summary_dataset shows only the first study when study has multiple values", {
  lines <- printed_lines(as_summary_dataset(
    study = c("S1", "S2"),
    flags = "SAFFL"
  ))

  expect_true("Study: S1" %in% lines)
  expect_false(any(grepl("Study: S2", lines)))
})


# ---- Parameters --------------------------------------------------------------

test_that("print.summary_dataset prints Parameters as an indented table", {
  lines <- printed_lines(as_summary_dataset(params = default_params()))

  expect_true("Parameters:" %in% lines)
  expect_true(any(grepl("PARAMCD", lines)))
  expect_true(any(grepl("PARAM", lines)))
  expect_true(any(grepl("CMAX", lines)))
  expect_true(any(grepl("Cmax", lines)))
  expect_true(any(grepl("AUC", lines)))
})


test_that("print.summary_dataset omits the Parameters section when params is NULL", {
  lines <- printed_lines(as_summary_dataset(params = NULL, flags = "SAFFL"))

  expect_false(any(grepl("Parameters:", lines)))
})


test_that("print.summary_dataset still prints a Parameters heading for an empty params data frame", {
  lines <- printed_lines(as_summary_dataset(
    params = tibble::tribble(
      ~PARAMCD, ~PARAM
    ),
    flags = "SAFFL"
  ))

  expect_true("Parameters:" %in% lines)
})


test_that("print.summary_dataset prints PARAM-only params tables", {
  lines <- printed_lines(as_summary_dataset(
    params = tibble::tribble(
      ~PARAM,
      "Cmax",
       "AUC"
    ),
    flags = "SAFFL"
  ))

  expect_true("Parameters:" %in% lines)
  expect_true(any(grepl("Cmax", lines)))
  expect_true(any(grepl("AUC", lines)))
  expect_false(any(grepl("PARAMCD", lines)))
})


# ---- Flags -------------------------------------------------------------------

test_that("print.summary_dataset enumerates a single flag", {
  lines <- printed_lines(as_summary_dataset(
    params = NULL,
    flags = "SAFFL"
  ))

  expect_true("Flags:" %in% lines)
  expect_true("  SAFFL" %in% lines)
})


test_that("print.summary_dataset enumerates two flags with and", {
  lines <- printed_lines(as_summary_dataset(
    params = NULL,
    flags = c("SAFFL", "FASFL")
  ))

  expect_true("  SAFFL and FASFL" %in% lines)
})


test_that("print.summary_dataset enumerates three flags with commas and and", {
  lines <- printed_lines(as_summary_dataset(
    params = NULL,
    flags = c("SAFFL", "FASFL", "ITTFL")
  ))

  expect_true("  SAFFL, FASFL and ITTFL" %in% lines)
})


test_that("print.summary_dataset keeps a Flags heading when flags is empty or NULL", {
  empty <- printed_lines(as_summary_dataset(
    params = NULL,
    flags = character(0)
  ))
  missing <- printed_lines(as_summary_dataset(
    params = NULL,
    flags = NULL
  ))

  expect_true("Flags:" %in% empty)
  expect_true("Flags:" %in% missing)
  expect_false(any(grepl("SAFFL", empty)))
  expect_false(any(grepl("SAFFL", missing)))
})


# ---- Layout, return value, and integration -----------------------------------

test_that("print.summary_dataset separates sections with a blank line", {
  lines <- printed_lines(as_summary_dataset(params = default_params()))

  expect_true("" %in% lines)
  domain_idx <- match("Domain: ADPC", lines)
  flags_idx <- match("Flags:", lines)
  expect_equal(lines[[domain_idx + 3]], "")
  expect_true(flags_idx > domain_idx)
})


test_that("print.summary_dataset returns the message list invisibly", {
  x <- as_summary_dataset(params = NULL, flags = "SAFFL")
  result <- NULL
  capture.output(result <- print(x))

  expect_type(result, "list")
  expect_length(result, 3L)
  expect_equal(result[[1]], "Domain: ADPC\nStudy: S1\n2 subjects")
  expect_null(result[[2]])
  expect_equal(result[[3]], "Flags:\n  SAFFL")
})


test_that("print.summary_dataset ignores unused dots", {
  expect_no_error(
    capture.output(print(as_summary_dataset(params = NULL, flags = "SAFFL"), extra = TRUE))
  )
})


test_that("print.summary_dataset works with objects from summary.adam_dataset", {
  obj <- as_adam_dataset_test(tibble::tribble(
    ~STUDYID, ~DOMAIN,   ~USUBJID, ~PARAMCD, ~PARAM, ~SAFFL, ~FASFL,
        "S1",  "ADPC", "SUBJ-001",   "CMAX", "Cmax",    "Y",    "Y",
        "S1",  "ADPC", "SUBJ-002",    "AUC",  "AUC",    "Y",    "N"
  ))
  lines <- printed_lines(summary(obj))

  expect_match(lines[[1]], "ADaM dataset summary")
  expect_true("Domain: ADPC" %in% lines)
  expect_true("Study: S1" %in% lines)
  expect_true("2 subjects" %in% lines)
  expect_true("Parameters:" %in% lines)
  expect_true(any(grepl("CMAX", lines)))
  expect_true("  SAFFL and FASFL" %in% lines)
})
