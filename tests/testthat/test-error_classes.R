# Tests that validation functions signal errors with the expected classes.
# Condition classes: nif_arg_error, sdtm_arg_error, adam_arg_error,
# nif_imputation_error.

test_nif <- function() {
  as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     0,   "A",
    1,   1,     0,    2,    0,     10,  "A"
  ))
}


test_sdtm <- function() {
  sdtm(list(
    pc = tibble::tribble(
      ~USUBJID,   ~DOMAIN, ~PCTESTCD, ~PCTEST,         ~PCSTRESN,
      "SUBJ-001", "PC",    "RS2023",  "Concentration", 10.5,
      "SUBJ-002", "PC",    "RS2024",  "Concentration", 12.3
    ),
    dm = tibble::tribble(
      ~USUBJID,   ~DOMAIN, ~AGE,
      "SUBJ-001", "DM",    25,
      "SUBJ-002", "DM",    30
    )
  ))
}


test_that("validate_argument signals nif_arg_error", {
  expect_error(validate_argument(NULL, "character"), class = "nif_arg_error")
  expect_error(validate_argument(1, "character"), class = "nif_arg_error")
  expect_error(validate_argument(NA, "logical"), class = "nif_arg_error")
  expect_error(
    validate_argument(c("a", "b"), "character"),
    class = "nif_arg_error"
  )
  expect_error(validate_argument("", "character"), class = "nif_arg_error")
  expect_error(
    validate_argument("c", "character", values = c("a", "b")),
    class = "nif_arg_error"
  )
})


test_that("check_df_columns signals nif_arg_error", {
  df <- tibble::tribble(
    ~a, ~b,
    1,  "x"
  )

  expect_error(check_df_columns(df, "z"), class = "nif_arg_error")
  expect_invisible(check_df_columns(df, c("a", "b")))
})


test_that("validate_df_argument signals nif_arg_error for missing columns", {
  df <- tibble::tribble(
    ~a, ~b,
    1,  "x"
  )

  expect_error(
    validate_df_argument(df, expected_fields = "z"),
    class = "nif_arg_error"
  )
})


test_that("validate_fields signals nif_arg_error", {
  df <- tibble::tribble(
    ~a, ~b,
    1,  "x"
  )

  expect_error(validate_fields(df, "z"), class = "nif_arg_error")
  expect_error(validate_fields(df, 1), class = "nif_arg_error")
})


test_that("validate_nif_argument signals nif_arg_error", {
  # not a nif object
  expect_error(
    validate_nif_argument(data.frame(ID = 1)),
    class = "nif_arg_error"
  )
  expect_error(validate_nif_argument(NULL), class = "nif_arg_error")

  # missing essential fields
  incomplete <- as_nif_test(tibble::tribble(
    ~ID,
    1
  ))
  expect_error(validate_nif_argument(incomplete), class = "nif_arg_error")

  # missing custom field
  expect_error(
    validate_nif_argument(test_nif(), fields = "NONEXISTENT"),
    class = "nif_arg_error"
  )

  # invalid fields argument
  expect_error(
    validate_nif_argument(test_nif(), fields = 1),
    class = "nif_arg_error"
  )
})


test_that("validate_analyte signals nif_arg_error", {
  expect_error(
    validate_analyte(test_nif(), "UNKNOWN"),
    class = "nif_arg_error"
  )
  expect_error(
    validate_analyte(test_nif(), c("A", "UNKNOWN")),
    class = "nif_arg_error"
  )
  expect_error(validate_analyte(test_nif(), 1), class = "nif_arg_error")
  expect_error(
    validate_analyte(data.frame(ID = 1), "A"),
    class = "nif_arg_error"
  )
  expect_silent(validate_analyte(test_nif(), "A"))
})


test_that("validate_domain signals sdtm_arg_error for multiple domains", {
  domain <- tibble::tribble(
    ~USUBJID,   ~DOMAIN,
    "SUBJ-001", "DM",
    "SUBJ-002", "PC"
  )

  expect_error(validate_domain(domain), class = "sdtm_arg_error")
})


test_that("validate_sdtm_argument signals sdtm_arg_error", {
  # not a sdtm object
  expect_error(
    validate_sdtm_argument(data.frame(a = 1)),
    class = "sdtm_arg_error"
  )
  expect_error(validate_sdtm_argument(NULL), class = "sdtm_arg_error")

  # expected domain missing
  expect_error(
    validate_sdtm_argument(test_sdtm(), "lb"),
    class = "sdtm_arg_error"
  )
  expect_error(
    validate_sdtm_argument(test_sdtm(), c("pc", "lb", "ae")),
    class = "sdtm_arg_error"
  )

  # invalid expected_domains argument is reported as nif_arg_error
  expect_error(
    validate_sdtm_argument(test_sdtm(), 1),
    class = "nif_arg_error"
  )

  expect_silent(validate_sdtm_argument(test_sdtm(), c("pc", "dm")))
})


test_that("validate_testcd signals sdtm_arg_error", {
  # invalid sdtm object
  expect_error(validate_testcd("not_sdtm", "RS2023"), class = "sdtm_arg_error")

  # domain not in sdtm
  expect_error(
    validate_testcd(test_sdtm(), "RS2023", "lb"),
    class = "sdtm_arg_error"
  )

  # domain without TESTCD field
  expect_error(
    validate_testcd(test_sdtm(), "AGE", "dm"),
    class = "sdtm_arg_error"
  )

  # testcd missing in domain
  expect_error(
    validate_testcd(test_sdtm(), "RS2099", "pc"),
    class = "sdtm_arg_error"
  )

  # testcd missing in sdtm
  expect_error(
    validate_testcd(test_sdtm(), "NONEXISTENT"),
    class = "sdtm_arg_error"
  )

  # invalid testcd argument is reported as nif_arg_error
  expect_error(validate_testcd(test_sdtm(), 123), class = "nif_arg_error")
})


test_that("validate_dataset signals adam_arg_error", {
  expect_error(validate_dataset(data.frame(a = 1)), class = "adam_arg_error")
  expect_error(validate_dataset(NULL), class = "adam_arg_error")

  adam <- new_adam_dataset(
    tibble::tribble(
      ~USUBJID,   ~AGE,
      "SUBJ-001", 25
    )
  )
  expect_silent(validate_dataset(adam))
})


test_that("validate_imputation_set signals nif_imputation_error", {
  # not a list
  expect_error(
    validate_imputation_set("standard"),
    class = "nif_imputation_error"
  )
  expect_error(
    validate_imputation_set(data.frame(a = 1)),
    class = "nif_imputation_error"
  )

  # unnamed elements
  expect_error(
    validate_imputation_set(list(function(obs, silent) obs)),
    class = "nif_imputation_error"
  )

  # unknown slot
  expect_error(
    validate_imputation_set(list(unknown_slot = function(obs, silent) obs)),
    class = "nif_imputation_error"
  )

  # slot is not a function
  expect_error(
    validate_imputation_set(list(obs_final = "not a function")),
    class = "nif_imputation_error"
  )

  # function lacks the 'silent' argument
  expect_error(
    validate_imputation_set(list(obs_final = function(obs) obs)),
    class = "nif_imputation_error"
  )
})


test_that("error classes inherit from rlang_error", {
  err <- expect_error(
    validate_sdtm_argument(NULL),
    class = "sdtm_arg_error"
  )
  expect_s3_class(err, "rlang_error")

  err <- expect_error(
    validate_dataset(NULL),
    class = "adam_arg_error"
  )
  expect_s3_class(err, "rlang_error")
})
