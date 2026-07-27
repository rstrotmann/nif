## Tests for validate_imputation_set

valid_imputation_set <- function() {
  list(
    admin_pre_expansion = function(
        ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
      ) {
      ex
    },
    admin_post_expansion = function(
        ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
      ) {
      ex
    },
    obs_raw = function(obs, silent) {
      obs
    },
    obs_final = function(obs, silent) {
      obs
    }
  )
}


test_that("validate_imputation_set accepts exported rule sets", {
  expect_invisible(validate_imputation_set(imputation_rules_standard))
  expect_invisible(validate_imputation_set(imputation_rules_minimal))
  expect_invisible(validate_imputation_set(imputation_rules_1))
})


test_that("validate_imputation_set accepts a custom list with all required fields", {
  expect_invisible(validate_imputation_set(valid_imputation_set()))
})


test_that("validate_imputation_set accepts extra fields beyond the required slots", {
  rules <- valid_imputation_set()
  rules$custom_step <- function(x) x

  expect_invisible(validate_imputation_set(rules))
})


test_that("validate_imputation_set accepts non-function slot values", {
  rules <- list(
    admin_pre_expansion = NULL,
    admin_post_expansion = 1,
    obs_raw = "noop",
    obs_final = TRUE
  )

  expect_invisible(validate_imputation_set(rules))
})


test_that("validate_imputation_set rejects non-list input", {
  expect_error(
    validate_imputation_set(NULL),
    "Imputation rule set must be a list!"
  )
  expect_error(
    validate_imputation_set("imputation_rules_standard"),
    "Imputation rule set must be a list!"
  )
  expect_error(
    validate_imputation_set(c(
      "admin_pre_expansion",
      "admin_post_expansion",
      "obs_raw",
      "obs_final"
    )),
    "Imputation rule set must be a list!"
  )
  expect_error(
    validate_imputation_set(1),
    "Imputation rule set must be a list!"
  )
  expect_error(
    validate_imputation_set(function(x) x),
    "Imputation rule set must be a list!"
  )
})


test_that("validate_imputation_set rejects data frames missing required slots", {
  expect_error(
    validate_imputation_set(data.frame(slot = "admin_pre_expansion")),
    "Missing fields in imputation rule set:"
  )
  expect_error(
    validate_imputation_set(data.frame(slot = "admin_pre_expansion")),
    "admin_pre_expansion"
  )
})


test_that("validate_imputation_set rejects empty lists", {
  expect_error(
    validate_imputation_set(list()),
    paste0(
      "Missing fields in imputation rule set: ",
      "admin_pre_expansion, admin_post_expansion, obs_raw and obs_final"
    )
  )
  expect_error(
    validate_imputation_set(imputation_rules_void),
    paste0(
      "Missing fields in imputation rule set: ",
      "admin_pre_expansion, admin_post_expansion, obs_raw and obs_final"
    )
  )
})


test_that("validate_imputation_set rejects a single missing field", {
  rules <- valid_imputation_set()
  rules$admin_post_expansion <- NULL
  rules <- rules[names(rules) != "admin_post_expansion"]

  expect_error(
    validate_imputation_set(rules),
    "Missing field in imputation rule set: admin_post_expansion"
  )
})


test_that("validate_imputation_set rejects each required field individually", {
  for (field in c(
    "admin_pre_expansion",
    "admin_post_expansion",
    "obs_raw",
    "obs_final"
  )) {
    rules <- valid_imputation_set()
    rules[[field]] <- NULL
    rules <- rules[names(rules) != field]

    expect_error(
      validate_imputation_set(rules),
      paste0("Missing field in imputation rule set: ", field)
    )
  }
})


test_that("validate_imputation_set rejects multiple missing fields", {
  rules <- list(
    admin_pre_expansion = function(
        ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
      ) {
      ex
    },
    obs_final = function(obs, silent) {
      obs
    }
  )

  expect_error(
    validate_imputation_set(rules),
    paste0(
      "Missing fields in imputation rule set: ",
      "admin_post_expansion and obs_raw"
    )
  )
})


test_that("validate_imputation_set rejects lists with unrelated names only", {
  rules <- list(
    step_one = function(x) x,
    step_two = function(x) x,
    step_three = function(x) x,
    step_four = function(x) x
  )

  expect_error(
    validate_imputation_set(rules),
    "Missing fields in imputation rule set:"
  )
  expect_error(
    validate_imputation_set(rules),
    "admin_pre_expansion"
  )
  expect_error(
    validate_imputation_set(rules),
    "admin_post_expansion"
  )
  expect_error(
    validate_imputation_set(rules),
    "obs_raw"
  )
  expect_error(
    validate_imputation_set(rules),
    "obs_final"
  )
})


test_that("validate_imputation_set error uses singular field for one omission", {
  rules <- list(
    admin_pre_expansion = identity,
    admin_post_expansion = identity,
    obs_raw = identity
  )

  expect_error(
    validate_imputation_set(rules),
    "Missing field in imputation rule set:"
  )
  expect_error(
    validate_imputation_set(rules),
    "obs_final"
  )
})


test_that("validate_imputation_set error uses plural fields for multiple omissions", {
  rules <- list(
    admin_pre_expansion = identity,
    obs_final = identity
  )

  expect_error(
    validate_imputation_set(rules),
    "Missing fields in imputation rule set:"
  )
  expect_error(
    validate_imputation_set(rules),
    "admin_post_expansion"
  )
  expect_error(
    validate_imputation_set(rules),
    "obs_raw"
  )
})
