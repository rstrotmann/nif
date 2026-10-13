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
  expect_invisible(validate_imputation_set(imputation_rules_void))
})


test_that("validate_imputation_set accepts a custom list with all slots", {
  expect_invisible(validate_imputation_set(valid_imputation_set()))
})


test_that("validate_imputation_set accepts an empty list", {
  expect_invisible(validate_imputation_set(list()))
})


test_that("validate_imputation_set accepts a partial rule set", {
  rules <- valid_imputation_set()["obs_final"]
  expect_invisible(validate_imputation_set(rules))

  rules <- valid_imputation_set()[c("admin_pre_expansion", "obs_raw")]
  expect_invisible(validate_imputation_set(rules))
})


test_that("validate_imputation_set accepts slot functions with ...", {
  rules <- list(
    admin_pre_expansion = function(ex, ...) ex,
    admin_post_expansion = function(...) NULL,
    obs_raw = function(obs, ...) obs,
    obs_final = function(...) NULL
  )

  expect_invisible(validate_imputation_set(rules))
})


test_that("validate_imputation_set accepts silent with a default value", {
  rules <- list(
    obs_raw = function(obs, silent = NULL) obs
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


test_that("validate_imputation_set rejects data frames", {
  rules <- tibble::tribble(
    ~obs_raw, ~obs_final,
    1,        2
  )

  expect_error(
    validate_imputation_set(rules),
    "Imputation rule set must be a list!"
  )
})


test_that("validate_imputation_set rejects unnamed elements", {
  expect_error(
    validate_imputation_set(list(function(obs, silent) obs)),
    "All elements of an imputation rule set must be named!"
  )

  rules <- c(
    valid_imputation_set()["obs_raw"],
    list(function(obs, silent) obs)
  )
  expect_error(
    validate_imputation_set(rules),
    "All elements of an imputation rule set must be named!"
  )
})


test_that("validate_imputation_set rejects an unknown slot", {
  rules <- valid_imputation_set()
  rules$custom_step <- function(x) x

  expect_error(
    validate_imputation_set(rules),
    "Unknown slot in imputation rule set: custom_step",
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects a misspelled slot", {
  rules <- list(
    obs_fianl = function(obs, silent) obs
  )

  expect_error(
    validate_imputation_set(rules),
    "Unknown slot in imputation rule set: obs_fianl",
    fixed = TRUE
  )
})


test_that("validate_imputation_set lists all unknown slots and allowed names", {
  rules <- list(
    step_one = function(x) x,
    step_two = function(x) x
  )

  expect_error(
    validate_imputation_set(rules),
    "Unknown slots in imputation rule set: step_one and step_two",
    fixed = TRUE
  )
  expect_error(
    validate_imputation_set(rules),
    paste0(
      "Allowed: admin_pre_expansion, admin_post_expansion, ",
      "obs_raw and obs_final"
    ),
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects non-function slot values", {
  expect_error(
    validate_imputation_set(list(obs_raw = "noop")),
    "'obs_raw' must be a function",
    fixed = TRUE
  )
  expect_error(
    validate_imputation_set(list(admin_post_expansion = 1)),
    "'admin_post_expansion' must be a function",
    fixed = TRUE
  )
  expect_error(
    validate_imputation_set(list(obs_final = TRUE)),
    "'obs_final' must be a function",
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects NULL slot values", {
  rules <- list(obs_raw = NULL)

  expect_error(
    validate_imputation_set(rules),
    "'obs_raw' must be a function",
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects obs slots without silent", {
  rules <- list(obs_raw = function(obs) obs)

  expect_error(
    validate_imputation_set(rules),
    "'obs_raw' must accept 1 positional argument and the 'silent' argument",
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects obs slots without positional args", {
  rules <- list(obs_final = function(silent) NULL)

  expect_error(
    validate_imputation_set(rules),
    "'obs_final' must accept 1 positional argument and the 'silent' argument",
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects identity as a slot function", {
  rules <- list(obs_final = identity)

  expect_error(
    validate_imputation_set(rules),
    "'obs_final' must accept 1 positional argument and the 'silent' argument",
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects admin slots with too few arguments", {
  rules <- list(
    admin_pre_expansion = function(ex, sdtm, extrt, silent) ex
  )

  expect_error(
    validate_imputation_set(rules),
    paste0(
      "'admin_pre_expansion' must accept 6 positional arguments ",
      "and the 'silent' argument"
    ),
    fixed = TRUE
  )
})


test_that("validate_imputation_set rejects admin slots without silent", {
  rules <- list(
    admin_post_expansion = function(
        ex, sdtm, extrt, analyte, pctestcd, cut_off_date
      ) {
      ex
    }
  )

  expect_error(
    validate_imputation_set(rules),
    paste0(
      "'admin_post_expansion' must accept 6 positional arguments ",
      "and the 'silent' argument"
    ),
    fixed = TRUE
  )
})


test_that("validate_imputation_set reports all invalid slots in one error", {
  rules <- list(
    admin_pre_expansion = "noop",
    obs_raw = function(obs) obs
  )

  err <- expect_error(validate_imputation_set(rules))
  expect_match(err$message, "Invalid imputation rule set: ", fixed = TRUE)
  expect_match(
    err$message, "'admin_pre_expansion' must be a function", fixed = TRUE
  )
  expect_match(
    err$message,
    "'obs_raw' must accept 1 positional argument and the 'silent' argument",
    fixed = TRUE
  )
})


test_that("resolve_imputation_rules validates list input", {
  expect_error(
    nif:::resolve_imputation_rules(list(obs_fianl = function(obs, silent) obs)),
    "Unknown slot in imputation rule set: obs_fianl",
    fixed = TRUE
  )
})


test_that("nif() rejects an invalid imputation_rules list", {
  expect_error(
    nif(imputation_rules = list(obs_raw = "noop"), silent = TRUE),
    "'obs_raw' must be a function",
    fixed = TRUE
  )
})
