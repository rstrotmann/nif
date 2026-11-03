# Tests for validate_domain_param()

test_that("validate_domain_param passes for a sdtm_domain object", {
  dm <- sdtm_domain(
    tibble::tribble(
      ~DOMAIN, ~STUDYID, ~USUBJID,
      "DM",    "STUDY1", "SUBJ-001",
      "DM",    "STUDY1", "SUBJ-002"
    )
  )

  expect_invisible(validate_domain_param(dm))
  expect_null(validate_domain_param(dm))
})


test_that("validate_domain_param passes for a domain from a sdtm object", {
  dm <- domain(examplinib_sad, "dm")

  expect_s3_class(dm, "sdtm_domain")
  expect_no_error(validate_domain_param(dm))
})


test_that("validate_domain_param passes for an empty sdtm_domain", {
  empty <- sdtm_domain(
    tibble::tribble(
      ~DOMAIN, ~USUBJID
    )
  )

  expect_no_error(validate_domain_param(empty))
})


test_that("validate_domain_param rejects a plain data frame", {
  df <- data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")
  tbl <- tibble::tribble(
    ~DOMAIN, ~USUBJID,
    "DM",    "SUBJ-001"
  )

  expect_error(validate_domain_param(df), "Input must be a sdtm_domain object")
  expect_error(validate_domain_param(tbl), "Input must be a sdtm_domain object")
})


test_that("validate_domain_param rejects other object types", {
  expect_error(validate_domain_param(NULL), "Input must be a sdtm_domain object")
  expect_error(validate_domain_param("dm"), "Input must be a sdtm_domain object")
  expect_error(validate_domain_param(1), "Input must be a sdtm_domain object")
  expect_error(
    validate_domain_param(list(DOMAIN = "DM")),
    "Input must be a sdtm_domain object"
  )
  expect_error(
    validate_domain_param(examplinib_sad),
    "Input must be a sdtm_domain object"
  )
})


test_that("validate_domain_param signals sdtm_arg_error", {
  expect_error(validate_domain_param(NULL), class = "sdtm_arg_error")
  expect_error(
    validate_domain_param(data.frame(DOMAIN = "DM")),
    class = "sdtm_arg_error"
  )
  expect_error(validate_domain_param(examplinib_sad), class = "sdtm_arg_error")
})


test_that("validate_domain_param reports error in the calling function", {
  check_input <- function(x) validate_domain_param(x)

  err <- expect_error(check_input(1), class = "sdtm_arg_error")
  expect_equal(rlang::call_name(err$call), "check_input")

  explicit_call <- function(x) {
    validate_domain_param(x, call = rlang::current_env())
  }
  err <- expect_error(explicit_call(1), class = "sdtm_arg_error")
  expect_equal(rlang::call_name(err$call), "explicit_call")
})


test_that("plot.sdtm_domain rejects non-domain input via validate_domain_param", {
  expect_error(
    plot.sdtm_domain(data.frame(DOMAIN = "DM")),
    class = "sdtm_arg_error"
  )
})
