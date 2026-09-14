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


test_that("new_domain() returns a tibble with sdtm_domain class and metadata", {
  out <- as_domain_test(
    test_dm(),
    name = "dm",
    trial_title = "Food effect study",
    studyid = "S1"
  )

  expect_s3_class(out, "sdtm_domain")
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "name"), "dm")
  expect_equal(attr(out, "trial_title"), "Food effect study")
  expect_equal(attr(out, "studyid"), "S1")
  expect_equal(out, test_dm(), ignore_attr = TRUE)
})


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
})


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
