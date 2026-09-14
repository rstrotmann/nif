sdtm_from <- function(...) {
  sdtm(list(...))
}


test_dm <- function() {
  tibble::tribble(
      ~USUBJID, ~DOMAIN, ~AGE,
    "SUBJ-001",    "DM",   25,
    "SUBJ-002",    "DM",   30
  )
}


test_pc <- function() {
  tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCSTRESN,
    "SUBJ-001",    "PC",      10.5,
    "SUBJ-001",    "PC",      15.2,
    "SUBJ-002",    "PC",      12.3
  )
}


test_that("domain() returns an sdtm_domain tibble with the stored rows", {
  obj <- sdtm_from(dm = test_dm(), pc = test_pc())
  dm <- domain(obj, "dm")
  pc <- domain(obj, "pc")

  expect_s3_class(dm, "sdtm_domain")
  expect_s3_class(dm, "tbl_df")
  expect_s3_class(pc, "sdtm_domain")
  expect_equal(dm, test_dm(), ignore_attr = TRUE)
  expect_equal(pc, test_pc(), ignore_attr = TRUE)
})


test_that("domain() attaches name, studyid, and trial_title", {
  obj <- sdtm_from(
    dm = tibble::tribble(
      ~STUDYID, ~DOMAIN,    ~USUBJID,
          "S1",    "DM",  "SUBJ-001"
    ),
    ts = tibble::tribble(
      ~STUDYID, ~DOMAIN, ~TSPARMCD,            ~TSVAL,
          "S1",    "TS",   "TITLE", "Food effect study"
    )
  )
  dm <- domain(obj, "dm")

  expect_equal(attr(dm, "name"), "dm")
  expect_equal(attr(dm, "studyid"), "S1")
  expect_equal(attr(dm, "trial_title"), "Food effect study")
})


test_that("domain() sets studyid to empty string when STUDYID is missing", {
  obj <- sdtm_from(dm = test_dm())
  dm <- domain(obj, "dm")

  expect_equal(attr(dm, "studyid"), "")
  expect_no_warning(domain(obj, "dm"))
})


test_that("domain() comma-collapses multiple STUDYID values", {
  obj <- sdtm_from(
    dm = tibble::tribble(
      ~STUDYID, ~DOMAIN,    ~USUBJID,
          "S1",    "DM",  "SUBJ-001",
          "S2",    "DM",  "SUBJ-002"
    )
  )

  expect_equal(attr(domain(obj, "dm"), "studyid"), "S1,S2")
})


test_that("domain() stores NULL trial_title when TS is absent", {
  obj <- sdtm_from(dm = test_dm())

  expect_null(attr(domain(obj, "dm"), "trial_title"))
})


test_that("domain() errors for non-existent domains", {
  obj <- sdtm_from(
    dm = tibble::tribble(
        ~USUBJID, ~DOMAIN,
      "SUBJ-001",    "DM",
      "SUBJ-002",    "DM"
    )
  )

  expect_error(domain(obj, "lb"), "Domain 'lb' not found in SDTM object")
  expect_error(domain(obj, "ae"), "Domain 'ae' not found in SDTM object")
  expect_error(domain(obj, "vs"), "Domain 'vs' not found in SDTM object")
})


test_that("domain() is case-insensitive and stores a lowercase name", {
  obj <- sdtm_from(dm = test_dm())

  expect_equal(domain(obj, "dm"), test_dm(), ignore_attr = TRUE)
  expect_equal(domain(obj, "DM"), test_dm(), ignore_attr = TRUE)
  expect_equal(domain(obj, "Dm"), test_dm(), ignore_attr = TRUE)
  expect_equal(domain(obj, "dM"), test_dm(), ignore_attr = TRUE)
  expect_equal(attr(domain(obj, "DM"), "name"), "dm")
})


test_that("domain() handles input validation correctly", {
  obj <- sdtm_from(
    dm = tibble::tribble(
        ~USUBJID, ~DOMAIN,
      "SUBJ-001",    "DM",
      "SUBJ-002",    "DM"
    )
  )

  invalid_obj <- list(domains = list())
  expect_error(
    domain(invalid_obj, "dm"),
    "Input must be a sdtm object"
  )

  expect_error(
    domain(NULL, "dm"),
    "Input must be a sdtm object"
  )

  expect_error(
    domain(obj, NULL),
    "name must not be NULL"
  )
  expect_error(
    domain(obj, 123),
    "name must be a character value"
  )
  expect_error(
    domain(obj, character(0)),
    "name must be a single value"
  )
})


test_that("domain() rejects vectors with multiple names", {
  obj <- sdtm_from(
    dm = tibble::tribble(
        ~USUBJID, ~DOMAIN,
      "SUBJ-001",    "DM",
      "SUBJ-002",    "DM"
    ),
    pc = tibble::tribble(
        ~USUBJID, ~DOMAIN,
      "SUBJ-001",    "PC",
      "SUBJ-002",    "PC"
    )
  )

  expect_error(
    domain(obj, c("dm", "pc")),
    "name must be a single value"
  )
})
