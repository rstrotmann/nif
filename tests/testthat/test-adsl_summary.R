## Tests for adsl_summary
##
## Summarizes an ADSL adam_dataset: demography, treatment (TRT01P/TRT01A),
## end-of-study status, and analysis population flags. Subjects with
## TRT01P == "SCREEN FAILURE" are excluded from the treated subset.


test_that("adsl_summary returns the expected list structure", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~SITEID, ~COUNTRY, ~SEX, ~RACE,  ~TRT01P,  ~TRT01A, ~SAFFL,
       "U1",    "01",    "USA",  "M", "WHITE", "Placebo", "Placebo",   "Y"
  ))

  result <- adsl_summary(adsl)

  expect_type(result, "list")
  expect_equal(
    names(result),
    c("country", "site", "sex", "race", "arm", "eos", "population")
  )
})


test_that("adsl_summary summarizes TRT01P/TRT01A and excludes screen failures", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~SITEID, ~COUNTRY, ~SEX, ~RACE,                     ~TRT01P,         ~TRT01A, ~SAFFL, ~ITTFL, ~FASFL,
       "U1",    "01",    "USA",  "M", "WHITE",                    "Placebo",       "Placebo",   "Y",   "Y",   "Y",
       "U2",    "01",    "USA",  "F", "WHITE",                    "Active",        "Active",    "Y",   "Y",   "N",
       "U3",    "02",    "DEU",  "M", "ASIAN",                    "SCREEN FAILURE", "SCREEN FAILURE", "N", "N", "N",
       "U4",    "02",    "DEU",  "F", "BLACK OR AFRICAN AMERICAN", "Placebo",       "Placebo",   "Y",   "N",   "N"
  ))

  result <- adsl_summary(adsl)

  expect_equal(sort(result$site), c("01", "02"))
  expect_equal(result$country$COUNTRY, c("USA", "DEU"))
  expect_equal(result$country$n, c(2, 1))
  expect_equal(result$country$percent, c(66.7, 33.3))

  expect_equal(result$sex$SEX, c("M", "F"))
  expect_equal(result$sex$n, c(1, 2))
  expect_equal(result$sex$percent, c(33.3, 66.7))

  expect_equal(
    result$race$RACE,
    c("WHITE", "BLACK OR AFRICAN AMERICAN")
  )
  expect_equal(result$race$n, c(2, 1))

  expect_equal(result$arm$TRT01P, c("Placebo", "Active"))
  expect_equal(result$arm$TRT01A, c("Placebo", "Active"))
  expect_equal(result$arm$n, c(2, 1))
  expect_equal(result$arm$percent, c(66.7, 33.3))

  expect_equal(result$population$population, c("FASFL", "SAFFL", "ITTFL"))
  expect_equal(
    result$population$n[result$population$population == "SAFFL"],
    3
  )
  expect_equal(
    result$population$percent[result$population$population == "SAFFL"],
    100
  )
  expect_equal(
    result$population$n[result$population$population == "FASFL"],
    1
  )
  expect_equal(
    result$population$percent[result$population$population == "FASFL"],
    round(100 / 3, 1)
  )
  expect_equal(
    result$population$n[result$population$population == "ITTFL"],
    2
  )
})


test_that("adsl_summary is case-insensitive for screen failure on TRT01P", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~TRT01P,           ~SEX, ~COUNTRY, ~SITEID, ~RACE,
       "U1", "Placebo",          "M",    "USA",    "01", "WHITE",
       "U2", "screen failure",   "F",    "DEU",    "02", "ASIAN"
  ))

  result <- adsl_summary(adsl)

  expect_equal(sum(result$sex$n), 1)
  expect_equal(result$site, "01")
  expect_equal(result$arm$TRT01P, "Placebo")
})


test_that("adsl_summary returns NULL for missing optional columns", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~TRT01P,
       "U1", "Placebo"
  ))

  result <- adsl_summary(adsl)

  expect_null(result$country)
  expect_null(result$sex)
  expect_null(result$race)
  expect_null(result$eos)
  expect_null(result$population)
  expect_null(result$site)
  expect_equal(result$arm$TRT01P, "Placebo")
  expect_equal(result$arm$n, 1)
})


test_that("adsl_summary returns NULL arm when TRT01P/TRT01A are absent", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~ARM, ~ARMCD, ~SEX, ~COUNTRY, ~SITEID, ~RACE, ~SAFFL,
       "U1", "Placebo", "PBO", "M", "USA", "01", "WHITE", "Y",
       "U2", "SCREEN FAILURE", "SCRNFAIL", "F", "DEU", "02", "ASIAN", "N"
  ))

  result <- adsl_summary(adsl)

  expect_null(result$arm)
  # Without TRT01P, screen failures are not excluded
  expect_equal(sort(result$site), c("01", "02"))
  expect_equal(sum(result$sex$n), 2)
})


test_that("adsl_summary summarizes EOSSTT when present", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~TRT01P,  ~EOSSTT,      ~SEX, ~COUNTRY, ~SITEID, ~RACE,
       "U1", "Placebo", "COMPLETED",  "M",    "USA",    "01", "WHITE",
       "U2", "Active",  "DISCONTINUED", "F",  "USA",    "01", "WHITE",
       "U3", "Placebo", "COMPLETED",  "F",    "USA",    "02", "WHITE"
  ))

  result <- adsl_summary(adsl)

  expect_equal(result$eos$EOSSTT, c("COMPLETED", "DISCONTINUED"))
  expect_equal(result$eos$n, c(2, 1))
  expect_equal(result$eos$percent, c(66.7, 33.3))
})


test_that("adsl_summary only keeps population rows flagged Y", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~TRT01P,  ~SAFFL, ~ITTFL,
       "U1", "Placebo",   "Y",   "Y",
       "U2", "Active",    "Y",   "N",
       "U3", "Placebo",   "N",   "N"
  ))

  result <- adsl_summary(adsl)

  expect_equal(result$population$population, c("SAFFL", "ITTFL"))
  expect_true(all(result$population$n >= 1))
  expect_equal(
    result$population$n[result$population$population == "SAFFL"],
    2
  )
  expect_equal(
    result$population$n[result$population$population == "ITTFL"],
    1
  )
})


test_that("adsl_summary errors when USUBJID is not unique", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~TRT01P,
       "U1", "Placebo",
       "U1", "Placebo"
  ))

  expect_error(
    adsl_summary(adsl),
    "more than one rows per subject"
  )
})


test_that("adsl_summary validates adam_dataset input", {
  expect_error(
    adsl_summary(data.frame(USUBJID = "U1", TRT01P = "Placebo")),
    "adam_dataset"
  )
})


test_that("adsl_summary errors when USUBJID is missing", {
  adsl <- new_dataset(tibble::tribble(
    ~TRT01P, ~SEX,
    "Placebo", "M"
  ))

  expect_error(adsl_summary(adsl))
})


test_that("adsl_summary works with TRT01P only (no TRT01A)", {
  adsl <- new_dataset(tibble::tribble(
    ~USUBJID, ~TRT01P,  ~SEX, ~COUNTRY, ~SITEID, ~RACE,
       "U1", "Placebo", "M",    "USA",    "01", "WHITE",
       "U2", "Active",  "F",    "USA",    "01", "WHITE"
  ))

  result <- adsl_summary(adsl)

  expect_equal(names(result$arm), c("TRT01P", "n", "percent"))
  expect_equal(result$arm$TRT01P, c("Placebo", "Active"))
  expect_equal(result$arm$n, c(1, 1))
})
