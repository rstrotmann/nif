## Incomplete PCRFTDTC values during administration time imputation


incomplete_pcrftdtc_fixture <- function(pcrftdtc) {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
           "1",    0, "2025-01-14T08:00", "2025-01-14",   "TREAT"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
           "1",      1,    "A", "2025-01-14T08:00", "2025-01-14T08:00",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,             ~PCDTC, ~PCRFTDTC,         ~PCTPT,
           "1",    "PC",       "A", "2025-01-14T09:00",  pcrftdtc, "1 H POSTDOSE"
    )
  ))

  ex <- expand_ex(domain(sdtm, "ex"))
  list(sdtm = sdtm, ex = ex)
}


test_that("date-only PCRFTDTC does not emit parse warnings or impute clock time", {
  ## YYYY-MM-DD parses but cannot supply a clock time.
  fx <- incomplete_pcrftdtc_fixture("2025-01-14")

  expect_no_warning(
    result <- get_admin_time_from_pcrftdtc(
      fx$ex, fx$sdtm, extrt = "A", pctestcd = "A", silent = TRUE
    )
  )

  expect_equal(result$DTC_time, "08:00")
  expect_false(grepl("PCRFTDTC", result$IMPUTATION))

  expect_message(
    get_admin_time_from_pcrftdtc(
      fx$ex, fx$sdtm, extrt = "A", pctestcd = "A", silent = FALSE
    ),
    "PCRFTDTC.*no time component"
  )
})


test_that("hour-only PCRFTDTC does not emit parse warnings or impute clock time", {
  ## ISO truncated YYYY-MM-DDTHH is dropped with a package message.
  fx <- incomplete_pcrftdtc_fixture("2025-01-14T08")

  expect_no_warning(
    result <- get_admin_time_from_pcrftdtc(
      fx$ex, fx$sdtm, extrt = "A", pctestcd = "A", silent = TRUE
    )
  )

  expect_equal(result$DTC_time, "08:00")
  expect_false(grepl("PCRFTDTC", result$IMPUTATION))

  expect_message(
    get_admin_time_from_pcrftdtc(
      fx$ex, fx$sdtm, extrt = "A", pctestcd = "A", silent = FALSE
    ),
    "unparseable PCRFTDTC"
  )
})


test_that("PCRFTDTC with trailing space does not emit parse warnings", {
  ## Trailing space is trimmed; complete datetime imputes 08:15.
  fx <- incomplete_pcrftdtc_fixture("2025-01-14T08:15 ")

  expect_no_warning(
    result <- get_admin_time_from_pcrftdtc(
      fx$ex, fx$sdtm, extrt = "A", pctestcd = "A", silent = TRUE
    )
  )

  expect_equal(result$DTC_time, "08:15")
  expect_true(grepl("PCRFTDTC", result$IMPUTATION))
})


test_that("blank PCRFTDTC does not emit parse warnings or impute clock time", {
  fx <- incomplete_pcrftdtc_fixture(" ")

  expect_no_warning(
    result <- get_admin_time_from_pcrftdtc(
      fx$ex, fx$sdtm, extrt = "A", pctestcd = "A", silent = TRUE
    )
  )

  expect_equal(result$DTC_time, "08:00")
  expect_false(grepl("PCRFTDTC", result$IMPUTATION))
})


test_that("empty PCRFTDTC does not emit parse warnings or impute clock time", {
  fx <- incomplete_pcrftdtc_fixture("")

  expect_no_warning(
    result <- get_admin_time_from_pcrftdtc(
      fx$ex, fx$sdtm, extrt = "A", pctestcd = "A", silent = TRUE
    )
  )

  expect_equal(result$DTC_time, "08:00")
  expect_false(grepl("PCRFTDTC", result$IMPUTATION))
})


test_that("add_administration does not warn on incomplete PCRFTDTC variants", {
  ## Trailing-space value should trim and impute; other incomplete forms must
  ## not trigger lubridate "failed to parse" warnings.
  dm <- tibble::tribble(
    ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
         "1",    0, "2025-01-14T08:00", "2025-01-14",   "TREAT"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "1",      1,    "A", "2025-01-14T08:00", "2025-01-14T08:00",     100
  )

  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD,             ~PCDTC,          ~PCRFTDTC,         ~PCTPT,
         "1",    "PC",       "A", "2025-01-14T09:00",       "2025-01-14", "1 H POSTDOSE",
         "1",    "PC",       "A", "2025-01-14T10:00",    "2025-01-14T08", "2H POST",
         "1",    "PC",       "A", "2025-01-14T11:00", "2025-01-14T08:15 ", "3H POST",
         "1",    "PC",       "A", "2025-01-14T12:00",               " ", "4H POST",
         "1",    "PC",       "A", "2025-01-14T13:00",                "", "5H POST"
  )

  sdtm <- sdtm(list(dm = dm, ex = ex, pc = pc))

  expect_no_warning(
    result <- add_administration(
      nif(),
      sdtm,
      extrt      = "A",
      pctestcd   = "A",
      imputation = imputation_rules_standard,
      silent     = TRUE
    )
  )

  res <- as.data.frame(result)
  expect_equal(format(res$DTC, "%H:%M"), "08:15")
  expect_true(grepl("PCRFTDTC", res$IMPUTATION))
})

