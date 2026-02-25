test_that("imputation_rules_standard works with single treatment", {
  # Test data
  # sdtm <- list(
  #   pc = tibble::tribble(
  #     ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
  #          "1",    "PC", "ANALYTE_A", "2025-01-15T08:15",
  #          "1",    "PC", "ANALYTE_A", "2025-01-17T09:17",
  #   )
  # ) |>
  #   sdtm()

  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-15T08:15",
           "1",    "PC", "ANALYTE_A", "2025-01-17T09:17"
      )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,           ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-18", "TREATMENT_A"
    )

  ex <- expand_ex(ex)

  result <- imputation_rules_standard[["admin_post_expansion"]](
    ex, sdtm, extrt = "TREATMENT_A", analyte = "ANALYTE_A",
    pctestcd = "ANALYTE_A", cut_off_date = NULL, silent = TRUE)

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "", "time imputed from PCRFTDTC", "",
      "time imputed from PCRFTDTC", "")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", NA, "08:15", NA, "09:17", NA)
  )

  expect_message(
    result <- imputation_rules_standard[["admin_post_expansion"]](
      ex, sdtm, "TREATMENT_A", analyte = NULL, pctestcd = NULL,
      cut_off_date = NULL, silent = FALSE),
    "Assuming PCTESTCD 'ANALYTE_A' relates to EXTRT 'TREATMENT_A'!"
  )

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "", "time imputed from PCRFTDTC", "",
      "time imputed from PCRFTDTC", "")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", NA, "08:15", NA, "09:17", NA)
  )
})



test_that("imputation_rules_standard works with multiple treatments", {
  # Test data
  # sdtm <- list(
  #   pc = tibble::tribble(
  #      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
  #           "1",    "PC", "ANALYTE_B", "2025-01-17T09:20",
  #           "1",    "PC", "ANALYTE_A", "2025-01-15T08:15",
  #           "1",    "PC", "ANALYTE_A", "2025-01-17T09:17"
  #     )
  # ) |>
  #   sdtm()

  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_B", "2025-01-17T09:20",
           "1",    "PC", "ANALYTE_A", "2025-01-15T08:15",
           "1",    "PC", "ANALYTE_A", "2025-01-17T09:17"
      )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,           ~EXENDTC,              ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-18T10:00", "TREATMENT_A",
  )

  expanded_ex <- expand_ex(ex)

  expect_message(
    result <- imputation_rules_standard[["admin_post_expansion"]](
      expanded_ex, sdtm, "TREATMENT_B", analyte = NULL,
      pctestcd = NULL, cut_off_date = NULL, silent = FALSE),
    "Multiple PCRFTDTC for same days, selecting the earlier!"
  )

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "", "time imputed from PCRFTDTC", "",
      "time imputed from PCRFTDTC", "time copied from EXENDTC")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", NA, "08:15", NA, "09:17", "10:00")
  )

  expect_message(
    result <- imputation_rules_standard[["admin_post_expansion"]](
      expanded_ex, sdtm, "TREATMENT_A", analyte = NULL,
      pctestcd = NULL, cut_off_date = NULL, silent = FALSE),
    "Multiple PCRFTDTC for same days, selecting the earlier"
  )

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "", "time imputed from PCRFTDTC", "",
      "time imputed from PCRFTDTC", "time copied from EXENDTC")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", NA, "08:15", NA, "09:17", "10:00")
  )
})



test_that("get_admin_time_from_ntime works as intended", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00",     "PRE-DOSE", "2025-01-14T07:00",
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00", "1 H POSTDOSE", "2025-01-14T09:05",
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00",      "2H POST", "2025-01-14T10:05",
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00",         "4HRS", "2025-01-14T12:05"
      )
  ))

  ex <- tibble::tribble(
     ~USUBJID,     ~EXSTDTC,           ~EXENDTC,        ~EXTRT,
          "1", "2025-01-13T07:00", "2025-01-18", "TREATMENT_A"
     )

  ex <- expand_ex(ex)

  result <- ex |>
    get_admin_time_from_ntime(
      sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = FALSE
    ) |>
    get_admin_time_from_pcrftdtc(
      sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = FALSE
    ) |>
    carry_forward_admin_time_imputations()

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time imputed from PCRFTDTC",
      "time carried forward", "time carried forward", "time carried forward",
      "time carried forward"))

  expect_equal(
    result$DTC_time,
    c("07:00", "08:00", "08:00", "08:00", "08:00", "08:00"))
})








