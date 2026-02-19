test_that("imputation_standard works with single treatment", {
  # Test data
  sdtm <- list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-15T08:15",
           "1",    "PC", "ANALYTE_A", "2025-01-17T09:17",
    )
  ) |>
    sdtm()

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,           ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-18", "TREATMENT_A"
    )

  ex <- expand_ex(ex)

  result <- imputation_standard[["admin_post_expansion"]](
    expanded_ex, sdtm, "TREATMENT_A", analyte = "ANALYTE_A",
    cut_off_date = NULL, silent = FALSE) |>
    decompose_dtc("DTC")

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time carried forward")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", "07:00", "08:15", "08:15", "09:17", "09:17")
  )

  expect_message(
    result <- imputation_standard[["admin_post_expansion"]](
      expanded_ex, sdtm, "TREATMENT_A", analyte = NULL,
      cut_off_date = NULL, silent = FALSE),
    "Assuming PCTESTCD 'ANALYTE_A' relates to EXTRT 'TREATMENT_A'!"
  )

  result <- result |>
    decompose_dtc("DTC")

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time carried forward")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", "07:00", "08:15", "08:15", "09:17", "09:17")
  )
})



test_that("imputation_standard works with multiple treatments", {
  # Test data
  sdtm <- list(
    pc = tibble::tribble(
       ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
            "1",    "PC", "ANALYTE_B", "2025-01-17T09:20",
            "1",    "PC", "ANALYTE_A", "2025-01-15T08:15",
            "1",    "PC", "ANALYTE_A", "2025-01-17T09:17"
      )
  ) |>
    sdtm()

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,           ~EXENDTC,              ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-18T10:00", "TREATMENT_A",
  )

  expanded_ex <- expand_ex(ex)

  expect_message(
    result <- imputation_standard[["admin_post_expansion"]](
      expanded_ex, sdtm, "TREATMENT_B", analyte = NULL,
      cut_off_date = NULL, silent = FALSE),
    "Multiple PCRFTDTC for same days, selecting the earlier!"
  )

  result <- result |>
    decompose_dtc("DTC")

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time copied from EXENDTC")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", "07:00", "08:15", "08:15", "09:17", "10:00")
  )

  expect_message(
    result <- imputation_standard[["admin_post_expansion"]](
      expanded_ex, sdtm, "TREATMENT_A", analyte = NULL,
      cut_off_date = NULL, silent = FALSE),
    "Multiple PCRFTDTC for same days, selecting the earlier"
  )

  result <- result |>
    decompose_dtc("DTC")

  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time carried forward",
      "time imputed from PCRFTDTC", "time copied from EXENDTC")
  )
  expect_equal(
    result$DTC_time,
    c("07:00", "07:00", "08:15", "08:15", "09:17", "10:00")
  )
})
