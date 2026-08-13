# Comprehensive tests for get_admin_time_from_ntime() (fill-if-missing).
# Expanded EX is expected as input (DTC_date, DTC_time, IMPUTATION present).


test_that("get_admin_time_from_ntime imputes missing DTC_time from PCDTC minus NTIME", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, c("07:00", "08:00"))
  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time imputed from PCELTM/PCTPT")
  )
  expect_equal(result$.NTIME_DTC_time, c(NA, "08:00"))
})


test_that("get_admin_time_from_ntime does not overwrite existing DTC_time", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14T07:00", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "07:00")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
  # Estimate is still attached for downstream use
  expect_equal(result$.NTIME_DTC_time, "08:00")
})


test_that("get_admin_time_from_ntime leaves days without postdose PC unchanged", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-15T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_true(is.na(result$DTC_time))
  expect_true(is.na(result$.NTIME_DTC_time))
  expect_equal(result$IMPUTATION, "")
})


test_that("get_admin_time_from_ntime imputes independently per administration day", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00",
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-16T11:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-16", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(
    result$DTC_time,
    c("07:00", "08:00", NA, "10:00")
  )
  expect_equal(
    result$IMPUTATION,
    c(
      "time copied from EXSTDTC",
      "time imputed from PCELTM/PCTPT",
      "",
      "time imputed from PCELTM/PCTPT"
    )
  )
  expect_equal(result$.NTIME_DTC_time, c(NA, "08:00", NA, "10:00"))
})


test_that("get_admin_time_from_ntime ignores predose rows (NTIME must be > 0)", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,     ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "PRE-DOSE", "2025-01-14T07:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_true(is.na(result$DTC_time))
  expect_true(is.na(result$.NTIME_DTC_time))
  expect_equal(result$IMPUTATION, "")
})


test_that("get_admin_time_from_ntime uses postdose rows when mixed with predose", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A",     "PRE-DOSE", "2025-01-14T07:00",
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$.NTIME_DTC_time, "08:00")
  expect_equal(result$IMPUTATION, "time imputed from PCELTM/PCTPT")
})


test_that("get_admin_time_from_ntime prefers PCELTM over PCTPT when both are present", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD, ~PCELTM,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A",  "PT2H", "1 H POSTDOSE", "2025-01-14T10:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  # PCELTM PT2H: 10:00 - 2 h -> 08:00 (not 09:00 from PCTPT)
  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$.NTIME_DTC_time, "08:00")
})


test_that("get_admin_time_from_ntime averages multiple postdose points that imply the same admin time", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:05",
           "1",    "PC", "ANALYTE_A",      "2H POST", "2025-01-14T10:05"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "08:05")
  expect_equal(result$.NTIME_DTC_time, "08:05")
  expect_equal(result$IMPUTATION, "time imputed from PCELTM/PCTPT")
})


test_that("get_admin_time_from_ntime weighted average prefers earlier NTIME by default", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00",
           "1",    "PC", "ANALYTE_A",         "4HRS", "2025-01-14T13:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  # Default ntime_exponent = -0.8 weights the 1 h estimate more heavily
  result_default <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )
  expect_equal(result_default$DTC_time, "08:14")

  # Equal weights (exponent 0): mean of 08:00 and 09:00 -> 08:30
  result_equal <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE,
    ntime_exponent = 0
  )
  expect_equal(result_equal$DTC_time, "08:30")
})


test_that("get_admin_time_from_ntime estimates times per subject, not across subjects", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "A",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00",
           "B",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T13:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "A", "2025-01-14", "2025-01-14", "TREATMENT_A",
         "B", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time[result$USUBJID == "A"], "08:00")
  expect_equal(result$DTC_time[result$USUBJID == "B"], "12:00")
})


test_that("get_admin_time_from_ntime with explicit pctestcd ignores other analytes", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00",
           "1",    "PC", "ANALYTE_B", "1 H POSTDOSE", "2025-01-14T11:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$.NTIME_DTC_time, "08:00")
})


test_that("get_admin_time_from_ntime uses all PCTESTCD when pctestcd is NULL", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00",
           "1",    "PC", "ANALYTE_B", "1 H POSTDOSE", "2025-01-14T11:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = NULL, silent = TRUE
  )

  # Equal NTIME weights: mean of 08:00 and 10:00 -> 09:00
  expect_equal(result$DTC_time, "09:00")
  expect_equal(result$.NTIME_DTC_time, "09:00")
  expect_equal(result$IMPUTATION, "time imputed from PCELTM/PCTPT")
})


test_that("get_admin_time_from_ntime returns NA helper column when PC domain is missing", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN,
           "1",    "DM"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-13", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "07:00")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
  expect_true(all(is.na(result$.NTIME_DTC_time)))
})


test_that("get_admin_time_from_ntime leaves times missing when PCDTC is NA", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT, ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE",     NA
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_true(is.na(result$DTC_time))
  expect_true(is.na(result$.NTIME_DTC_time))
  expect_equal(result$IMPUTATION, "")
})


test_that("get_admin_time_from_ntime derives time from date-only PCDTC", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,     ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  # Midnight PCDTC minus 1 h -> 23:00 previous calendar instant as time-of-day
  expect_equal(result$DTC_time, "23:00")
  expect_equal(result$.NTIME_DTC_time, "23:00")
  expect_equal(result$IMPUTATION, "time imputed from PCELTM/PCTPT")
})


test_that("get_admin_time_from_ntime errors for unknown pctestcd", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_error(
    get_admin_time_from_ntime(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "MISSING", silent = TRUE
    ),
    "missing PCTESTCD MISSING"
  )
})


test_that("get_admin_time_from_ntime validates input types", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_error(
    get_admin_time_from_ntime(
      ex, list(), extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
    ),
    "Input must be a sdtm object"
  )

  expect_error(
    get_admin_time_from_ntime(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = 123, silent = TRUE
    ),
    "pctestcd must be a character value"
  )

  expect_error(
    get_admin_time_from_ntime(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = "yes"
    ),
    "silent must be a logical value"
  )

  expect_error(
    get_admin_time_from_ntime(
      ex, sdtm, extrt = "TREATMENT_A",
      pctestcd = c("ANALYTE_A", "ANALYTE_B"), silent = TRUE
    ),
    "pctestcd must be a single value"
  )
})


test_that("get_admin_time_from_ntime preserves EX columns and does not leave .impute", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT, ~EXDOSE,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A",    100
  ) |>
    expand_ex()

  result <- get_admin_time_from_ntime(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_true("EXDOSE" %in% names(result))
  expect_equal(result$EXDOSE, 100)
  expect_false(".impute" %in% names(result))
  expect_true(".NTIME_DTC_time" %in% names(result))
})


test_that("get_admin_time_from_ntime emits NTIME debug details when debug is enabled", {
  nif_option(debug = TRUE)
  on.exit(nif_option(debug = FALSE), add = TRUE)

  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "A",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "A", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_message(
    result <- get_admin_time_from_ntime(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
    ),
    "get_admin_time_from_ntime: NTIME details for TREATMENT_A"
  )

  expect_equal(result$DTC_time, "08:00")
})


test_that("get_admin_time_from_ntime does not emit debug details when debug is disabled", {
  nif_option(debug = FALSE)

  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,         ~PCTPT,             ~PCDTC,
           "A",    "PC", "ANALYTE_A", "1 H POSTDOSE", "2025-01-14T09:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "A", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_no_message(
    result <- get_admin_time_from_ntime(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
    )
  )

  expect_equal(result$DTC_time, "08:00")
})
