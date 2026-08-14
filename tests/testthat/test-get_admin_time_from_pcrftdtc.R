test_that("get_admin_time_from_pcrftdtc imputes missing DTC_time from PCRFTDTC by USUBJID and date", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:15",
           "1",    "PC", "ANALYTE_A", "2025-01-16T09:17"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-16", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(
    result$DTC_time,
    c("07:00", "08:15", NA, "09:17")
  )
  expect_equal(
    result$IMPUTATION,
    c(
      "time copied from EXSTDTC",
      "time imputed from PCRFTDTC",
      "",
      "time imputed from PCRFTDTC"
    )
  )
  expect_false(".PCRFTDTC_DTC_time" %in% names(result))
  expect_false(".impute" %in% names(result))
  expect_false(".time" %in% names(result))
})


test_that("get_admin_time_from_pcrftdtc does not overwrite existing DTC_time", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-13T09:30"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-13", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_equal(ex$DTC_time, "07:00")
  expect_equal(ex$IMPUTATION, "time copied from EXSTDTC")

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "07:00")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
})


test_that("get_admin_time_from_pcrftdtc leaves days without PCRFTDTC unchanged", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-15T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_true(is.na(result$DTC_time))
  expect_equal(result$IMPUTATION, "")
})


test_that("get_admin_time_from_pcrftdtc imputes independently per administration day", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:15",
           "1",    "PC", "ANALYTE_A", "2025-01-16T09:17"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-16", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_date, c(
    "2025-01-13", "2025-01-14", "2025-01-15", "2025-01-16"
  ))
  expect_equal(result$DTC_time, c("07:00", "08:15", NA, "09:17"))
})


test_that("get_admin_time_from_pcrftdtc selects the earlier time when multiple PCRFTDTC exist on one day", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T10:00",
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_message(
    result <- get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = FALSE
    ),
    "Multiple PCRFTDTC for same days, selecting the earlier!"
  )

  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$IMPUTATION, "time imputed from PCRFTDTC")
})


test_that("get_admin_time_from_pcrftdtc does not warn for same-day PCRFTDTC across different subjects", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "A",    "PC", "ANALYTE_A", "2025-01-14T08:00",
           "B",    "PC", "ANALYTE_A", "2025-01-14T10:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "A", "2025-01-14", "2025-01-14", "TREATMENT_A",
         "B", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_no_message(
    result <- get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = FALSE
    )
  )

  expect_equal(result$DTC_time[result$USUBJID == "A"], "08:00")
  expect_equal(result$DTC_time[result$USUBJID == "B"], "10:00")
})


test_that("get_admin_time_from_pcrftdtc does not pool PCRFTDTC across subjects", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "A",    "PC", "ANALYTE_A", "2025-01-14T08:00",
           "B",    "PC", "ANALYTE_A", "2025-01-14T12:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "A", "2025-01-14", "2025-01-14", "TREATMENT_A",
         "B", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time[result$USUBJID == "A"], "08:00")
  expect_equal(result$DTC_time[result$USUBJID == "B"], "12:00")
})


test_that("get_admin_time_from_pcrftdtc ignores NA PCRFTDTC rows when a valid time exists", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A",                 NA,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$IMPUTATION, "time imputed from PCRFTDTC")
})


test_that("get_admin_time_from_pcrftdtc ignores date-only PCRFTDTC without a time component", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,     ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_true(is.na(result$DTC_time))
  expect_equal(result$IMPUTATION, "")
})


test_that("get_admin_time_from_pcrftdtc with explicit pctestcd ignores other analytes", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00",
           "1",    "PC", "ANALYTE_B", "2025-01-14T10:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$IMPUTATION, "time imputed from PCRFTDTC")
})


test_that("get_admin_time_from_pcrftdtc uses EXTRT as PCTESTCD when pctestcd is NULL and EXTRT matches", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,      ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "TREATMENT_A", "2025-01-14T11:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_no_message(
    result <- get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = NULL, silent = FALSE
    )
  )

  expect_equal(result$DTC_time, c("07:00", "11:00"))
  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time imputed from PCRFTDTC")
  )
})


test_that("get_admin_time_from_pcrftdtc assumes the only PCTESTCD when pctestcd is NULL and EXTRT does not match", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:15"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_message(
    result <- get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = NULL, silent = FALSE
    ),
    "Assuming PCTESTCD 'ANALYTE_A' relates to EXTRT 'TREATMENT_A'!"
  )

  expect_equal(result$DTC_time, c("07:00", "08:15"))
  expect_equal(
    result$IMPUTATION,
    c("time copied from EXSTDTC", "time imputed from PCRFTDTC")
  )
})


test_that("get_admin_time_from_pcrftdtc uses all PCTESTCD when pctestcd is NULL, EXTRT does not match, and multiple codes exist", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00",
           "1",    "PC", "ANALYTE_B", "2025-01-14T10:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC, ~EXTRT,
         "1", "2025-01-14", "2025-01-14",    "X"
  ) |>
    expand_ex()

  expect_message(
    result <- get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "X", pctestcd = NULL, silent = FALSE
    ),
    "Multiple PCRFTDTC for same days, selecting the earlier!"
  )

  expect_equal(result$DTC_time, "08:00")
  expect_equal(result$IMPUTATION, "time imputed from PCRFTDTC")
})


test_that("get_admin_time_from_pcrftdtc returns ex unchanged when PC domain is missing", {
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

  expect_message(
    result <- get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = FALSE
    ),
    "PC not found in sdtm object"
  )

  expect_equal(result$DTC_time, "07:00")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
  expect_false(".PCRFTDTC_DTC_time" %in% names(result))
})


test_that("get_admin_time_from_pcrftdtc returns ex unchanged when PCRFTDTC is missing", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,             ~PCDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-13T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-13", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_message(
    result <- get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = FALSE
    ),
    "PCRFTDTC not found in PC"
  )

  expect_equal(result$DTC_time, "07:00")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
  expect_false(".PCRFTDTC_DTC_time" %in% names(result))
})


test_that("get_admin_time_from_pcrftdtc silent = TRUE suppresses informational messages", {
  sdtm_no_pc <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~DOMAIN,
           "1",    "DM"
    )
  ))

  sdtm_assume <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00"
    )
  ))

  sdtm_dup <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T10:00",
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_no_message(
    get_admin_time_from_pcrftdtc(
      ex, sdtm_no_pc, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A",
      silent = TRUE
    )
  )

  expect_no_message(
    get_admin_time_from_pcrftdtc(
      ex, sdtm_assume, extrt = "TREATMENT_A", pctestcd = NULL, silent = TRUE
    )
  )

  expect_no_message(
    get_admin_time_from_pcrftdtc(
      ex, sdtm_dup, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A",
      silent = TRUE
    )
  )
})


test_that("get_admin_time_from_pcrftdtc errors for unknown pctestcd", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-13T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-13T07:00", "2025-01-13", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_error(
    get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "MISSING", silent = TRUE
    ),
    "missing PCTESTCD MISSING"
  )
})


test_that("get_admin_time_from_pcrftdtc validates input types", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A"
  ) |>
    expand_ex()

  expect_error(
    get_admin_time_from_pcrftdtc(
      ex, list(), extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
    ),
    "must be an SDTM object"
  )

  expect_error(
    get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = 123, silent = TRUE
    ),
    "pctestcd must be a character value"
  )

  expect_error(
    get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = "yes"
    ),
    "silent must be a logical value"
  )

  expect_error(
    get_admin_time_from_pcrftdtc(
      ex, sdtm, extrt = "TREATMENT_A",
      pctestcd = c("ANALYTE_A", "ANALYTE_B"), silent = TRUE
    ),
    "pctestcd must be a single value"
  )
})


test_that("get_admin_time_from_pcrftdtc preserves EX columns", {
  sdtm <- sdtm(list(
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN,   ~PCTESTCD,          ~PCRFTDTC,
           "1",    "PC", "ANALYTE_A", "2025-01-14T08:00"
    )
  ))

  ex <- tibble::tribble(
    ~USUBJID,     ~EXSTDTC,     ~EXENDTC,        ~EXTRT, ~EXDOSE,
         "1", "2025-01-14", "2025-01-14", "TREATMENT_A",    100
  ) |>
    expand_ex()

  result <- get_admin_time_from_pcrftdtc(
    ex, sdtm, extrt = "TREATMENT_A", pctestcd = "ANALYTE_A", silent = TRUE
  )

  expect_true("EXDOSE" %in% names(result))
  expect_equal(result$EXDOSE, 100)
  expect_equal(result$DTC_time, "08:00")
})
