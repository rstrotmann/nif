test_that("impute_lloq_pc returns unchanged data when DOMAIN column is missing", {
  pc <- tibble::tribble(
    ~USUBJID, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "5.2",      5.2,       0.1
  )

  result <- impute_lloq_pc(pc, silent = TRUE)
  expect_equal(result, pc)
})


test_that("impute_lloq_pc returns unchanged data when no PC rows exist", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "LB",    "5.2",      5.2,       0.1
  )

  result <- impute_lloq_pc(pc, silent = TRUE)
  expect_equal(result, pc)
})


test_that("impute_lloq_pc imputes BQL values to PCLLOQ / 2 by default", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "5.2",      5.2,       0.5,
    "001",    "PC",    "BQL",      NA_real_,  0.5,
    "001",    "PC",    "10.0",     10.0,      0.5
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, c(5.2, 0.25, 10.0))
  expect_equal(result$IMPUTATION, c("", "LLOQ imputation", ""))
})


test_that("impute_lloq_pc detects BLQ values", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BLQ",      NA_real_,  1.0
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, 0.5)
  expect_equal(result$IMPUTATION, "LLOQ imputation")
})


test_that("impute_lloq_pc detects LOQ values", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "LOQ",      NA_real_,  2.0
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, 1.0)
  expect_equal(result$IMPUTATION, "LLOQ imputation")
})


test_that("impute_lloq_pc detects values with < prefix", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "<0.5",     NA_real_,  0.5
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, 0.25)
  expect_equal(result$IMPUTATION, "LLOQ imputation")
})


test_that("impute_lloq_pc leaves non-LLOQ rows unchanged", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "5.2",      5.2,       0.5,
    "001",    "PC",    "10.0",     10.0,      0.5,
    "001",    "PC",    "0.8",      0.8,       0.5
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, c(5.2, 10.0, 0.8))
  expect_equal(result$IMPUTATION, c("", "", ""))
})


test_that("impute_lloq_pc handles custom lloq_expression", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  1.0,
    "001",    "PC",    "3.0",      3.0,       1.0
  )

  result <- impute_lloq_pc(pc, lloq_expression = "PCLLOQ", silent = TRUE)
  expect_equal(result$DV, c(1.0, 3.0))

  result_zero <- impute_lloq_pc(pc, lloq_expression = "0", silent = TRUE)
  expect_equal(result_zero$DV, c(0, 3.0))
})


test_that("impute_lloq_pc handles custom lloq_filter", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "NS",       NA_real_,  0.5,
    "001",    "PC",    "5.2",      5.2,       0.5
  )

  result_default <- impute_lloq_pc(pc, silent = TRUE)
  expect_equal(result_default$DV, c(NA_real_, 5.2))

  result_custom <- impute_lloq_pc(pc, lloq_filter = "NS", silent = TRUE)
  expect_equal(result_custom$DV, c(0.25, 5.2))
  expect_equal(result_custom$IMPUTATION, c("LLOQ imputation", ""))
})


test_that("impute_lloq_pc creates IMPUTATION column if missing", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  0.5,
    "001",    "PC",    "5.2",      5.2,       0.5
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_true("IMPUTATION" %in% names(result))
  expect_equal(result$IMPUTATION, c("LLOQ imputation", ""))
})


test_that("impute_lloq_pc preserves existing IMPUTATION values", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ, ~IMPUTATION,
    "001",    "PC",    "BQL",      NA_real_,  0.5,     "prior imputation",
    "001",    "PC",    "5.2",      5.2,       0.5,     "other imputation"
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$IMPUTATION, c("LLOQ imputation", "other imputation"))
})


test_that("impute_lloq_pc warns when required columns are missing", {
  pc_no_lloq <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN,
    "001",    "PC",    "BQL",      NA_real_
  )

  expect_message(
    result <- impute_lloq_pc(pc_no_lloq, silent = FALSE),
    "LLOQ imputation cannot be done"
  )
  expect_false("DV" %in% names(result))

  pc_no_stresc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    5.2,        0.5
  )

  expect_message(
    result2 <- impute_lloq_pc(pc_no_stresc, silent = FALSE),
    "LLOQ imputation cannot be done"
  )
  expect_false("DV" %in% names(result2))
})


test_that("impute_lloq_pc returns data unchanged when columns missing", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC,
    "001",    "PC",    "BQL"
  )

  result <- impute_lloq_pc(pc, silent = TRUE)
  expect_equal(result, pc)
})


test_that("impute_lloq_pc errors on invalid lloq_expression", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  0.5
  )

  expect_error(
    impute_lloq_pc(pc, lloq_expression = "!!!invalid{{{", silent = TRUE)
  )
})


test_that("impute_lloq_pc validates argument types", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "5.2",      5.2,       0.5
  )

  expect_error(impute_lloq_pc(pc, lloq_filter = 123))
  expect_error(impute_lloq_pc(pc, lloq_expression = 123))
  expect_error(impute_lloq_pc(pc, silent = "yes"))
})


test_that("impute_lloq_pc handles all rows being LLOQ", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  0.5,
    "001",    "PC",    "<0.5",     NA_real_,  0.5,
    "001",    "PC",    "BLQ",      NA_real_,  0.5
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, c(0.25, 0.25, 0.25))
  expect_true(all(result$IMPUTATION == "LLOQ imputation"))
})


test_that("impute_lloq_pc handles no rows being LLOQ", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "5.2",      5.2,       0.5,
    "001",    "PC",    "10.0",     10.0,      0.5
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, c(5.2, 10.0))
  expect_true(all(result$IMPUTATION == ""))
})


test_that("impute_lloq_pc handles different PCLLOQ values per row", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  0.5,
    "001",    "PC",    "BQL",      NA_real_,  1.0,
    "001",    "PC",    "BQL",      NA_real_,  2.0
  )

  result <- impute_lloq_pc(pc, silent = TRUE)
  expect_equal(result$DV, c(0.25, 0.5, 1.0))
})


test_that("impute_lloq_pc handles multiple subjects", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "5.2",      5.2,       0.5,
    "001",    "PC",    "BQL",      NA_real_,  0.5,
    "002",    "PC",    "BQL",      NA_real_,  1.0,
    "002",    "PC",    "8.0",      8.0,       1.0
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_equal(result$DV, c(5.2, 0.25, 0.5, 8.0))
  expect_equal(
    result$IMPUTATION,
    c("", "LLOQ imputation", "LLOQ imputation", "")
  )
})


test_that("impute_lloq_pc preserves extra columns", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ, ~VISIT, ~PCTPT,
    "001",    "PC",    "BQL",      NA_real_,  0.5,     "DAY 1", "PRE-DOSE",
    "001",    "PC",    "5.2",      5.2,       0.5,     "DAY 1", "1H POST"
  )

  result <- impute_lloq_pc(pc, silent = TRUE)

  expect_true(all(c("VISIT", "PCTPT") %in% names(result)))
  expect_equal(result$VISIT, c("DAY 1", "DAY 1"))
  expect_equal(result$PCTPT, c("PRE-DOSE", "1H POST"))
})


test_that("impute_lloq_pc handles empty PC data frame", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ
  )

  result <- impute_lloq_pc(pc, silent = TRUE)
  expect_equal(nrow(result), 0)
})


test_that("impute_lloq_pc suppresses messages when silent = TRUE", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC,
    "001",    "PC",    "BQL"
  )

  expect_no_message(impute_lloq_pc(pc, silent = TRUE))
})


test_that("impute_lloq_pc handles lloq_expression referencing custom column", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ, ~CUSTOM_VAL,
    "001",    "PC",    "BQL",      NA_real_,  0.5,     10.0,
    "001",    "PC",    "5.2",      5.2,       0.5,     10.0
  )

  result <- impute_lloq_pc(
    pc,
    lloq_expression = "CUSTOM_VAL * PCLLOQ",
    silent = TRUE
  )

  expect_equal(result$DV, c(5.0, 5.2))
})


test_that("impute_lloq_pc warns when custom expression column is missing", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  0.5
  )

  expect_message(
    result <- impute_lloq_pc(
      pc,
      lloq_expression = "MISSING_COL / 2",
      silent = FALSE
    ),
    "LLOQ imputation cannot be done"
  )

  expect_false("DV" %in% names(result))
})


test_that("impute_lloq_pc with NULL lloq_filter errors", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  0.5
  )

  expect_error(impute_lloq_pc(pc, lloq_filter = NULL))
})


test_that("impute_lloq_pc with NULL lloq_expression errors", {
  pc <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~PCSTRESC, ~PCSTRESN, ~PCLLOQ,
    "001",    "PC",    "BQL",      NA_real_,  0.5
  )

  expect_error(impute_lloq_pc(pc, lloq_expression = NULL))
})
