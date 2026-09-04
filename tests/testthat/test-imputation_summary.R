## Tests for imputation_summary

make_imputation_summary_nif <- function() {
  nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,   ~ANALYTE, ~IMPUTATION,
     1,    0,  100,    1,     1,  NA, "DRUG_A", "",
     1,    1,    0,    2,     0,  10, "DRUG_A", "should not count",
     1,   24,  100,    1,     1,  NA, "DRUG_A", "time carried forward",
     2,    0,  100,    1,     1,  NA, "DRUG_A", "",
     2,    0,    0,    2,     0,   5, "MET_A",  "should not count",
     2,   12,  100,    1,     1,  NA, "DRUG_B", "time imputed from PCRFTDTC"
  ))
}


test_that("imputation_summary validates nif input", {
  obj <- make_imputation_summary_nif()

  expect_error(
    imputation_summary(data.frame(ID = 1)),
    "Input must be a nif object"
  )

  expect_error(
    imputation_summary(as.data.frame(obj)),
    "Input must be a nif object"
  )
})


test_that("imputation_summary validates required fields", {
  obj <- make_imputation_summary_nif()

  obj_no_imputation <- obj
  obj_no_imputation$IMPUTATION <- NULL

  expect_error(
    imputation_summary(obj_no_imputation),
    "Missing required fields: IMPUTATION"
  )

  obj_no_analyte <- obj
  obj_no_analyte$ANALYTE <- NULL

  expect_error(
    imputation_summary(obj_no_analyte),
    "Missing required fields: ANALYTE"
  )
})


test_that("imputation_summary validates analyte argument", {
  obj <- make_imputation_summary_nif()

  expect_error(
    imputation_summary(obj, analyte = 1),
    "analyte must be a character value"
  )

  expect_error(
    imputation_summary(obj, analyte = c("DRUG_A", "DRUG_B")),
    "analyte must be a single value"
  )

  expect_error(
    imputation_summary(obj, analyte = NA_character_),
    "analyte must not contain NA"
  )
})


test_that("imputation_summary returns expected columns", {
  result <- imputation_summary(make_imputation_summary_nif())

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("ANALYTE", "IMPUTATION", "N"))
})


test_that("imputation_summary counts only administration rows", {
  result <- imputation_summary(make_imputation_summary_nif())

  expect_equal(
    as.data.frame(result),
    as.data.frame(tibble::tribble(
      ~ANALYTE,                 ~IMPUTATION, ~N,
      "DRUG_A",                            "",  2,
      "DRUG_A",       "time carried forward",  1,
      "DRUG_B", "time imputed from PCRFTDTC",  1
    ))
  )
})


test_that("imputation_summary arranges by analyte and imputation", {
  obj <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,   ~ANALYTE, ~IMPUTATION,
     1,    0,  100,    1,     1,  NA, "ZZZ",    "z last",
     1,   24,  100,    1,     1,  NA, "AAA",    "a first",
     1,   48,  100,    1,     1,  NA, "AAA",    "b second"
  ))

  result <- imputation_summary(obj)

  expect_equal(result$ANALYTE, c("AAA", "AAA", "ZZZ"))
  expect_equal(result$IMPUTATION, c("a first", "b second", "z last"))
})


test_that("imputation_summary filters by analyte when provided", {
  obj <- make_imputation_summary_nif()

  result <- imputation_summary(obj, analyte = "DRUG_A")

  expect_equal(
    as.data.frame(result),
    as.data.frame(tibble::tribble(
      ~ANALYTE,           ~IMPUTATION, ~N,
      "DRUG_A",                      "",  2,
      "DRUG_A", "time carried forward",  1
    ))
  )
  expect_false(any(result$ANALYTE == "DRUG_B"))
})


test_that("imputation_summary returns empty result for unknown analyte", {
  obj <- make_imputation_summary_nif()

  result <- imputation_summary(obj, analyte = "UNKNOWN")

  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("ANALYTE", "IMPUTATION", "N"))
})


test_that("imputation_summary returns empty result when no administrations exist", {
  obj <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,   ~ANALYTE, ~IMPUTATION,
     1,    0,    0,    2,     0,  10, "DRUG_A", "",
     1,    1,    0,    2,     0,  20, "DRUG_A", "time carried forward"
  ))

  result <- imputation_summary(obj)

  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("ANALYTE", "IMPUTATION", "N"))
})


test_that("imputation_summary works with example nif objects", {
  sad_result <- imputation_summary(examplinib_sad_nif)

  expect_equal(
    as.data.frame(sad_result),
    as.data.frame(tibble::tribble(
      ~ANALYTE,                 ~IMPUTATION, ~N,
      "RS2023", "time copied from EXSTDTC", 48
    ))
  )

  poc_result <- imputation_summary(examplinib_poc_nif)

  expect_equal(
    as.data.frame(poc_result),
    as.data.frame(tibble::tribble(
      ~ANALYTE,                     ~IMPUTATION,   ~N,
      "RS2023",           "time carried forward", 5212,
      "RS2023",       "time copied from EXENDTC",  414,
      "RS2023",       "time copied from EXSTDTC",  455,
      "RS2023", "time imputed from PCELTM/PCTPT",    1,
      "RS2023",     "time imputed from PCRFTDTC",   68
    ))
  )
  expect_equal(unique(poc_result$ANALYTE), "RS2023")
  expect_equal(sum(poc_result$N), sum(examplinib_poc_nif$EVID == 1))
})


test_that("imputation_summary filters example nif by analyte", {
  result <- imputation_summary(examplinib_poc_nif, analyte = "RS2023")

  expect_equal(unique(result$ANALYTE), "RS2023")
  expect_equal(sum(result$N), sum(
    examplinib_poc_nif$EVID == 1 &
      examplinib_poc_nif$ANALYTE == "RS2023"
  ))
})

