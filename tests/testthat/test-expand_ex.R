test_that("expand_ex requires data frame input", {
  expect_error(expand_ex("not a data frame"), "Input must be a data frame")
  expect_error(expand_ex(123), "Input must be a data frame")
  expect_error(expand_ex(list()), "Input must be a data frame")
  expect_error(expand_ex(NULL), "Input must be a data frame")
})


test_that("expand_ex requires required columns", {
  ex <- tribble(
    ~USUBJID, ~EXTRT,
    "A", "DRUG"
  ) %>% lubrify_dates()

  expect_error(expand_ex(ex), "Missing fields: EXSTDTC and EXENDTC!")
})


test_that("expand_ex works with single day episode", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-01T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$USUBJID, "A")
  expect_equal(result$EXTRT, "DRUG")
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
})


test_that("expand_ex works with single day episode without time", {
  ex <- tribble(
    ~USUBJID, ~EXTRT,     ~EXSTDTC,    ~EXENDTC,
         "A", "DRUG", "2025-01-01", "2025-01-01"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$IMPUTATION, "")
})


test_that("expand_ex expands multi-day episode correctly", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 3)
  expect_equal(result$USUBJID, rep("A", 3))
  expect_equal(result$EXTRT, rep("DRUG", 3))
  # expect_equal(result$IMPUTATION, c("", "time carried forward", ""))
})


test_that("expand_ex handles multiple episodes for same subject", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-02",
    "A", "DRUG", "2025-01-05T08:00", "2025-01-06"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 4)
  expect_equal(result$USUBJID, rep("A", 4))
})


test_that("expand_ex handles multiple subjects", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-02",
    "B", "DRUG", "2025-01-01T08:00", "2025-01-02"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 4)
  expect_equal(unique(result$USUBJID), c("A", "B"))
})


test_that("expand_ex handles multiple treatments", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG1", "2025-01-01T07:00", "2025-01-02",
    "A", "DRUG2", "2025-01-01T08:00", "2025-01-02"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 4)
  expect_equal(unique(result$EXTRT), c("DRUG1", "DRUG2"))
})


test_that("expand_ex creates IMPUTATION column when missing", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-02"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_true("IMPUTATION" %in% names(result))
  expect_equal(result$IMPUTATION[1], "time copied from EXSTDTC")
})


# test_that("expand_ex preserves existing IMPUTATION values", {
#   ex <- tribble(
#     ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
#     "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00", "original"
#   ) %>% lubrify_dates()
#
#   result <- expand_ex(ex)
#
#   expect_equal(result$IMPUTATION[1], "original")
#   expect_equal(result$IMPUTATION[2], "time carried forward")
#   expect_equal(result$IMPUTATION[3], "original")
# })


test_that("expand_ex handles time imputation correctly", {
  ex <- tibble::tribble(
     ~USUBJID, ~EXTRT,           ~EXSTDTC,     ~EXENDTC,
          "A", "DRUG", "2025-01-01T07:00", "2025-01-03"
     ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$IMPUTATION[1], "time copied from EXSTDTC")
  expect_equal(result$IMPUTATION[2], "")
  expect_equal(result$IMPUTATION[3], "")
})


test_that("expand_ex handles missing EXENDTC time", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$IMPUTATION[3], "")
})


test_that("expand_ex handles no time information", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01", "2025-01-03"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(unique(result$IMPUTATION), "")
})


test_that("expand_ex calculates EXDY when EXSTDY and EXENDY provided", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSTDY, ~EXENDY,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00", 1, 3
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_true("EXDY" %in% names(result))
  expect_equal(result$EXDY, c(1, 2, 3))
})


test_that("expand_ex does not create EXDY when study days not provided", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_false("EXDY" %in% names(result))
})


test_that("expand_ex handles EXSTDY and EXENDY as character", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSTDY, ~EXENDY,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00", "1", "3"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$EXDY, c(1, 2, 3))
})


test_that("expand_ex handles only EXSTDY provided", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSTDY,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00", 1
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_false("EXDY" %in% names(result))
})


test_that("expand_ex handles only EXENDY provided", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXENDY,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00", 3
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_false("EXDY" %in% names(result))
})


test_that("expand_ex handles long episode correctly", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-10T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 10)
  expect_equal(result$IMPUTATION[1], "time copied from EXSTDTC")
  expect_equal(result$IMPUTATION[10], "time copied from EXENDTC")
  expect_equal(unique(result$IMPUTATION[2:9]), "")
})


test_that("expand_ex handles episode with only start time", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 3)
  expect_equal(result$IMPUTATION[1], "time copied from EXSTDTC")
  expect_equal(result$IMPUTATION[2], "")
  expect_equal(result$IMPUTATION[3], "")
})


test_that("expand_ex handles episode with only end time", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01", "2025-01-03T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 3)
  expect_equal(
    result$IMPUTATION,
    c("", "", "time copied from EXENDTC")
  )
  expect_equal(result$DTC_time, c(NA, NA, "08:00"))
})


test_that("expand_ex handles multiple episodes with different time patterns", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-02T08:00",
    "A", "DRUG", "2025-01-03", "2025-01-04",
    "A", "DRUG", "2025-01-05T09:00", "2025-01-06"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 6)
})


test_that("expand_ex handles complex scenario with multiple subjects and treatments", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG1", "2025-01-01T07:00", "2025-01-02",
    "A", "DRUG2", "2025-01-03T08:00", "2025-01-04",
    "B", "DRUG1", "2025-01-01T09:00", "2025-01-02",
    "B", "DRUG2", "2025-01-03T10:00", "2025-01-04"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 8)
  expect_equal(length(unique(result$USUBJID)), 2)
  expect_equal(length(unique(result$EXTRT)), 2)
})


test_that("expand_ex preserves additional columns", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EXROUTE,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-02", 100, "ORAL"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_true("EXDOSE" %in% names(result))
  expect_true("EXROUTE" %in% names(result))
  expect_equal(result$EXDOSE, rep(100, 2))
  expect_equal(result$EXROUTE, rep("ORAL", 2))
})


test_that("expand_ex handles episode spanning month boundary", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-30T07:00", "2025-02-02T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 4)
  expect_equal(result$IMPUTATION[1], "time copied from EXSTDTC")
  expect_equal(result$IMPUTATION[4], "time copied from EXENDTC")
})


test_that("expand_ex handles episode spanning year boundary", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2024-12-30T07:00", "2025-01-02T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 4)
})


test_that("expand_ex handles episode with EXDY spanning multiple episodes", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSTDY, ~EXENDY,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-02", 1, 2,
    "A", "DRUG", "2025-01-04T08:00", "2025-01-05", 4, 5
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(result$EXDY, c(1, 2, 4, 5))
})


test_that("expand_ex handles single row with EXSTDY and EXENDY", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSTDY, ~EXENDY,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-01T08:00", 1, 1
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$EXDY, 1)
})


test_that("expand_ex errs when end date before start date", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-03T07:00", "2025-01-01"
  ) %>% lubrify_dates()

  expect_error(expand_ex(ex), "End date before start date")
})


test_that("expand_ex errs when end day before start day", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSTDY, ~EXENDY,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03", 3, 1
  ) %>% lubrify_dates()

  expect_error(expand_ex(ex), "End day before start day")
})


test_that("expand_ex handles episode with same start and end date but different times", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-01T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 1)
  expect_equal(result$IMPUTATION, "time copied from EXSTDTC")
})


test_that("expand_ex correctly assigns time from EXENDTC for last row", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  # Last row should use EXENDTC time (08:00), not EXSTDTC time (07:00)
  expect_equal(nrow(result), 3)
  # The DTC for the last row should reflect the end time
  expect_equal(result$DTC_time[3], "08:00")
})


test_that("expand_ex handles imputation when last row has no end time but start time exists", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-03"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  # When there's more than one row and last row has no end time but start time exists,
  # it should be "time carried forward"
  expect_equal(nrow(result), 3)
  expect_equal(result$IMPUTATION[3], "")
})


test_that("expand_ex handles multiple subjects with different episode lengths", {
  ex <- tribble(
    ~USUBJID, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    "A", "DRUG", "2025-01-01T07:00", "2025-01-01T08:00",
    "B", "DRUG", "2025-01-01T07:00", "2025-01-05T08:00"
  ) %>% lubrify_dates()

  result <- expand_ex(ex)

  expect_equal(nrow(result), 6) # 1 + 5
  expect_equal(sum(result$USUBJID == "A"), 1)
  expect_equal(sum(result$USUBJID == "B"), 5)
})










