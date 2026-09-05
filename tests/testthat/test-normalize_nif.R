as_dtc_nif <- function(df) {
  df |>
    dplyr::mutate(DTC = as.POSIXct(.data$DTC, tz = "UTC")) |>
    as_nif_test()
}


test_that("normalize_nif rejects non-nif input", {
  expect_error(
    normalize_nif(data.frame(
      ID = 1, TIME = 0, AMT = 0, CMT = 1, EVID = 0, DV = 10
    )),
    "Input must be a nif object"
  )

  expect_error(
    normalize_nif(NULL),
    "Input must be a nif object"
  )
})


test_that("normalize_nif rejects nif objects missing essential fields", {
  incomplete <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,
    1,   0,     100,  1,    1
  ))

  expect_error(
    normalize_nif(incomplete),
    "Missing essential fields in nif object: DV"
  )
})


test_that("normalize_nif validates cleanup", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00"
  ))

  expect_error(
    normalize_nif(test_nif, cleanup = "TRUE"),
    "cleanup must be a logical value"
  )

  expect_error(
    normalize_nif(test_nif, cleanup = 1),
    "cleanup must be a logical value"
  )

  expect_error(
    normalize_nif(test_nif, cleanup = c(TRUE, FALSE)),
    "cleanup must be a single value"
  )

  expect_error(
    normalize_nif(test_nif, cleanup = NA),
    "cleanup must not contain NA"
  )
})


test_that("normalize_nif validates keep", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00"
  ))

  expect_error(
    normalize_nif(test_nif, keep = 1),
    "keep must be a character value"
  )

  expect_error(
    normalize_nif(test_nif, keep = TRUE),
    "keep must be a character value"
  )

  expect_error(
    normalize_nif(test_nif, keep = NA_character_),
    "keep must not contain NA"
  )

  expect_error(
    normalize_nif(test_nif, keep = ""),
    "keep must be a non-empty string"
  )

  expect_error(
    normalize_nif(test_nif, keep = c("CUSTOM", NA_character_)),
    "keep must not contain NA"
  )

  expect_error(
    normalize_nif(test_nif, cleanup = FALSE, keep = 1),
    "keep must be a character value"
  )
})


test_that("normalize_nif returns a nif object with the same number of rows", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00",
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_s3_class(result, "nif")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(test_nif))
})


test_that("normalize_nif reindexes ID from USUBJID", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-002",  88,  0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  99,  0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  99,  1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(sort(unique(result$ID)), c(1, 2))
  expect_true(all(result$ID[result$USUBJID == "SUBJ-001"] == 1))
  expect_true(all(result$ID[result$USUBJID == "SUBJ-002"] == 2))
  expect_false(any(result$ID %in% c(88, 99)))
})


test_that("normalize_nif combines STUDYID with USUBJID when assigning ID", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~STUDYID, ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "S1",     "001",    1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "S2",     "001",    1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(length(unique(result$ID)), 2)
  expect_false(
    unique(result$ID[result$STUDYID == "S1"]) ==
      unique(result$ID[result$STUDYID == "S2"])
  )
})


test_that("normalize_nif recalculates TIME, TAFD, and TAD from DTC", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   99,    100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   99,    0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00",
    "SUBJ-001",  1,   99,    0,    2,    0,     8,        "DRUG",   "DRUG",  "2024-01-01 11:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$TIME, c(0, 1, 3))
  expect_equal(result$TAFD, c(0, 1, 3))
  expect_equal(result$TAD, c(0, 1, 3))
})


test_that("normalize_nif computes TIME per subject from the first record", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   0,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 10:00:00",
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 12:00:00",
    "SUBJ-002",  2,   0,     0,    2,    0,     20,       "DRUG",   "DRUG",  "2024-01-01 13:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$TIME[result$USUBJID == "SUBJ-001"], c(0, 2))
  expect_equal(result$TIME[result$USUBJID == "SUBJ-002"], c(0, 1))
})


test_that("normalize_nif computes TAD relative to the most recent dose", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   0,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00",
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 11:00:00",
    "SUBJ-001",  1,   0,     0,    2,    0,     8,        "DRUG",   "DRUG",  "2024-01-01 12:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$TAFD, c(0, 1, 3, 4))
  expect_equal(result$TAD, c(0, 1, 0, 1))
})


test_that("normalize_nif computes negative TAFD and TAD for pre-dose observations", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     0,    2,    0,     5,        "DRUG",   "DRUG",  "2024-01-01 07:00:00",
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   0,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$TIME, c(0, 1, 2))
  expect_equal(result$TAFD, c(-1, 0, 1))
  expect_equal(result$TAD, c(-1, 0, 1))
})


test_that("normalize_nif errors when make_time required columns are missing", {
  missing_parent <- as_nif_test(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
  ))

  expect_error(
    normalize_nif(missing_parent),
    "Missing required columns: PARENT"
  )

  missing_dtc <- as_nif_test(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG"
  ))

  expect_error(
    normalize_nif(missing_dtc),
    "Missing required columns: DTC"
  )
})


test_that("normalize_nif errors when DTC is not POSIXct", {
  test_nif <- as_nif_test(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00"
  ))

  expect_error(
    normalize_nif(test_nif),
    "DTC column must contain POSIXct datetime values"
  )
})


test_that("normalize_nif fills subject-level fields within ID and PARENT", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~AGE,     ~SEX,          ~RACE,          ~WEIGHT,  ~DOSE,     ~FOOD, ~FASTED,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 45,       "M",           "WHITE",        70,       100,       TRUE,  FALSE,
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_, NA_character_, NA_character_, NA_real_, NA_real_, NA,    NA
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$AGE, c(45, 45))
  expect_equal(result$SEX, c("M", "M"))
  expect_equal(result$RACE, c("WHITE", "WHITE"))
  expect_equal(result$WEIGHT, c(70, 70))
  expect_equal(result$DOSE, c(100, 100))
  expect_equal(result$FOOD, c(TRUE, TRUE))
  expect_equal(result$FASTED, c(FALSE, FALSE))
})


test_that("normalize_nif fills subject-level fields in both time directions", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~AGE,
    "SUBJ-001",  1,   0,     0,    2,    0,     5,        "DRUG",   "DRUG",  "2024-01-01 07:00:00", NA_real_,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 45,
    "SUBJ-001",  1,   0,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$AGE, c(45, 45, 45))
})


test_that("normalize_nif does not overwrite non-missing filled fields", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~AGE,     ~DOSE,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 45,       100,
    "SUBJ-001",  1,   0,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_, NA_real_,
    "SUBJ-001",  1,   0,     50,   1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 11:00:00", 50,       50
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$AGE, c(45, 45, 50))
  expect_equal(result$DOSE, c(100, 100, 50))
})


test_that("normalize_nif does not fill subject-level fields across subjects", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~AGE,     ~SEX,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 45,       "M",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_, NA_character_,
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", NA_real_, NA_character_,
    "SUBJ-002",  2,   1,     0,    2,    0,     8,        "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_, NA_character_
  ))

  result <- normalize_nif(test_nif)

  expect_equal(unique(result$AGE[result$USUBJID == "SUBJ-001"]), 45)
  expect_equal(unique(result$SEX[result$USUBJID == "SUBJ-001"]), "M")
  expect_true(all(is.na(result$AGE[result$USUBJID == "SUBJ-002"])))
  expect_true(all(is.na(result$SEX[result$USUBJID == "SUBJ-002"])))
})


test_that("normalize_nif does not fill fields across PARENT groups", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~DOSE,    ~AGE,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 100,      45,
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_, NA_real_,
    "SUBJ-001",  1,   2,     50,   3,    1,     NA_real_, "OTHER",  "OTHER", "2024-01-01 10:00:00", NA_real_, NA_real_,
    "SUBJ-001",  1,   3,     0,    4,    0,     2,        "OTHER",  "OTHER", "2024-01-01 11:00:00", NA_real_, NA_real_
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$DOSE[result$PARENT == "DRUG"], c(100, 100))
  expect_equal(result$AGE[result$PARENT == "DRUG"], c(45, 45))
  expect_true(all(is.na(result$DOSE[result$PARENT == "OTHER"])))
  expect_true(all(is.na(result$AGE[result$PARENT == "OTHER"])))
})


test_that("normalize_nif fills baseline columns and keeps them after cleanup", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~BL_WEIGHT, ~BL_HEIGHT,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 70,         NA_real_,
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_,   180
  ))

  result <- normalize_nif(test_nif)

  expect_true("BL_WEIGHT" %in% names(result))
  expect_true("BL_HEIGHT" %in% names(result))
  expect_equal(result$BL_WEIGHT, c(70, 70))
  expect_equal(result$BL_HEIGHT, c(180, 180))
})


test_that("normalize_nif does not fill baseline columns across subjects", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~BL_WEIGHT,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 70,
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_,
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", NA_real_
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$BL_WEIGHT[result$USUBJID == "SUBJ-001"], c(70, 70))
  expect_true(is.na(result$BL_WEIGHT[result$USUBJID == "SUBJ-002"]))
})


test_that("normalize_nif works when no baseline columns are present", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~AGE,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 45,
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_real_
  ))

  result <- normalize_nif(test_nif)

  expect_s3_class(result, "nif")
  expect_false(any(grepl("^BL_", names(result))))
  expect_equal(result$AGE, c(45, 45))
})


test_that("normalize_nif does not fill IMPUTATION", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~IMPUTATION,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", "time carried forward",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", NA_character_
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$IMPUTATION, c("time carried forward", NA_character_))
})


test_that("normalize_nif removes non-essential columns when cleanup is TRUE", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~CUSTOM, ~SRC_DOMAIN, ~VISIT, ~ETHNIC,        ~COUNTRY, ~ARM,    ~EPOCH,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", "x",     "EX",        "V1",   "NOT HISPANIC", "USA",    "TRT",  "TREATMENT",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", "y",     "PC",        "V1",   NA_character_,  NA_character_, NA_character_, NA_character_
  ))

  result <- normalize_nif(test_nif, cleanup = TRUE)

  expect_false("CUSTOM" %in% names(result))
  expect_false("SRC_DOMAIN" %in% names(result))
  expect_false("VISIT" %in% names(result))
  expect_false("ETHNIC" %in% names(result))
  expect_false("COUNTRY" %in% names(result))
  expect_false("ARM" %in% names(result))
  expect_false("EPOCH" %in% names(result))
  expect_true(all(c("ID", "USUBJID", "DTC", "TIME", "TAFD", "TAD", "EVID", "AMT",
                    "CMT", "DV", "ANALYTE", "PARENT") %in% names(result)))
})


test_that("normalize_nif keeps non-essential columns when cleanup is FALSE", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~CUSTOM, ~SRC_DOMAIN, ~ETHNIC,        ~COUNTRY, ~ARM,   ~EPOCH,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", "x",     "EX",        "NOT HISPANIC", "USA",    "TRT", "TREATMENT",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", "y",     "PC",        NA_character_,  NA_character_, NA_character_, NA_character_
  ))

  result <- normalize_nif(test_nif, cleanup = FALSE)

  expect_true("CUSTOM" %in% names(result))
  expect_true("SRC_DOMAIN" %in% names(result))
  expect_true("ETHNIC" %in% names(result))
  expect_true("COUNTRY" %in% names(result))
  expect_true("ARM" %in% names(result))
  expect_true("EPOCH" %in% names(result))
  expect_equal(result$ETHNIC, c("NOT HISPANIC", "NOT HISPANIC"))
  expect_equal(result$COUNTRY, c("USA", "USA"))
  expect_equal(result$ARM, c("TRT", "TRT"))
  expect_equal(result$EPOCH, c("TREATMENT", "TREATMENT"))
})


test_that("normalize_nif keep retains requested extra columns during cleanup", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~CUSTOM, ~VISIT, ~SRC_DOMAIN,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", "x",     "V1",   "EX",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", "y",     "V2",   "PC"
  ))

  result <- normalize_nif(test_nif, keep = c("CUSTOM", "VISIT"))

  expect_true("CUSTOM" %in% names(result))
  expect_true("VISIT" %in% names(result))
  expect_false("SRC_DOMAIN" %in% names(result))
  expect_equal(result$CUSTOM, c("x", "y"))
})


test_that("normalize_nif keep ignores names that are not present", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~CUSTOM,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", "x"
  ))

  result <- normalize_nif(test_nif, keep = c("CUSTOM", "NOT_A_COLUMN"))

  expect_true("CUSTOM" %in% names(result))
  expect_false("NOT_A_COLUMN" %in% names(result))
})


test_that("normalize_nif keep has no effect when cleanup is FALSE", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~CUSTOM, ~VISIT,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", "x",     "V1"
  ))

  kept <- normalize_nif(test_nif, cleanup = FALSE, keep = "CUSTOM")
  all_cols <- normalize_nif(test_nif, cleanup = FALSE)

  expect_true("VISIT" %in% names(kept))
  expect_setequal(names(kept), names(all_cols))
})


test_that("normalize_nif keep = NULL matches the default cleanup", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~CUSTOM,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", "x"
  ))

  result_null <- normalize_nif(test_nif, keep = NULL)
  result_default <- normalize_nif(test_nif)

  expect_equal(names(result_null), names(result_default))
  expect_false("CUSTOM" %in% names(result_null))
})


test_that("normalize_nif preserves NTIME, METABOLITE, PERIOD, and MDV through cleanup", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~NTIME, ~AMT, ~CMT, ~EVID, ~DV,      ~MDV, ~ANALYTE, ~PARENT, ~METABOLITE, ~PERIOD, ~DTC,
    "SUBJ-001",  1,   0,     0,      100,  1,    1,     NA_real_, 1,    "DRUG",   "DRUG",  FALSE,       1,       "2024-01-01 08:00:00",
    "SUBJ-001",  1,   1,     1,      0,    2,    0,     10,       0,    "DRUG",   "DRUG",  FALSE,       1,       "2024-01-01 09:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_true(all(c("NTIME", "METABOLITE", "PERIOD", "MDV") %in% names(result)))
  expect_equal(result$NTIME, c(0, 1))
  expect_equal(result$METABOLITE, c(FALSE, FALSE))
  expect_equal(result$PERIOD, c(1, 1))
  expect_equal(result$MDV, c(1, 0))
})


test_that("normalize_nif adds sequential REF and places it first", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(names(result)[1], "REF")
  expect_equal(result$REF, seq_len(nrow(result)))
})


test_that("normalize_nif orders rows by ID, TIME, and EVID with doses first", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-002",  2,   0,     0,    2,    0,     8,        "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   0,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(result$USUBJID, c("SUBJ-001", "SUBJ-001", "SUBJ-002", "SUBJ-002"))
  expect_equal(result$EVID, c(1, 0, 1, 0))
  expect_equal(result$ID, c(1, 1, 2, 2))
})


test_that("normalize_nif puts standard columns in canonical order", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~DV, ~ANALYTE, ~PARENT, ~EVID, ~AMT, ~CMT, ~TIME, ~ID, ~USUBJID,    ~DTC,                  ~AGE,
    NA_real_, "DRUG", "DRUG", 1, 100, 1, 0, 1, "SUBJ-001", "2024-01-01 08:00:00", 45,
    10,       "DRUG", "DRUG", 0, 0,   2, 1, 1, "SUBJ-001", "2024-01-01 09:00:00", NA_real_
  ))

  result <- normalize_nif(test_nif)
  present <- intersect(
    c("REF", "ID", "USUBJID", "AGE", "DTC", "TIME", "TAFD", "TAD",
      "EVID", "AMT", "CMT", "DV", "ANALYTE", "PARENT"),
    names(result)
  )

  expect_equal(match(present, names(result)), sort(match(present, names(result))))
  expect_equal(present[1], "REF")
})


test_that("normalize_nif places baseline columns after standard fields", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~BL_WEIGHT,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 70
  ))

  result <- normalize_nif(test_nif)

  expect_gt(match("BL_WEIGHT", names(result)), match("PARENT", names(result)))
})


test_that("normalize_nif handles a single-row nif", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  7,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
  expect_equal(result$REF, 1)
  expect_equal(result$TIME, 0)
  expect_equal(result$TAFD, 0)
  expect_equal(result$TAD, 0)
})


test_that("normalize_nif handles an empty nif with required columns", {
  test_nif <- as_nif_test(tibble::tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~DTC
  ))

  result <- normalize_nif(test_nif)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), 0)
  expect_true(all(c("REF", "ID", "TIME", "TAFD", "TAD") %in% names(result)))
})


test_that("normalize_nif keeps all original rows for multiple subjects", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00",
    "SUBJ-001",  1,   2,     0,    2,    0,     8,        "DRUG",   "DRUG",  "2024-01-01 10:00:00",
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-003",  3,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00",
    "SUBJ-003",  3,   1,     0,    2,    0,     12,       "DRUG",   "DRUG",  "2024-01-01 09:00:00"
  ))

  result <- normalize_nif(test_nif)

  expect_equal(nrow(result), 6)
  expect_equal(sort(unique(result$ID)), c(1, 2, 3))
  expect_equal(as.numeric(table(result$USUBJID)), c(3, 1, 2))
})


test_that("normalize_nif is stable for the same input", {
  test_nif <- as_dtc_nif(tibble::tribble(
    ~USUBJID,    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,      ~ANALYTE, ~PARENT, ~DTC,                  ~AGE,     ~CUSTOM,
    "SUBJ-002",  2,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", 30,       "a",
    "SUBJ-001",  1,   0,     100,  1,    1,     NA_real_, "DRUG",   "DRUG",  "2024-01-01 08:00:00", NA_real_, "b",
    "SUBJ-001",  1,   1,     0,    2,    0,     10,       "DRUG",   "DRUG",  "2024-01-01 09:00:00", 45,       "c"
  ))

  result1 <- normalize_nif(test_nif, keep = "CUSTOM")
  result2 <- normalize_nif(test_nif, keep = "CUSTOM")

  expect_equal(result1, result2)
})


test_that("normalize_nif works on package example data", {
  result <- normalize_nif(examplinib_sad_nif)

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), nrow(examplinib_sad_nif))
  expect_equal(result$REF, seq_len(nrow(result)))
  expect_true(all(c("TIME", "TAFD", "TAD") %in% names(result)))
  expect_false(any(is.na(result$ID)))
})
