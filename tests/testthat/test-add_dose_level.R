## Tests for add_dose_level


test_that("add_dose_level requires a nif object", {
  expect_error(
    add_dose_level(data.frame(ID = 1, TIME = 0, AMT = 1, CMT = 1, EVID = 1, DV = NA)),
    "Input must be a nif object"
  )
})


test_that("add_dose_level uses starting dose and ignores later dose changes", {
  obj <- nif(tibble::tribble(
       ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
         1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
         1,    24,   90,    1,     1,  NA,      "A",     "A",       FALSE,    90,
         1,    48,   80,    1,     1,  NA,      "A",     "A",       FALSE,    80,
         1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
         2,     0,  200,    1,     1,  NA,      "A",     "A",       FALSE,   200,
         3,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
         3,    24,   NA,    1,     1,  NA,      "A",     "A",       FALSE,    NA,
         3,    48,  150,    1,     1,  NA,      "A",     "A",       FALSE,   150
      ))

  result <- add_dose_level(obj)

  expect_equal(
    as.data.frame(distinct(result, ID, DL)),
    as.data.frame(tibble::tribble(
      ~ID, ~DL,
         1, "100-A",
         2, "200-A",
         3, "100-A"
    ))
  )
  expect_type(result$DL, "character")
})


test_that("add_dose_level returns character DL for single-analyte data", {
  result <- add_dose_level(examplinib_sad_nif)

  expect_true("DL" %in% names(result))
  expect_type(result$DL, "character")
  expect_setequal(
    unique(result$DL),
    c(
      "5-RS2023", "10-RS2023", "20-RS2023", "50-RS2023", "100-RS2023",
      "200-RS2023", "500-RS2023", "800-RS2023", "1000-RS2023"
    )
  )
})


test_that("add_dose_level matches dose_levels for example data when DOSE equals AMT", {
  result <- add_dose_level(examplinib_sad_nif)

  expected <- dose_levels(examplinib_sad_nif)$RS2023
  dl_numeric <- as.numeric(sub("-.*$", "", unique(result$DL)))
  expect_true(all(sort(dl_numeric) %in% expected))
})


test_that("add_dose_level builds combination DL from starting doses", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,     1,    0,    3,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA,
       2,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       2,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50
     )
  )

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL), "100-DRUG_A+50-DRUG_B")
  expect_type(result$DL, "character")
})


test_that("add_dose_level works on minimal nif via ensure helpers", {
  result <- add_dose_level(examplinib_sad_min_nif)

  expect_true("DL" %in% names(result))
  expect_type(result$DL, "character")
  expect_gt(length(unique(result$DL)), 1)
})


test_that("add_dose_level inherits nif class", {
  result <- add_dose_level(examplinib_sad_nif)

  expect_s3_class(result, "nif")
})


test_that("add_dose_level preserves row count when starting doses are duplicated", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
    ))

  result <- add_dose_level(obj)

  expect_equal(nrow(result), nrow(obj))
})


test_that("add_dose_level preserves row count when starting doses conflict at same time", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     0,  200,    1,     1,  NA,      "A",     "A",       FALSE,   200,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
    ))

  expect_error(
    result <- suppressWarnings(add_dose_level(obj)),
    "Dose level cannot be determined"
  )
})


test_that("add_dose_level uses character DL for single-drug subjects in mixed studies", {
  obj <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
     1,    0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
     1,    1,    0,    2,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA,
     2,    0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
     2,    0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
     2,    1,    0,    3,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA
  ))

  result <- add_dose_level(obj)

  expect_equal(result$DL[result$ID == 1][1], "100-DRUG_A")
})


test_that("add_dose_level is idempotent when DL already exists", {
  obj <- add_dose_level(examplinib_sad_nif)

  result <- add_dose_level(obj, silent = TRUE)

  expect_true("DL" %in% names(result))
  expect_false(any(c("DL.x", "DL.y") %in% names(result)))
  expect_equal(result$DL, obj$DL)
})


test_that("add_dose_level agrees with dose_levels when DOSE and AMT differ", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   999,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
     )
)

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL), "100-A")
})


test_that("add_dose_level orders combination components by administered analytes", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,     1,    0,    3,     0,  10, "DRUG_B", "DRUG_B",       FALSE,    NA
     ))

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL), "50-DRUG_B+100-DRUG_A")
})


test_that("add_dose_level errors when no qualifying administrations exist", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
     ))

  expect_error(add_dose_level(obj), "No administrations")
})

