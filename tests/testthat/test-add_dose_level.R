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


test_that("add_dose_level builds combination DL from the starting regimen", {
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


test_that("add_dose_level keeps starting DL when a later regimen change occurs", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
       1,    24,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,    24,   50,    2,     1,  NA,      "B",     "B",       FALSE,    50,
       1,    25,    0,    2,     0,  20,      "A",     "A",       FALSE,    NA
    ))

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL), "100-A")
  expect_false("100-A+50-B" %in% result$DL)
})


test_that("add_dose_level treats staggered co-administration as starting combination DL", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     2,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,     3,    0,    3,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA
    ))

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL), "100-DRUG_A+50-DRUG_B")
})


test_that("add_dose_level sorts combination components by analyte name", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     1,    0,    3,     0,  10, "DRUG_B", "DRUG_B",       FALSE,    NA
     ))

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL), "100-DRUG_A+50-DRUG_B")
})


test_that("add_dose_level uses character DL for single-drug subjects in mixed studies", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     1,    0,    2,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA,
       2,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       2,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       2,     1,    0,    3,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA
  ))

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL[result$ID == 1]), "100-DRUG_A")
  expect_equal(unique(result$DL[result$ID == 2]), "100-DRUG_A+50-DRUG_B")
})


test_that("add_dose_level does not retain REG_ID or REG", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
    ))

  result <- add_dose_level(obj)

  expect_false(any(c("REG_ID", "REG") %in% names(result)))
  expect_true("DL" %in% names(result))
})


test_that("add_dose_level works on minimal nif via ensure helpers", {
  result <- add_dose_level(examplinib_sad_min_nif)

  expect_true("DL" %in% names(result))
  expect_type(result$DL, "character")
  expect_gt(length(unique(result$DL)), 1)
})


test_that("add_dose_level errors on duplicate administrations at the same time", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
    ))

  expect_error(add_dose_level(obj), "Dose level cannot be determined")
})


test_that("add_dose_level errors when starting doses conflict at the same time", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     0,  200,    1,     1,  NA,      "A",     "A",       FALSE,   200,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
    ))

  expect_error(add_dose_level(obj), "Dose level cannot be determined")
})


test_that("add_dose_level replaces existing DL without creating join suffixes", {
  obj <- examplinib_sad_nif |>
    mutate(DL = "placeholder")

  result <- add_dose_level(obj, silent = TRUE)

  expect_true("DL" %in% names(result))
  expect_false(any(c("DL.x", "DL.y") %in% names(result)))
  expect_false("placeholder" %in% result$DL)
})


test_that("add_dose_level replaces existing REG and REG_ID columns", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
    )) |>
    mutate(REG = "OLD", REG_ID = 99L)

  result <- add_dose_level(obj, silent = TRUE)

  expect_false(any(c("REG", "REG_ID", "REG.x", "REG.y") %in% names(result)))
  expect_equal(unique(result$DL), "100-A")
})


test_that("add_dose_level uses AMT rather than DOSE when they differ", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   999,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
     )
)

  result <- add_dose_level(obj)

  expect_equal(unique(result$DL), "100-A")
})


test_that("add_dose_level errors when no qualifying administrations exist", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
     ))

  expect_error(add_dose_level(obj), "No administrations")
})
