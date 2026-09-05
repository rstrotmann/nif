## Tests for index_regimen

index_regimen <- nif:::index_regimen


test_that("index_regimen requires a nif object", {
  expect_error(
    index_regimen(data.frame(ID = 1, TIME = 0, EVID = 1, ANALYTE = "A", AMT = 100)),
    "nif object"
  )
})


test_that("index_regimen errors when admin_window is not numeric", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100
  ))

  expect_error(index_regimen(obj, admin_window = "12"), "admin_window")
  expect_error(index_regimen(obj, admin_window = TRUE), "admin_window")
  expect_error(index_regimen(obj, admin_window = NULL), "admin_window")
})


test_that("index_regimen uses default admin_window of 12", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     2,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50
  ))

  expect_equal(
    index_regimen(obj)$REG_ID,
    index_regimen(obj, admin_window = 12)$REG_ID
  )
})


test_that("index_regimen assigns REG_ID, REG, and DL for monotherapy", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
       1,    24,   90,    1,     1,  NA,      "A",     "A",       FALSE,    90,
       1,    25,    0,    2,     0,  20,      "A",     "A",       FALSE,    NA
  ))

  result <- index_regimen(obj)

  expect_true(all(c("REG_ID", "REG", "DL") %in% names(result)))
  expect_equal(result$REG_ID, c(1L, 1L, 1L, 1L))
  expect_equal(unique(result$REG), "A")
  expect_equal(unique(result$DL), "100-A")
})


test_that("index_regimen fills REG_ID, REG, and DL onto observation rows", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
       1,    12,    0,    2,     0,  20,      "A",     "A",       FALSE,    NA
  ))

  result <- index_regimen(obj)
  obs <- result[result$EVID == 0, ]

  expect_false(any(is.na(result$REG_ID)))
  expect_false(any(is.na(result$REG)))
  expect_false(any(is.na(result$DL)))
  expect_equal(unique(obs$REG_ID), 1L)
  expect_equal(unique(obs$REG), "A")
  expect_equal(unique(obs$DL), "100-A")
})


test_that("index_regimen ignores dose modifications within the same analyte set", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,    24,   80,    1,     1,  NA,      "A",     "A",       FALSE,    80,
       1,    48,   60,    1,     1,  NA,      "A",     "A",       FALSE,    60
  ))

  result <- index_regimen(obj)

  expect_equal(result$REG_ID, c(1L, 1L, 1L))
  expect_equal(unique(result$REG), "A")
  expect_equal(unique(result$DL), "100-A")
})


test_that("index_regimen identifies simultaneous combination as a new regimen", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     1,    0,    2,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    12,    0,    2,     0,  20, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    24,   90,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,    90,
       1,    48,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,    48,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,    49,    0,    3,     0,  15, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    49,    0,    4,     0,  25, "DRUG_B", "DRUG_B",       FALSE,    NA,
       1,    72,   80,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,    80,
       1,    72,   40,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    40,
       1,    73,    0,    3,     0,  30, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    73,    0,    4,     0,  35, "DRUG_B", "DRUG_B",       FALSE,    NA
  ))

  result <- index_regimen(obj)

  mono <- result[result$TIME < 48, ]
  combo <- result[result$TIME >= 48, ]

  expect_equal(unique(mono$REG_ID), 1L)
  expect_equal(unique(mono$REG), "DRUG_A")
  expect_equal(unique(mono$DL), "100-DRUG_A")

  expect_equal(unique(combo$REG_ID), 2L)
  expect_equal(unique(combo$REG), "DRUG_A+DRUG_B")
  expect_equal(unique(combo$DL), "100-DRUG_A+50-DRUG_B")
})


test_that("index_regimen treats staggered co-administration within admin_window as one regimen", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     1,    0,    2,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    48,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,    50,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,    51,    0,    3,     0,  15, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    51,    0,    4,     0,  25, "DRUG_B", "DRUG_B",       FALSE,    NA,
       1,    72,   80,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,    80,
       1,    74,   40,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    40,
       1,    75,    0,    3,     0,  30, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    75,    0,    4,     0,  35, "DRUG_B", "DRUG_B",       FALSE,    NA
  ))

  result <- index_regimen(obj, admin_window = 12)

  expect_equal(
    result$REG_ID,
    c(1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)
  )
  expect_equal(unique(result$REG[result$REG_ID == 1]), "DRUG_A")
  expect_equal(unique(result$REG[result$REG_ID == 2]), "DRUG_A+DRUG_B")
  expect_equal(unique(result$DL[result$REG_ID == 2]), "100-DRUG_A+50-DRUG_B")
})


test_that("index_regimen splits staggered administrations outside admin_window", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     2,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50
  ))

  within_window <- index_regimen(obj, admin_window = 12)
  outside_window <- index_regimen(obj, admin_window = 1)

  expect_equal(within_window$REG_ID, c(1L, 1L))
  expect_equal(unique(within_window$REG), "DRUG_A+DRUG_B")
  expect_equal(outside_window$REG_ID, c(1L, 2L))
  expect_equal(outside_window$REG, c("DRUG_A", "DRUG_B"))
})


test_that("index_regimen treats gap equal to admin_window as a new cluster", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,    12,   50,    2,     1,  NA,      "B",     "B",       FALSE,    50
  ))

  at_boundary <- index_regimen(obj, admin_window = 12)
  within_window <- index_regimen(obj, admin_window = 13)

  expect_equal(at_boundary$REG_ID, c(1L, 2L))
  expect_equal(within_window$REG_ID, c(1L, 1L))
  expect_equal(unique(within_window$REG), "A+B")
})


test_that("index_regimen sorts analytes for stable REG signatures", {
  obj_ab <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,    48,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100
  ))

  obj_ba <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,    48,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100
  ))

  expect_equal(index_regimen(obj_ab)$REG_ID, index_regimen(obj_ba)$REG_ID)
  expect_equal(unique(index_regimen(obj_ab)$REG[1]), "DRUG_A+DRUG_B")
  expect_equal(unique(index_regimen(obj_ba)$REG[1]), "DRUG_A+DRUG_B")
})


test_that("index_regimen starts a new REG_ID when returning to monotherapy", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     0,   50,    2,     1,  NA,      "B",     "B",       FALSE,    50,
       1,     1,    0,    3,     0,  10,      "A",     "A",       FALSE,    NA,
       1,    48,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,    49,    0,    3,     0,  20,      "A",     "A",       FALSE,    NA
  ))

  result <- index_regimen(obj)

  expect_equal(result$REG_ID, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(unique(result$REG[result$REG_ID == 1]), "A+B")
  expect_equal(unique(result$REG[result$REG_ID == 2]), "A")
  expect_equal(unique(result$DL[result$REG_ID == 2]), "100-A")
})


test_that("index_regimen assigns REG_ID independently per subject", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
       2,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       2,     0,   50,    2,     1,  NA,      "B",     "B",       FALSE,    50,
       2,     1,    0,    3,     0,  15,      "A",     "A",       FALSE,    NA
  ))

  result <- index_regimen(obj)

  expect_equal(result$REG_ID[result$ID == 1], c(1L, 1L))
  expect_equal(unique(result$REG[result$ID == 1]), "A")
  expect_equal(result$REG_ID[result$ID == 2], c(1L, 1L, 1L))
  expect_equal(unique(result$REG[result$ID == 2]), "A+B")
})


test_that("index_regimen preserves row count", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
       1,    48,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,    48,   50,    2,     1,  NA,      "B",     "B",       FALSE,    50
  ))

  expect_equal(nrow(index_regimen(obj)), nrow(obj))
})


test_that("index_regimen keeps starting DL after later dose modifications in combo", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,    48,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,    48,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,    72,   80,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,    80,
       1,    72,   40,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    40
  ))

  result <- index_regimen(obj, admin_window = 12)
  combo <- result[result$REG_ID == 2, ]

  expect_equal(unique(combo$DL), "100-DRUG_A+50-DRUG_B")
  expect_false("80-DRUG_A+40-DRUG_B" %in% result$DL)
})


test_that("index_regimen builds DL with analytes sorted like REG", {
  obj <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
    1,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
    1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100
  ))
  result <- index_regimen(obj)
  expect_equal(unique(result$REG), "DRUG_A+DRUG_B")
  expect_equal(unique(result$DL), "100-DRUG_A+50-DRUG_B")
})


test_that("index_regimen fills predose observations from the first administration", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID,  ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,    -1,    0,    2,     0,  0.1,      "A",     "A",       FALSE,    NA,
       1,     0,  100,    1,     1,   NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,   10,      "A",     "A",       FALSE,    NA
  ))

  result <- index_regimen(obj)

  expect_equal(result$REG_ID, c(1L, 1L, 1L))
  expect_equal(unique(result$REG), "A")
  expect_equal(unique(result$DL), "100-A")
})


test_that("index_regimen does not fill REG across subjects onto predose rows", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID,  ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,   NA,      "A",     "A",       FALSE,   100,
       1,     0,   50,    2,     1,   NA,      "B",     "B",       FALSE,    50,
       1,     1,    0,    3,     0,   10,      "A",     "A",       FALSE,    NA,
       2,    -1,    0,    2,     0,  0.1,      "A",     "A",       FALSE,    NA,
       2,     0,  100,    1,     1,   NA,      "A",     "A",       FALSE,   100
  ))

  result <- index_regimen(obj)
  id2 <- result[result$ID == 2, ]

  expect_equal(unique(result$REG[result$ID == 1]), "A+B")
  expect_equal(unique(id2$REG), "A")
  expect_equal(id2$REG_ID, c(1L, 1L))
  expect_equal(unique(id2$DL), "100-A")
})


test_that("index_regimen errors when admin_window is not positive", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100
  ))

  expect_error(
    index_regimen(obj, admin_window = 0),
    "admin_window must be positive"
  )
  expect_error(
    index_regimen(obj, admin_window = -1),
    "admin_window must be positive"
  )
})


test_that("index_regimen errors when there are no administrations", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
  ))

  expect_error(index_regimen(obj), "No administrations")
})


test_that("index_regimen replaces existing REG, REG_ID, and DL", {
  obj <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
  )) |>
    mutate(REG = "OLD", REG_ID = 99L, DL = "placeholder")

  result <- index_regimen(obj, silent = TRUE)

  expect_false(
    any(c("REG.x", "REG.y", "REG_ID.x", "REG_ID.y", "DL.x", "DL.y") %in%
          names(result))
  )
  expect_equal(unique(result$REG), "A")
  expect_equal(unique(result$REG_ID), 1L)
  expect_equal(unique(result$DL), "100-A")
})

