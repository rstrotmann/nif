test_that("add_bl_odwg classifies all hepatic function categories correctly", {
  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~SEX, ~ACTARMCD,   ~RFXSTDTC,
    "1",      "DM",    "M",  "TREATMENT", "2001-01-01T10:29",
    "2",      "DM",    "M",  "TREATMENT", "2001-01-02T09:09",
    "3",      "DM",    "M",  "TREATMENT", "2000-12-29T09:07",
    "4",      "DM",    "F",  "TREATMENT", "2001-01-06T11:18",
    "5",      "DM",    "F",  "TREATMENT", "2001-01-07T11:18"
  ) %>%
    mutate(RFSTDTC = RFXSTDTC)

  vs <- tribble(
    ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSBLFL, ~VSSTRESN,
    "1",      "VS",    "HEIGHT",  "Y",     190.8,
    "1",      "VS",    "WEIGHT",  "Y",     79.3,
    "2",      "VS",    "HEIGHT",  "Y",     199.5,
    "2",      "VS",    "WEIGHT",  "Y",     81.6,
    "3",      "VS",    "HEIGHT",  "Y",     185.4,
    "3",      "VS",    "WEIGHT",  "Y",     92.8,
    "4",      "VS",    "HEIGHT",  "Y",     177.8,
    "4",      "VS",    "WEIGHT",  "Y",     83.3,
    "5",      "VS",    "HEIGHT",  "Y",     177.9,
    "5",      "VS",    "WEIGHT",  "Y",     83.4
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBDTC,           ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "SERUM", "2001-01-01T10:29", "Y",    "BILI",    1,         1,        # normal: BILI <= 1, AST <= 1
    "1",      "LB",    "SERUM", "2001-01-01T10:29", "Y",    "AST",     1,         1,
    "2",      "LB",    "SERUM", "2001-01-02T09:09", "Y",    "BILI",    1.5,       1,        # mild: BILI > 1 to 1.5
    "2",      "LB",    "SERUM", "2001-01-02T09:09", "Y",    "AST",     1,         1,
    "3",      "LB",    "SERUM", "2000-12-29T09:07", "Y",    "BILI",    1,         1,        # mild: AST > 1
    "3",      "LB",    "SERUM", "2000-12-29T09:07", "Y",    "AST",     1.2,       1,
    "4",      "LB",    "SERUM", "2001-01-06T11:18", "Y",    "BILI",    3,         1,        # moderate: BILI > 1.5 to 3
    "4",      "LB",    "SERUM", "2001-01-06T11:18", "Y",    "AST",     1,         1,
    "5",      "LB",    "SERUM", "2001-01-07T11:18", "Y",    "BILI",    3.1,       1,        # severe: BILI > 3 to 10
    "5",      "LB",    "SERUM", "2001-01-07T11:18", "Y",    "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, vs = vs, lb = lb))

  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA,
    "2",      2,   0,     0,    1,    0,     NA,
    "3",      3,   0,     0,    1,    0,     NA,
    "4",      4,   0,     0,    1,    0,     NA,
    "5",      5,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_true("BL_ODWG" %in% names(result))
  expect_true("BL_BILI_X_ULN" %in% names(result))
  expect_true("BL_AST_X_ULN" %in% names(result))
  expect_equal(
    as.character(result$BL_ODWG),
    c("normal", "mild", "mild", "moderate", "severe")
  )
  expect_s3_class(result$BL_ODWG, "factor")
  expect_equal(levels(result$BL_ODWG), c("normal", "mild", "moderate", "severe"))
})


test_that("add_bl_odwg handles missing LB domain gracefully", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )
  sdtm <- sdtm(list(dm = dm))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg handles missing BILI marker", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg handles missing AST marker", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg handles missing both BILI and AST markers", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "CREAT",   1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg handles missing LBSTRESN field", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,
    "1",      "LB",    "Y",     "AST",     1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg handles missing LBSTNRHI field", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN,
    "1",      "LB",    "Y",     "BILI",    1,
    "1",      "LB",    "Y",     "AST",     1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg handles missing both required fields", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD,
    "1",      "LB",    "Y",     "BILI",
    "1",      "LB",    "Y",     "AST"
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg handles missing baseline flag columns", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "BILI",    1,         1,
    "1",      "LB",    "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg works with LBLOBXFL instead of LBBLFL", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBLOBXFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",       "BILI",    1,         1,
    "1",      "LB",    "Y",       "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_true("BL_ODWG" %in% names(result))
  expect_equal(as.character(result$BL_ODWG), "normal")
})


test_that("add_bl_odwg handles empty results after filtering", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "N",     "BILI",    1,         1,
    "1",      "LB",    "N",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_false("BL_ODWG" %in% names(result))
  expect_equal(result, test_nif)
})


test_that("add_bl_odwg works with custom baseline_filter", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "N",     "BILI",    1,         1,
    "1",      "LB",    "N",     "AST",     1,         1,
    "1",      "LB",    "Y",     "BILI",    1.5,       1,
    "1",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(
    test_nif, sdtm,
    baseline_filter = "LBBLFL == 'Y'",
    silent = TRUE
  )

  expect_true("BL_ODWG" %in% names(result))
  expect_equal(as.character(result$BL_ODWG), "mild")
})


test_that("add_bl_odwg works with custom observation_filter", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "SERUM",  "Y",     "BILI",    1,         1,
    "1",      "LB",    "SERUM",  "Y",     "AST",     1,         1,
    "1",      "LB",    "URINE",  "Y",     "BILI",    2,         1,
    "1",      "LB",    "URINE",  "Y",     "AST",     2,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(
    test_nif, sdtm,
    observation_filter = "LBSPEC == 'SERUM'",
    silent = TRUE
  )

  expect_true("BL_ODWG" %in% names(result))
  expect_equal(as.character(result$BL_ODWG), "normal")
  expect_equal(result$BL_BILI_X_ULN, 1)
  expect_equal(result$BL_AST_X_ULN, 1)
})


test_that("add_bl_odwg automatically excludes URINE when LBSPEC contains URINE", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "SERUM",  "Y",     "BILI",    1,         1,
    "1",      "LB",    "SERUM",  "Y",     "AST",     1,         1,
    "1",      "LB",    "URINE",  "Y",     "BILI",    2,         1,
    "1",      "LB",    "URINE",  "Y",     "AST",     2,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_true("BL_ODWG" %in% names(result))
  expect_equal(as.character(result$BL_ODWG), "normal")
  expect_equal(result$BL_BILI_X_ULN, 1)
  expect_equal(result$BL_AST_X_ULN, 1)
})


test_that("add_bl_odwg handles multiple baseline values with summary_function", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,         1,
    "1",      "LB",    "Y",     "BILI",    1.5,       1,
    "1",      "LB",    "Y",     "AST",     1,         1,
    "1",      "LB",    "Y",     "AST",     1.2,       1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  # Test with mean (default)
  result_mean <- add_bl_odwg(test_nif, sdtm, summary_function = mean, silent = TRUE)
  expect_equal(result_mean$BL_BILI_X_ULN, 1.25)  # mean of 1 and 1.5
  expect_equal(result_mean$BL_AST_X_ULN, 1.1)     # mean of 1 and 1.2

  # Test with max
  result_max <- add_bl_odwg(test_nif, sdtm, summary_function = max, silent = TRUE)
  expect_equal(result_max$BL_BILI_X_ULN, 1.5)
  expect_equal(result_max$BL_AST_X_ULN, 1.2)

  # Test with min
  result_min <- add_bl_odwg(test_nif, sdtm, summary_function = min, silent = TRUE)
  expect_equal(result_min$BL_BILI_X_ULN, 1)
  expect_equal(result_min$BL_AST_X_ULN, 1)
})


test_that("add_bl_odwg handles division by zero in LBSTNRHI", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,         0,
    "1",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  # Should handle division by zero (result will be Inf or NA)
  expect_true("BL_ODWG" %in% names(result))
  expect_true(is.infinite(result$BL_BILI_X_ULN) || is.na(result$BL_BILI_X_ULN))
})


test_that("add_bl_odwg handles NA values in LBSTNRHI", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,         NA,
    "1",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  expect_error(
    result <- add_bl_odwg(test_nif, sdtm, silent = TRUE),
    "No valid baseline values found"
  )

})


test_that("add_bl_odwg handles edge cases for classification boundaries", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA,
    "2",      2,   0,     0,    1,    0,     NA,
    "3",      3,   0,     0,    1,    0,     NA,
    "4",      4,   0,     0,    1,    0,     NA,
    "5",      5,   0,     0,    1,    0,     NA,
    "6",      6,   0,     0,    1,    0,     NA,
    "7",      7,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT",
    "2",      "DM",    "TREATMENT",
    "3",      "DM",    "TREATMENT",
    "4",      "DM",    "TREATMENT",
    "5",      "DM",    "TREATMENT",
    "6",      "DM",    "TREATMENT",
    "7",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,         1,        # exactly 1 (normal)
    "1",      "LB",    "Y",     "AST",     1,         1,
    "2",      "LB",    "Y",     "BILI",    1.0001,    1,        # just above 1 (mild)
    "2",      "LB",    "Y",     "AST",     1,         1,
    "3",      "LB",    "Y",     "BILI",    1.5,       1,        # exactly 1.5 (mild)
    "3",      "LB",    "Y",     "AST",     1,         1,
    "4",      "LB",    "Y",     "BILI",    1.5001,    1,        # just above 1.5 (moderate)
    "4",      "LB",    "Y",     "AST",     1,         1,
    "5",      "LB",    "Y",     "BILI",    3,         1,        # exactly 3 (moderate)
    "5",      "LB",    "Y",     "AST",     1,         1,
    "6",      "LB",    "Y",     "BILI",    3.0001,    1,        # just above 3 (severe)
    "6",      "LB",    "Y",     "AST",     1,         1,
    "7",      "LB",    "Y",     "BILI",    10,        1,        # exactly 10 (severe)
    "7",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_equal(
    as.character(result$BL_ODWG),
    c("normal", "mild", "mild", "moderate", "moderate", "severe", "severe")
  )
})


test_that("add_bl_odwg handles BILI > 10 (should be NA)", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    11,        1,
    "1",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_true("BL_ODWG" %in% names(result))
  expect_true(is.na(result$BL_ODWG))
})


test_that("add_bl_odwg handles missing values in BL_BILI_X_ULN or BL_AST_X_ULN", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    NA,        1,
    "1",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  expect_error(
    result <- add_bl_odwg(test_nif, sdtm, silent = TRUE),
    "No valid baseline values found for test code 'BILI_X_ULN'",
  )
})


test_that("add_bl_odwg respects silent parameter", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    "1",      1,   0,     0,    1,    0,     NA
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,         1,
    "1",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  # Should not produce warnings when silent = TRUE
  expect_no_warning(
    result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)
  )

  expect_true("BL_ODWG" %in% names(result))
})


test_that("add_bl_odwg preserves original nif object structure", {
  test_nif <- tribble(
    ~USUBJID, ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~OTHER_COL,
    "1",      1,   0,     0,    1,    0,     NA,  "value1",
    "2",      2,   0,     0,    1,    0,     NA,  "value2"
  )
  class(test_nif) <- c("nif", "data.frame")

  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~ACTARMCD,
    "1",      "DM",    "TREATMENT",
    "2",      "DM",    "TREATMENT"
  )

  lb <- tribble(
    ~USUBJID, ~DOMAIN, ~LBBLFL, ~LBTESTCD, ~LBSTRESN, ~LBSTNRHI,
    "1",      "LB",    "Y",     "BILI",    1,         1,
    "1",      "LB",    "Y",     "AST",     1,         1,
    "2",      "LB",    "Y",     "BILI",    1.5,       1,
    "2",      "LB",    "Y",     "AST",     1,         1
  )

  sdtm <- sdtm(list(dm = dm, lb = lb))

  result <- add_bl_odwg(test_nif, sdtm, silent = TRUE)

  expect_true("OTHER_COL" %in% names(result))
  expect_equal(result$OTHER_COL, test_nif$OTHER_COL)
  expect_s3_class(result, "nif")
  expect_equal(nrow(result), nrow(test_nif))
})

