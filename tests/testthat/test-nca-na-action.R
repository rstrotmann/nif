nca_pp <- function(obj, param, ...) {
  result <- suppressWarnings(
    nca(obj, analyte = "DRUG", parent = "DRUG", silent = TRUE, ...)
  )
  unname(result$PPORRES[result$PPTESTCD == param])
}


midpoint_nif <- function(mid_dv) {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,   1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     2,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  obj$DV[obj$TIME == 2 & obj$EVID == 0] <- mid_dv
  obj
}


omit_midpoint_nif <- function() {
  tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,   1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     2,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)
}


test_that("nca() default excludes missing observation DV like omitting the row", {
  expect_equal(
    nca_pp(midpoint_nif(NA_real_), "auclast"),
    nca_pp(omit_midpoint_nif(), "auclast")
  )
})


test_that("nca() default does not treat missing observation DV as zero", {
  auclast_na <- nca_pp(midpoint_nif(NA_real_), "auclast")
  auclast_zero <- nca_pp(midpoint_nif(0), "auclast")

  expect_false(isTRUE(all.equal(auclast_na, auclast_zero)))
})


test_that("nca(na_action = 'zero') matches an explicit zero concentration", {
  expect_equal(
    nca_pp(midpoint_nif(NA_real_), "auclast", na_action = "zero"),
    nca_pp(midpoint_nif(0), "auclast")
  )
})


test_that("nca(na_action = 'zero') differs from omitting the missing row", {
  auclast_zero <- nca_pp(
    midpoint_nif(NA_real_),
    "auclast",
    na_action = "zero"
  )
  auclast_omit <- nca_pp(omit_midpoint_nif(), "auclast")

  expect_false(isTRUE(all.equal(auclast_zero, auclast_omit)))
})


test_that("nca(na_action = 'error') stops when observation DV is missing", {
  expect_error(
    nca(
      midpoint_nif(NA_real_),
      analyte = "DRUG",
      parent = "DRUG",
      na_action = "error",
      silent = TRUE
    ),
    "missing concentration"
  )
})


test_that("nca() rejects an invalid na_action", {
  expect_error(
    nca(
      midpoint_nif(5),
      analyte = "DRUG",
      parent = "DRUG",
      na_action = "drop",
      silent = TRUE
    ),
    "na_action must be exclude, zero or error"
  )
})


test_that("nca() warns when missing concentrations are excluded", {
  expect_message(
    suppressWarnings(
      nca(
        midpoint_nif(NA_real_),
        analyte = "DRUG",
        parent = "DRUG",
        silent = FALSE
      )
    ),
    "1 missing concentration excluded from NCA"
  )
})


test_that("nca() warns when missing concentrations are set to zero", {
  expect_message(
    suppressWarnings(
      nca(
        midpoint_nif(NA_real_),
        analyte = "DRUG",
        parent = "DRUG",
        na_action = "zero",
        silent = FALSE
      )
    ),
    "1 missing concentration set to zero"
  )
})


test_that("nca() silent = TRUE suppresses missing-concentration messages", {
  suppressWarnings(
    expect_no_message(
      nca(
        midpoint_nif(NA_real_),
        analyte = "DRUG",
        parent = "DRUG",
        silent = TRUE
      )
    )
  )
})


test_that("nca() reports how many missing concentrations were excluded", {
  obj <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,   1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     NA,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     NA,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     2,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  expect_message(
    suppressWarnings(
      nca(obj, analyte = "DRUG", parent = "DRUG", silent = FALSE)
    ),
    "2 missing concentrations excluded from NCA"
  )
})


test_that("nca() ignores missing DV on dose rows", {
  with_dose_na <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     NA,  1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     2,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     1,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  with_dose_zero <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,   1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     2,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     1,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  expect_equal(
    nca_pp(with_dose_na, "auclast"),
    nca_pp(with_dose_zero, "auclast")
  )
})


test_that("nca() still imputes concentration 0 at the start of the interval", {
  no_t0 <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,   1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   2,     2,    2,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     2,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  with_t0_zero <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,   1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     0,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     2,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  expect_equal(nca_pp(no_t0, "auclast"), nca_pp(with_t0_zero, "auclast"))
  expect_equal(nca_pp(no_t0, "tmax"), 2)
})


test_that("nca() sets negative concentrations to zero", {
  negative <- tibble::tribble(
    ~ID, ~TIME, ~TAD, ~TAFD, ~DV, ~EVID, ~ANALYTE, ~PARENT, ~DOSE, ~AMT, ~CMT,
    1,   0,     0,    0,     0,   1,     "DRUG",   "DRUG",  100,   100,  1,
    1,   0,     0,    0,     10,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   2,     2,    2,     -5,  0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   4,     4,    4,     5,   0,     "DRUG",   "DRUG",  100,   0,    2,
    1,   8,     8,    8,     2,   0,     "DRUG",   "DRUG",  100,   0,    2
  ) |>
    nif(silent = TRUE)

  expect_equal(
    nca_pp(negative, "auclast"),
    nca_pp(midpoint_nif(0), "auclast")
  )
})


test_that("nca() warns when negative concentrations are set to zero", {
  negative <- midpoint_nif(-5)

  expect_message(
    suppressWarnings(
      nca(
        negative,
        analyte = "DRUG",
        parent = "DRUG",
        silent = FALSE
      )
    ),
    "1 negative concentration set to zero"
  )
})
