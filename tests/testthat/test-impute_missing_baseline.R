## Tests for impute_missing_baseline

make_bl_nif <- function(
    weight,
    height = NULL,
    bmi = NULL,
    bl_creat = NULL,
    id = NULL,
    n_rows = NULL
) {
  n <- length(weight)
  if (is.null(id)) id <- seq_len(n)
  if (is.null(n_rows)) n_rows <- rep(2L, n)

  rows <- lapply(seq_len(n), function(i) {
    data.frame(
      ID     = id[[i]],
      USUBJID = paste0("S", id[[i]]),
      TIME   = seq(0, by = 24, length.out = n_rows[[i]]),
      AMT    = c(100, rep(0, n_rows[[i]] - 1L)),
      CMT    = 1,
      EVID   = c(1, rep(0, n_rows[[i]] - 1L)),
      DV     = NA_real_,
      WEIGHT = weight[[i]],
      stringsAsFactors = FALSE
    )
  })

  out <- dplyr::bind_rows(rows)

  if (!is.null(height)) {
    out$HEIGHT <- rep(height, times = n_rows)
  }
  if (!is.null(bmi)) {
    out$BMI <- rep(bmi, times = n_rows)
  }
  if (!is.null(bl_creat)) {
    out$BL_CREAT <- rep(bl_creat, times = n_rows)
  }

  nif(out)
}


test_that("impute_missing_baseline validates nif input", {
  expect_error(
    impute_missing_baseline(data.frame(ID = 1), silent = TRUE),
    "nif object"
  )
})


test_that("impute_missing_baseline validates baseline_fields type", {
  obj <- make_bl_nif(c(70, 80))

  expect_error(
    impute_missing_baseline(obj, baseline_fields = 1, silent = TRUE),
    "character"
  )
})


test_that("impute_missing_baseline validates summary_function", {
  obj <- make_bl_nif(c(70, 80))

  expect_error(
    impute_missing_baseline(obj, summary_function = "median", silent = TRUE),
    "summary_function must be a function"
  )
})


test_that("impute_missing_baseline errors when baseline columns are missing", {
  obj <- make_bl_nif(c(70, 80))

  expect_error(
    impute_missing_baseline(
      obj, baseline_fields = c("WEIGHT", "MISSING_COL"), silent = TRUE
    ),
    "missing in the input"
  )
})


test_that("impute_missing_baseline errors on non-numeric baseline columns", {
  obj <- make_bl_nif(c(70, 80))
  obj$BL_FOO <- "x"

  expect_error(
    impute_missing_baseline(
      obj, baseline_fields = "BL_FOO", silent = TRUE
    ),
    "Non-numeric baseline"
  )
})


test_that("impute_missing_baseline accepts integer baseline columns", {
  obj <- make_bl_nif(c(70, 80, NA_real_))
  obj$WEIGHT <- as.integer(obj$WEIGHT)

  result <- impute_missing_baseline(
    obj, baseline_fields = "WEIGHT", silent = TRUE
  )

  expect_false(any(is.na(result$WEIGHT)))
  expect_equal(
    unique(result$WEIGHT[result$ID == 3]),
    as.numeric(median(c(70L, 80L)))
  )
})


test_that("impute_missing_baseline imputes subject-level missing from population median", {
  obj <- make_bl_nif(c(70, 80, NA_real_))

  result <- impute_missing_baseline(
    obj, baseline_fields = "WEIGHT", silent = TRUE
  )

  expect_s3_class(result, "nif")
  expect_equal(result$WEIGHT[result$ID == 1], rep(70, 2))
  expect_equal(result$WEIGHT[result$ID == 2], rep(80, 2))
  expect_equal(
    result$WEIGHT[result$ID == 3],
    rep(median(c(70, 80)), 2)
  )
})


test_that("impute_missing_baseline fills within-subject NA before population impute", {
  obj <- make_bl_nif(c(70, 80), n_rows = c(3L, 2L))
  obj$WEIGHT[obj$ID == 1] <- c(NA_real_, 70, NA_real_)

  result <- impute_missing_baseline(
    obj, baseline_fields = "WEIGHT", silent = TRUE
  )

  expect_equal(result$WEIGHT[result$ID == 1], rep(70, 3))
  expect_equal(result$WEIGHT[result$ID == 2], rep(80, 2))
})


test_that("impute_missing_baseline errors on multiple distinct baselines per subject", {
  obj <- make_bl_nif(c(70, 80), n_rows = c(2L, 2L))
  obj$WEIGHT[obj$ID == 1] <- c(70, 72)

  expect_error(
    impute_missing_baseline(
      obj, baseline_fields = "WEIGHT", silent = TRUE
    ),
    "Multiple baseline values found"
  )
})


test_that("impute_missing_baseline uses custom summary_function", {
  obj <- make_bl_nif(c(70, 80, NA_real_))

  result <- impute_missing_baseline(
    obj,
    baseline_fields = "WEIGHT",
    summary_function = mean,
    silent = TRUE
  )

  expect_equal(
    unique(result$WEIGHT[result$ID == 3]),
    mean(c(70, 80))
  )
})


test_that("impute_missing_baseline auto-detects HEIGHT, WEIGHT, BMI, and BL_ fields", {
  obj <- make_bl_nif(
    weight   = c(70, 80, NA_real_),
    height   = c(170, 180, NA_real_),
    bmi      = c(24, 25, NA_real_),
    bl_creat = c(1.0, 1.2, NA_real_)
  )

  result <- impute_missing_baseline(obj, silent = TRUE)

  expect_false(any(is.na(result$WEIGHT)))
  expect_false(any(is.na(result$HEIGHT)))
  expect_false(any(is.na(result$BMI)))
  expect_false(any(is.na(result$BL_CREAT)))

  expect_equal(
    unique(result$WEIGHT[result$ID == 3]),
    median(c(70, 80))
  )
  expect_equal(
    unique(result$HEIGHT[result$ID == 3]),
    median(c(170, 180))
  )
  expect_equal(
    unique(result$BMI[result$ID == 3]),
    median(c(24, 25))
  )
  expect_equal(
    unique(result$BL_CREAT[result$ID == 3]),
    median(c(1.0, 1.2))
  )
})


test_that("impute_missing_baseline imputes explicit BL_ fields", {
  obj <- make_bl_nif(
    weight   = c(70, 80),
    bl_creat = c(1.0, NA_real_)
  )

  result <- impute_missing_baseline(
    obj, baseline_fields = "BL_CREAT", silent = TRUE
  )

  expect_equal(
    unique(result$BL_CREAT[result$ID == 2]),
    1.0
  )
})


test_that("impute_missing_baseline auto-detect excludes non-numeric BL_ columns", {
  obj <- make_bl_nif(
    weight   = c(70, NA_real_),
    bl_creat = c(1.0, NA_real_)
  )
  obj$BL_FOO <- "x"

  result <- impute_missing_baseline(obj, silent = TRUE)

  expect_equal(unique(result$WEIGHT[result$ID == 2]), 70)
  expect_equal(unique(result$BL_CREAT[result$ID == 2]), 1.0)
  expect_true(all(result$BL_FOO == "x"))
})


test_that("impute_missing_baseline leaves all-NA field unchanged", {
  obj <- make_bl_nif(c(NA_real_, NA_real_))

  result <- impute_missing_baseline(
    obj, baseline_fields = "WEIGHT", silent = TRUE
  )

  expect_true(all(is.na(result$WEIGHT)))
})


test_that("impute_missing_baseline is a no-op when no values are missing", {
  obj <- make_bl_nif(c(70, 80))

  result <- impute_missing_baseline(
    obj, baseline_fields = "WEIGHT", silent = TRUE
  )

  expect_equal(result$WEIGHT, obj$WEIGHT)
})


test_that("impute_missing_baseline returns nif and preserves row count and columns", {
  obj <- make_bl_nif(c(70, NA_real_, 90))

  result <- impute_missing_baseline(
    obj, baseline_fields = "WEIGHT", silent = TRUE
  )

  expect_s3_class(result, "nif")
  expect_equal(nrow(result), nrow(obj))
  expect_setequal(names(result), names(obj))
})


test_that("impute_missing_baseline silent suppresses messages", {
  obj <- make_bl_nif(c(70, NA_real_))

  expect_no_message(
    impute_missing_baseline(
      obj, baseline_fields = "WEIGHT", silent = TRUE
    )
  )
})


test_that("impute_missing_baseline reports population center and population fills", {
  obj <- make_bl_nif(c(70, NA_real_))

  expect_message(
    expect_message(
      impute_missing_baseline(
        obj, baseline_fields = "WEIGHT", silent = FALSE
      ),
      "Baseline population "
    ),
    "baseline values were imputed"
  )

  expect_message(
    expect_message(
      impute_missing_baseline(
        obj, baseline_fields = "WEIGHT", silent = FALSE
      ),
      "filled with population median"
    )
  )
})


test_that("impute_missing_baseline reports within-subject fills", {
  obj <- make_bl_nif(c(70, 80), n_rows = c(3L, 2L))
  obj$WEIGHT[obj$ID == 1] <- c(NA_real_, 70, NA_real_)

  expect_message(
    expect_message(
      impute_missing_baseline(
        obj, baseline_fields = "WEIGHT", silent = FALSE
      ),
      "filled within subject"
    )
  )
})


test_that("impute_missing_baseline reports custom summary_function label", {
  obj <- make_bl_nif(c(70, NA_real_))

  expect_message(
    expect_message(
      impute_missing_baseline(
        obj,
        baseline_fields = "WEIGHT",
        summary_function = mean,
        silent = FALSE
      ),
      "filled with population mean"
    )
  )
})


test_that("impute_missing_baseline reports center but not fills when nothing missing", {
  obj <- make_bl_nif(c(70, 80))

  msgs <- capture_messages(
    impute_missing_baseline(
      obj, baseline_fields = "WEIGHT", silent = FALSE
    )
  )

  expect_true(any(grepl("Baseline population", msgs)))
  expect_false(any(grepl("baseline values were imputed", msgs)))
})


test_that("impute_missing_baseline does not report fills for all-NA field", {
  obj <- make_bl_nif(c(NA_real_, NA_real_))

  msgs <- capture_messages(
    impute_missing_baseline(
      obj, baseline_fields = "WEIGHT", silent = FALSE
    )
  )

  expect_true(any(grepl("Baseline population", msgs)))
  expect_false(any(grepl("baseline values were imputed", msgs)))
})


test_that("impute_missing_baseline errors when no baseline fields are available", {
  obj <- make_bl_nif(c(70, 80)) |>
    dplyr::select(-"WEIGHT")

  expect_error(
    impute_missing_baseline(obj, silent = TRUE),
    "No numeric baseline fields detected"
  )
})


test_that("impute_missing_baseline imputes multiple fields independently", {
  obj <- make_bl_nif(
    weight = c(70, NA_real_, 90),
    height = c(NA_real_, 180, 170)
  )

  result <- impute_missing_baseline(
    obj,
    baseline_fields = c("WEIGHT", "HEIGHT"),
    silent = TRUE
  )

  expect_equal(
    unique(result$WEIGHT[result$ID == 2]),
    median(c(70, 90))
  )
  expect_equal(
    unique(result$HEIGHT[result$ID == 1]),
    median(c(180, 170))
  )
  expect_equal(result$WEIGHT[result$ID == 1], rep(70, 2))
  expect_equal(result$HEIGHT[result$ID == 2], rep(180, 2))
})

