# Helper to create minimal nif objects for testing
make_test_nif <- function(...) {
  df <- tibble::tribble(...) %>% as.data.frame()
  class(df) <- c("nif", "data.frame")
  df
}


test_that("normalize_id() reassigns IDs as sequential integers starting at 1", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    10,  0,     100,  1,    1,     NA,
    10,  1,     0,    1,    0,     50,
    20,  0,     100,  1,    1,     NA,
    20,  1,     0,    1,    0,     30
  )

  result <- normalize_id(obj)

  expect_equal(sort(unique(result$ID)), c(1, 2))
})


test_that("normalize_id() orders IDs by fingerprint (sum_dv, then sum_amt)", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     80,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     10,
    3,   0,     100,  1,    1,     NA,
    3,   1,     0,    1,    0,     50
  )

  result <- normalize_id(obj)

  dv_by_new_id <- result %>%
    filter(EVID == 0) %>%
    reframe(sum_dv = sum(DV, na.rm = TRUE), .by = "ID") %>%
    arrange(ID)

  expect_equal(dv_by_new_id$sum_dv, sort(dv_by_new_id$sum_dv))
})


test_that("normalize_id() uses sum_amt as tiebreaker when sum_dv is equal", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     200,  1,    1,     NA,
    1,   1,     0,    1,    0,     50,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     50
  )

  result <- normalize_id(obj)

  amt_by_new_id <- result %>%
    reframe(sum_amt = sum(AMT, na.rm = TRUE), .by = "ID") %>%
    arrange(ID)

  expect_equal(amt_by_new_id$sum_amt, sort(amt_by_new_id$sum_amt))
})


test_that("normalize_id() preserves the number of rows", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     50,
    1,   2,     0,    1,    0,     25,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     40
  )

  result <- normalize_id(obj)

  expect_equal(nrow(result), nrow(obj))
})


test_that("normalize_id() preserves all original columns", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE,
    1,   0,     100,  1,    1,     NA,  "DRUG",   100,
    1,   1,     0,    1,    0,     50,  "DRUG",   100,
    2,   0,     200,  1,    1,     NA,  "DRUG",   200,
    2,   1,     0,    1,    0,     30,  "DRUG",   200
  )

  result <- normalize_id(obj)

  expect_true(all(names(obj) %in% names(result)))
  expect_false("sum_dv" %in% names(result))
  expect_false("sum_amt" %in% names(result))
  expect_false(".id_order" %in% names(result))
})


test_that("normalize_id() does not add extra columns", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    2,   0,     100,  1,    1,     NA
  )

  result <- normalize_id(obj)

  expect_equal(sort(names(result)), sort(names(obj)))
})


test_that("normalize_id() handles a single subject", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    5,   0,     100,  1,    1,     NA,
    5,   1,     0,    1,    0,     50,
    5,   2,     0,    1,    0,     25
  )

  result <- normalize_id(obj)

  expect_equal(unique(result$ID), 1)
  expect_equal(nrow(result), 3)
})


test_that("normalize_id() maps all rows of same original ID to same new ID", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    10,  0,     100,  1,    1,     NA,
    10,  1,     0,    1,    0,     50,
    10,  2,     0,    1,    0,     25,
    10,  3,     0,    1,    0,     12,
    20,  0,     100,  1,    1,     NA,
    20,  1,     0,    1,    0,     40
  )

  result <- normalize_id(obj)

  ids_for_orig_10 <- unique(result$ID[obj$ID == 10 |
    result$DV %in% c(50, 25, 12) | (result$AMT == 100 & result$DV == 50)])

  expect_equal(length(unique(result$ID)), 2)
  for (new_id in unique(result$ID)) {
    rows <- result[result$ID == new_id, ]
    expect_true(nrow(rows) >= 1)
  }
})


test_that("normalize_id() is deterministic across repeated calls", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    3,   0,     100,  1,    1,     NA,
    3,   1,     0,    1,    0,     80,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     20,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     50
  )

  result1 <- normalize_id(obj)
  result2 <- normalize_id(obj)

  expect_equal(result1, result2)
})


test_that("normalize_id() produces same result regardless of original ID values", {
  obj_a <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     50,
    2,   0,     200,  1,    1,     NA,
    2,   1,     0,    1,    0,     30
  )

  obj_b <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    99,  0,     100,  1,    1,     NA,
    99,  1,     0,    1,    0,     50,
    77,  0,     200,  1,    1,     NA,
    77,  1,     0,    1,    0,     30
  )

  result_a <- normalize_id(obj_a)
  result_b <- normalize_id(obj_b)

  expect_equal(result_a$ID, result_b$ID)
  expect_equal(result_a$DV, result_b$DV)
  expect_equal(result_a$AMT, result_b$AMT)
})


test_that("normalize_id() handles all-NA DV values", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    2,   0,     200,  1,    1,     NA
  )

  result <- normalize_id(obj)

  expect_equal(sort(unique(result$ID)), c(1, 2))
  expect_equal(nrow(result), 2)
})


test_that("normalize_id() handles zero DV and AMT values", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     0,    1,    1,     0,
    1,   1,     0,    1,    0,     0,
    2,   0,     0,    1,    1,     0,
    2,   1,     0,    1,    0,     0
  )

  result <- normalize_id(obj)

  expect_equal(sort(unique(result$ID)), c(1, 2))
})


test_that("normalize_id() handles many subjects", {
  n_subjects <- 20
  rows <- lapply(1:n_subjects, function(i) {
    data.frame(
      ID = i * 10,
      TIME = c(0, 1),
      AMT = c(100, 0),
      CMT = 1,
      EVID = c(1, 0),
      DV = c(NA, i * 10)
    )
  })
  df <- do.call(rbind, rows)
  class(df) <- c("nif", "data.frame")

  result <- normalize_id(df)

  expect_equal(sort(unique(result$ID)), 1:n_subjects)
  expect_equal(nrow(result), n_subjects * 2)
})


test_that("normalize_id() output is sorted by ID", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    3,   0,     100,  1,    1,     NA,
    3,   1,     0,    1,    0,     80,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     20,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     50
  )

  result <- normalize_id(obj)

  expect_equal(result$ID, sort(result$ID))
})


test_that("normalize_id() rejects non-nif input", {
  df <- data.frame(
    ID = c(1, 2),
    TIME = c(0, 0),
    AMT = c(100, 100),
    CMT = c(1, 1),
    EVID = c(1, 1),
    DV = c(NA, NA)
  )

  expect_error(normalize_id(df), "Input must be a nif object")
})


test_that("normalize_id() rejects non-data-frame input", {
  expect_error(normalize_id("not a nif"), "Input must be a nif object")
  expect_error(normalize_id(42), "Input must be a nif object")
  expect_error(normalize_id(NULL), "Input must be a nif object")
})


test_that("normalize_id() preserves non-ID data values", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    10,  0,     100,  1,    1,     NA,
    10,  1,     0,    1,    0,     50,
    10,  2,     0,    1,    0,     25,
    20,  0,     200,  1,    1,     NA,
    20,  1,     0,    1,    0,     30
  )

  result <- normalize_id(obj)

  expect_true(all(c(100, 200) %in% result$AMT))
  expect_true(all(c(50, 25, 30) %in% result$DV[!is.na(result$DV)]))
  expect_equal(sort(result$TIME), sort(obj$TIME))
})


test_that("normalize_id() works with already-sequential IDs", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     50,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     30,
    3,   0,     100,  1,    1,     NA,
    3,   1,     0,    1,    0,     10
  )

  result <- normalize_id(obj)

  expect_equal(sort(unique(result$ID)), c(1, 2, 3))
  expect_equal(nrow(result), 6)
})


test_that("normalize_id() handles mixed NA and numeric DV per subject", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     50,
    1,   2,     0,    1,    0,     NA,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     30
  )

  result <- normalize_id(obj)

  expect_equal(sort(unique(result$ID)), c(1, 2))
  expect_equal(nrow(result), 5)
})


test_that("normalize_id() works with the examplinib_sad_nif data set", {
  result <- normalize_id(examplinib_sad_nif)

  expect_equal(sort(unique(result$ID)), seq_along(unique(result$ID)))
  expect_equal(nrow(result), nrow(examplinib_sad_nif))
  expect_true(all(names(examplinib_sad_nif) %in% names(result)))
})


test_that("normalize_id() correctly groups rows after ID reassignment", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    10,  0,     100,  1,    1,     NA,
    10,  1,     0,    1,    0,     50,
    10,  2,     0,    1,    0,     25,
    20,  0,     200,  1,    1,     NA,
    20,  1,     0,    1,    0,     30,
    20,  2,     0,    1,    0,     15
  )

  result <- normalize_id(obj)

  for (new_id in unique(result$ID)) {
    group <- result[result$ID == new_id, ]
    dv_vals <- group$DV[!is.na(group$DV)]
    amt_val <- group$AMT[group$AMT > 0]
    if (length(amt_val) > 0 && amt_val[1] == 100) {
      expect_true(all(dv_vals %in% c(50, 25)))
    } else if (length(amt_val) > 0 && amt_val[1] == 200) {
      expect_true(all(dv_vals %in% c(30, 15)))
    }
  }
})


test_that("normalize_id() handles subjects with identical fingerprints", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     50,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     50
  )

  result <- normalize_id(obj)

  expect_equal(sort(unique(result$ID)), c(1, 2))
  expect_equal(nrow(result), 4)
})
