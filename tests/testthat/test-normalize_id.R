# Helper to create minimal nif objects for testing
make_test_nif <- function(..., creation_date = as.Date("2020-01-15")) {
  nif:::new_nif(
    as.data.frame(tibble::tribble(...)),
    nif_version = as.package_version("0.1.0"),
    creation_date = creation_date
  )
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

  expect_equal(unique(result$ID), 1:2)
  expect_type(result$ID, "integer")
})


test_that("normalize_id() assigns ID 1 to the subject with the lowest sum_dv", {
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

  expect_equal(result$ID, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_equal(result$DV, c(NA, 10, NA, 50, NA, 80))
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

  expect_equal(result$ID, c(1L, 1L, 2L, 2L))
  expect_equal(result$AMT, c(100, 0, 200, 0))
})


test_that("normalize_id() treats missing AMT as zero in the fingerprint", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     NA,   1,    1,     10,
    2,   0,     50,   1,    1,     10
  )

  result <- normalize_id(obj)

  expect_equal(result$ID, c(1L, 2L))
  expect_equal(result$AMT, c(NA_real_, 50))
})


test_that("normalize_id() treats missing DV as zero in the fingerprint", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    2,   0,     200,  1,    1,     NA
  )

  result <- normalize_id(obj)

  expect_equal(result$ID, c(1L, 2L))
  expect_equal(result$AMT, c(100, 200))
})


test_that("normalize_id() breaks identical fingerprints by first appearance", {
  obj <- make_test_nif(
    ~ID,      ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID,
    2,        0,     100,  1,    1,     NA,  "SECOND",
    2,        1,     0,    1,    0,     50,  "SECOND",
    1,        0,     100,  1,    1,     NA,  "FIRST",
    1,        1,     0,    1,    0,     50,  "FIRST"
  )

  result <- normalize_id(obj)

  expect_equal(result$ID, c(1L, 1L, 2L, 2L))
  expect_equal(result$USUBJID, c("SECOND", "SECOND", "FIRST", "FIRST"))
})


test_that("normalize_id() keeps original within-subject row order", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID,
    20,  2,     0,    1,    0,     30,  "B",
    10,  0,     100,  1,    1,     NA,  "A",
    20,  0,     200,  1,    1,     NA,  "B",
    10,  1,     0,    1,    0,     50,  "A"
  )

  result <- normalize_id(obj)

  expect_equal(result$ID, c(1L, 1L, 2L, 2L))
  expect_equal(result$TIME, c(2, 0, 0, 1))
  expect_equal(result$USUBJID, c("B", "B", "A", "A"))
  expect_equal(result$DV, c(30, NA, NA, 50))
})


test_that("normalize_id() maps every row of a subject to the same new ID", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID,
    10,  0,     100,  1,    1,     NA,  "A",
    10,  1,     0,    1,    0,     50,  "A",
    10,  2,     0,    1,    0,     25,  "A",
    10,  3,     0,    1,    0,     12,  "A",
    20,  0,     100,  1,    1,     NA,  "B",
    20,  1,     0,    1,    0,     40,  "B"
  )

  result <- normalize_id(obj)

  expect_equal(unique(result$ID[result$USUBJID == "A"]), 2L)
  expect_equal(unique(result$ID[result$USUBJID == "B"]), 1L)
  expect_equal(sum(result$USUBJID == "A"), 4)
  expect_equal(sum(result$USUBJID == "B"), 2)
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


test_that("normalize_id() preserves columns and does not add helpers", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~DOSE,
    1,   0,     100,  1,    1,     NA,  "DRUG",   100,
    1,   1,     0,    1,    0,     50,  "DRUG",   100,
    2,   0,     200,  1,    1,     NA,  "DRUG",   200,
    2,   1,     0,    1,    0,     30,  "DRUG",   200
  )

  result <- normalize_id(obj)

  expect_equal(names(result), names(obj))
  expect_false("sum_dv" %in% names(result))
  expect_false("sum_amt" %in% names(result))
  expect_false(".id_order" %in% names(result))
})


test_that("normalize_id() handles a single subject", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    5,   0,     100,  1,    1,     NA,
    5,   1,     0,    1,    0,     50,
    5,   2,     0,    1,    0,     25
  )

  result <- normalize_id(obj)

  expect_equal(result$ID, c(1L, 1L, 1L))
  expect_equal(result$TIME, c(0, 1, 2))
  expect_equal(result$DV, c(NA, 50, 25))
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


test_that("normalize_id() produces the same rows regardless of original ID values", {
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
  expect_equal(result_a$TIME, result_b$TIME)
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

  expect_equal(result$ID, c(1L, 1L, 2L, 2L))
})


test_that("normalize_id() handles many subjects", {
  n_subjects <- 20
  rows <- lapply(seq_len(n_subjects), function(i) {
    data.frame(
      ID = i * 10,
      TIME = c(0, 1),
      AMT = c(100, 0),
      CMT = 1,
      EVID = c(1, 0),
      DV = c(NA, i * 10)
    )
  })
  df <- nif:::new_nif(
    do.call(rbind, rows),
    nif_version = as.package_version("0.1.0"),
    creation_date = as.Date("2020-01-15")
  )
  result <- normalize_id(df)

  expect_equal(unique(result$ID), seq_len(n_subjects))
  expect_equal(nrow(result), n_subjects * 2)
  expect_equal(
    result$DV[result$EVID == 0],
    sort(seq_len(n_subjects) * 10)
  )
})


test_that("normalize_id() output is sorted by the new ID", {
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
  expect_equal(result$DV, c(NA, 20, NA, 50, NA, 80))
})


test_that("normalize_id() reorders already-sequential IDs when fingerprints disagree", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     50,
    2,   0,     100,  1,    1,     NA,
    2,   1,     0,    1,    0,     10
  )

  result <- normalize_id(obj)

  expect_equal(result$ID, c(1L, 1L, 2L, 2L))
  expect_equal(result$DV, c(NA, 10, NA, 50))
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

  expect_equal(result$ID, c(1L, 1L, 2L, 2L, 2L))
  expect_equal(result$DV, c(NA, 30, NA, 50, NA))
})


test_that("normalize_id() works with the examplinib_sad_nif data set", {
  result <- normalize_id(examplinib_sad_nif)

  expect_equal(sort(unique(result$ID)), seq_along(unique(result$ID)))
  expect_equal(nrow(result), nrow(examplinib_sad_nif))
  expect_equal(names(result), names(examplinib_sad_nif))
})


test_that("normalize_id() preserves nif class and attributes", {
  obj <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    5,   0,     100,  1,    1,     NA
  )

  result <- normalize_id(obj)

  expect_s3_class(result, "nif")
  expect_s3_class(result, "tbl_df")
  expect_equal(attr(result, "nif_version"), as.package_version("0.1.0"))
  expect_equal(attr(result, "creation_date"), as.Date("2020-01-15"))
})


test_that("normalize_id() handles an empty nif object", {
  obj <- nif:::new_nif(
    data.frame(
      ID = numeric(),
      TIME = numeric(),
      AMT = numeric(),
      CMT = numeric(),
      EVID = numeric(),
      DV = numeric()
    ),
    nif_version = as.package_version("0.1.0"),
    creation_date = as.Date("2020-01-15")
  )

  result <- normalize_id(obj)

  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(obj))
  expect_s3_class(result, "nif")
  expect_type(result$ID, "integer")
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
  expect_error(normalize_id("not a nif"), "Input must be a nif object")
  expect_error(normalize_id(42), "Input must be a nif object")
  expect_error(normalize_id(NULL), "Input must be a nif object")
})


test_that("hash.nif() is invariant to original ID values", {
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

  expect_equal(hash(obj_a), hash(obj_b))
})


test_that("hash.nif() changes when observations change", {
  obj_a <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     50
  )

  obj_b <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    1,    0,     51
  )

  expect_false(identical(hash(obj_a), hash(obj_b)))
})


test_that("hash.nif() includes nif attributes", {
  obj_a <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    creation_date = as.Date("2020-01-15")
  )

  obj_b <- make_test_nif(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    creation_date = as.Date("2021-01-15")
  )

  expect_false(identical(hash(obj_a), hash(obj_b)))
})
