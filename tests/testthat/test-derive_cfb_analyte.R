test_that("derive_cfb_analyte works with default analyte name", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0,
    2,   -1,    20,   "A",      0,    2,    0,
    2,   0,     22,   "A",      0,    2,    0,
    2,   1,     25,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  expect_true("CFB_A" %in% analytes(result))
  expect_true("A" %in% analytes(result))

  cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
  expect_equal(unique(cfb_rows$CMT), max(test_nif$CMT) + 1)

  # ID 1: baseline = median(10, 12) = 11
  id1_cfb <- cfb_rows %>% filter(ID == 1) %>% arrange(TIME)
  expect_equal(id1_cfb$DV, c(-1, 1, 4))

  # ID 2: baseline = median(20, 22) = 21
  id2_cfb <- cfb_rows %>% filter(ID == 2) %>% arrange(TIME)
  expect_equal(id2_cfb$DV, c(-1, 1, 4))

  original_rows <- result %>% filter(ANALYTE == "A")
  expect_equal(nrow(original_rows), nrow(test_data))
})


test_that("derive_cfb_analyte works with custom analyte name", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(
    test_nif,
    source_analyte = "A",
    analyte = "CHANGE_FROM_BL",
    silent = TRUE
  )

  expect_true("CHANGE_FROM_BL" %in% analytes(result))
  expect_false("CFB_A" %in% analytes(result))
})


test_that("derive_cfb_analyte handles multiple subjects independently", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0,
    2,   -1,    20,   "A",      0,    2,    0,
    2,   0,     22,   "A",      0,    2,    0,
    2,   1,     25,   "A",      0,    2,    0,
    3,   -1,    30,   "A",      0,    2,    0,
    3,   0,     32,   "A",      0,    2,    0,
    3,   1,     35,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)
  cfb_rows <- result %>% filter(ANALYTE == "CFB_A")

  # ID 1: baseline = median(10, 12) = 11
  expect_equal(
    (cfb_rows %>% filter(ID == 1) %>% arrange(TIME))$DV,
    c(-1, 1, 4)
  )
  # ID 2: baseline = median(20, 22) = 21
  expect_equal(
    (cfb_rows %>% filter(ID == 2) %>% arrange(TIME))$DV,
    c(-1, 1, 4)
  )
  # ID 3: baseline = median(30, 32) = 31
  expect_equal(
    (cfb_rows %>% filter(ID == 3) %>% arrange(TIME))$DV,
    c(-1, 1, 4)
  )
})


test_that("derive_cfb_analyte only processes the source analyte", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0,
    1,   -1,    20,   "B",      0,    3,    0,
    1,   0,     22,   "B",      0,    3,    0,
    1,   1,     25,   "B",      0,    3,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  expect_true("CFB_A" %in% analytes(result))
  expect_true("B" %in% analytes(result))
  expect_false("CFB_B" %in% analytes(result))

  cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
  expect_equal(nrow(cfb_rows), 3)

  b_rows <- result %>% filter(ANALYTE == "B")
  expect_equal(b_rows$DV, c(20, 22, 25))
})


test_that("derive_cfb_analyte works with different summary functions", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  # mean: baseline = mean(10, 12) = 11, CFB at TIME 1 = 15 - 11 = 4
  result_mean <- derive_cfb_analyte(
    test_nif, source_analyte = "A", analyte = "CFB_MEAN",
    summary_function = mean, silent = TRUE
  )
  expect_equal(
    (result_mean %>% filter(ANALYTE == "CFB_MEAN", TIME == 1))$DV, 4
  )

  # min: baseline = min(10, 12) = 10, CFB at TIME 1 = 15 - 10 = 5
  result_min <- derive_cfb_analyte(
    test_nif, source_analyte = "A", analyte = "CFB_MIN",
    summary_function = min, silent = TRUE
  )
  expect_equal(
    (result_min %>% filter(ANALYTE == "CFB_MIN", TIME == 1))$DV, 5
  )

  # max: baseline = max(10, 12) = 12, CFB at TIME 1 = 15 - 12 = 3
  result_max <- derive_cfb_analyte(
    test_nif, source_analyte = "A", analyte = "CFB_MAX",
    summary_function = max, silent = TRUE
  )
  expect_equal(
    (result_max %>% filter(ANALYTE == "CFB_MAX", TIME == 1))$DV, 3
  )
})


test_that("derive_cfb_analyte works with custom baseline filter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -2,    8,    "A",      0,    2,    0,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  # Only TIME == -1 as baseline => baseline = 10
  result <- derive_cfb_analyte(
    test_nif, source_analyte = "A",
    baseline_filter = "TIME == -1", silent = TRUE
  )

  cfb <- result %>% filter(ANALYTE == "CFB_A") %>% arrange(TIME)
  expect_equal(cfb$DV[cfb$TIME == 1], 5)   # 15 - 10
  expect_equal(cfb$DV[cfb$TIME == -2], -2)  # 8 - 10
})


test_that("derive_cfb_analyte handles multiple baseline values (odd count)", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -2,    8,    "A",      0,    2,    0,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  cfb <- result %>% filter(ANALYTE == "CFB_A") %>% arrange(TIME)
  # baseline = median(8, 10, 12) = 10
  expect_equal(cfb$DV, c(-2, 0, 2, 5))
})


test_that("derive_cfb_analyte filters out EVID == 1 rows", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     NA,   "A",      100,  1,    1,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
  # Only EVID == 0 rows should appear in CFB analyte
  expect_equal(nrow(cfb_rows), 3)
  expect_true(all(cfb_rows$EVID == 0))

  # baseline = median(10, 12) = 11
  expect_equal(
    (cfb_rows %>% filter(TIME == 1))$DV, 4
  )
})


test_that("derive_cfb_analyte handles NA values in DV", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     NA,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  cfb <- result %>% filter(ANALYTE == "CFB_A") %>% arrange(TIME)
  # baseline = median(10) = 10 (NA omitted)
  expect_equal(cfb$DV[cfb$TIME == -1], 0)    # 10 - 10
  expect_true(is.na(cfb$DV[cfb$TIME == 0]))   # NA - 10 = NA
  expect_equal(cfb$DV[cfb$TIME == 1], 5)      # 15 - 10
})


test_that("derive_cfb_analyte returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  expect_s3_class(result, "nif")
  expect_true("REF" %in% names(result))
})


test_that("derive_cfb_analyte preserves original rows", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  original <- result %>% filter(ANALYTE == "A") %>% arrange(TIME)
  expect_equal(nrow(original), 3)
  expect_equal(original$DV, c(10, 12, 15))
})


test_that("derive_cfb_analyte assigns CMT as max + 1", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    5,    0,
    1,   0,     12,   "A",      0,    5,    0,
    1,   1,     15,   "A",      0,    5,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
  expect_equal(unique(cfb_rows$CMT), 6)
})


test_that("derive_cfb_analyte rejects non-nif input", {
  expect_error(
    derive_cfb_analyte(data.frame(x = 1), source_analyte = "A"),
    "Input must be a nif object"
  )
})


test_that("derive_cfb_analyte rejects non-existent source analyte", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_error(
    derive_cfb_analyte(test_nif, source_analyte = "NONEXISTENT"),
    "not found"
  )
})


test_that("derive_cfb_analyte rejects target analyte that already exists", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_error(
    derive_cfb_analyte(test_nif, source_analyte = "A", analyte = "A"),
    "already in data set"
  )
})


test_that("derive_cfb_analyte rejects non-function summary_function", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_error(
    derive_cfb_analyte(test_nif, source_analyte = "A",
                       summary_function = "mean"),
    "summary_function must be a function"
  )
})


test_that("derive_cfb_analyte rejects non-character analyte", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_error(
    derive_cfb_analyte(test_nif, source_analyte = "A", analyte = 123),
    "character"
  )
})


test_that("derive_cfb_analyte rejects non-logical silent", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_error(
    derive_cfb_analyte(test_nif, source_analyte = "A", silent = "yes"),
    "logical"
  )
})


test_that("derive_cfb_analyte rejects non-character baseline_filter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_error(
    derive_cfb_analyte(test_nif, source_analyte = "A",
                       baseline_filter = 42),
    "character"
  )
})


test_that("derive_cfb_analyte rejects filter referencing nonexistent column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_error(
    derive_cfb_analyte(
      test_nif, source_analyte = "A",
      baseline_filter = "NONEXISTENT_COL <= 0",
      silent = TRUE),
    "not found in data"
  )
})


test_that("derive_cfb_analyte silent = TRUE suppresses messages", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_silent(
    derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)
  )
})


test_that("derive_cfb_analyte silent = FALSE produces messages", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  expect_message(
    derive_cfb_analyte(test_nif, source_analyte = "A", silent = FALSE),
    "Compartment for"
  )
})


test_that("derive_cfb_analyte works with single baseline observation", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   0,     10,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0,
    1,   2,     20,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  cfb <- result %>% filter(ANALYTE == "CFB_A") %>% arrange(TIME)
  # baseline = median(10) = 10 (only TIME == 0 matches TIME <= 0)
  expect_equal(cfb$DV, c(0, 5, 10))
})


test_that("derive_cfb_analyte result row count is correct", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,   "A",      0,    2,    0,
    1,   0,     NA,   "A",      100,  1,    1,
    1,   0,     12,   "A",      0,    2,    0,
    1,   1,     15,   "A",      0,    2,    0,
    2,   -1,    20,   "A",      0,    2,    0,
    2,   0,     22,   "A",      0,    2,    0,
    2,   1,     25,   "A",      0,    2,    0
  )
  test_nif <- nif(test_data)

  n_obs_rows <- sum(test_data$EVID == 0)

  result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)

  # Original rows + CFB rows (one per EVID==0 row of source analyte)
  expect_equal(nrow(result), nrow(test_nif) + n_obs_rows)
})

