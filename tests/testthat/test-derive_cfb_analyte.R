# test_that("derive_cfb_analyte works with valid input and default analyte name", {
#   # Create a simple test dataset
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00",
#     2,   "SUBJ-002",  -1,    20,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     2,   "SUBJ-002",  0,     22,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     2,   "SUBJ-002",  1,     25,   "A",      0,    2,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Run derive_cfb_analyte with default analyte name
#   result <- derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)
#
#   # Check that new analyte was created
#   expect_true("CFB_A" %in% analytes(result))
#   expect_true("A" %in% analytes(result))
#
#   # Check that CFB analyte has correct CMT (should be max + 1 = 3)
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
#   expect_equal(unique(cfb_rows$CMT), 3)
#
#   # Check baseline calculation (median of TIME <= 0: 10 and 12 = 11 for ID 1)
#   # ID 1: baseline = median(10, 12) = 11
#   # ID 1 CFB at TIME -1: 10 - 11 = -1
#   # ID 1 CFB at TIME 0: 12 - 11 = 1
#   # ID 1 CFB at TIME 1: 15 - 11 = 4
#   id1_cfb <- cfb_rows %>% filter(ID == 1) %>% arrange(TIME)
#   expect_equal(id1_cfb$DV[1], -1)  # TIME -1
#   expect_equal(id1_cfb$DV[2], 1)   # TIME 0
#   expect_equal(id1_cfb$DV[3], 4)   # TIME 1
#
#   # ID 2: baseline = median(20, 22) = 21
#   # ID 2 CFB at TIME -1: 20 - 21 = -1
#   # ID 2 CFB at TIME 0: 22 - 21 = 1
#   # ID 2 CFB at TIME 1: 25 - 21 = 4
#   id2_cfb <- cfb_rows %>% filter(ID == 2) %>% arrange(TIME)
#   expect_equal(id2_cfb$DV[1], -1)  # TIME -1
#   expect_equal(id2_cfb$DV[2], 1)   # TIME 0
#   expect_equal(id2_cfb$DV[3], 4)   # TIME 1
#
#   # Check that original analyte rows are preserved
#   original_rows <- result %>% filter(ANALYTE == "A")
#   expect_equal(nrow(original_rows), nrow(test_data))
# })
#
#
# test_that("derive_cfb_analyte works with custom analyte name", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Run with custom analyte name
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     analyte = "CHANGE_FROM_BASELINE",
#     silent = TRUE
#   )
#
#   # Check that custom analyte name was used
#   expect_true("CHANGE_FROM_BASELINE" %in% analytes(result))
#   expect_false("CFB_A" %in% analytes(result))
# })
#
#
# test_that("derive_cfb_analyte works with different summary functions", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with mean
#   result_mean <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     analyte = "CFB_MEAN",
#     summary_function = mean,
#     silent = TRUE
#   )
#   cfb_mean <- result_mean %>% filter(ANALYTE == "CFB_MEAN" & ID == 1 & TIME == 1)
#   # Baseline = mean(10, 12) = 11, CFB at TIME 1 = 15 - 11 = 4
#   expect_equal(cfb_mean$DV, 4)
#
#   # Test with min
#   result_min <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     analyte = "CFB_MIN",
#     summary_function = min,
#     silent = TRUE
#   )
#   cfb_min <- result_min %>% filter(ANALYTE == "CFB_MIN" & ID == 1 & TIME == 1)
#   # Baseline = min(10, 12) = 10, CFB at TIME 1 = 15 - 10 = 5
#   expect_equal(cfb_min$DV, 5)
#
#   # Test with max
#   result_max <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     analyte = "CFB_MAX",
#     summary_function = max,
#     silent = TRUE
#   )
#   cfb_max <- result_max %>% filter(ANALYTE == "CFB_MAX" & ID == 1 & TIME == 1)
#   # Baseline = max(10, 12) = 12, CFB at TIME 1 = 15 - 12 = 3
#   expect_equal(cfb_max$DV, 3)
# })
#
#
# test_that("derive_cfb_analyte works with custom baseline filter", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -2,    8,    "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 02:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 03:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with custom baseline filter (only TIME == -1)
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     analyte = "CFB_CUSTOM",
#     baseline_filter = "TIME == -1",
#     silent = TRUE
#   )
#
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_CUSTOM" & ID == 1) %>% arrange(TIME)
#   # Baseline = median(10) = 10 (only TIME == -1)
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == 1], 5)  # 15 - 10 = 5
# })
#
#
# test_that("derive_cfb_analyte handles multiple baseline values correctly", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -2,    8,    "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 02:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 03:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     baseline_filter = "TIME <= 0",
#     silent = TRUE
#   )
#
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A" & ID == 1) %>% arrange(TIME)
#   # Baseline = median(8, 10, 12) = 10
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == -2], -2)  # 8 - 10 = -2
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == -1], 0)   # 10 - 10 = 0
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == 0], 2)   # 12 - 10 = 2
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == 1], 5)   # 15 - 10 = 5
# })
#
#
# test_that("derive_cfb_analyte filters EVID == 0 correctly", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  0,     NA,   "A",      100,  1,    1,     "2020-01-01 01:00:00",  # Dosing event
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     baseline_filter = "TIME <= 0",
#     silent = TRUE
#   )
#
#   # Should only use EVID == 0 rows, so baseline = median(10, 12) = 11
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A" & ID == 1) %>% arrange(TIME)
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == 1], 4)  # 15 - 11 = 4
#   # Should not have CFB row for EVID == 1
#   expect_equal(nrow(cfb_rows), 3)  # Only 3 EVID == 0 rows
# })
#
#
# test_that("derive_cfb_analyte handles multiple analytes correctly", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00",
#     1,   "SUBJ-001",  -1,    20,   "B",      0,    3,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     22,   "B",      0,    3,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  1,     25,   "B",      0,    3,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Only derive CFB for analyte A
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     silent = TRUE
#   )
#
#   # Check that CFB_A was created
#   expect_true("CFB_A" %in% analytes(result))
#   # Check that B was not affected
#   expect_true("B" %in% analytes(result))
#   expect_false("CFB_B" %in% analytes(result))
#
#   # Check that CFB_A only has rows for ID 1 (the only ID with analyte A)
#   cfb_a_rows <- result %>% filter(ANALYTE == "CFB_A")
#   expect_equal(unique(cfb_a_rows$ID), 1)
#   expect_equal(nrow(cfb_a_rows), 3)  # 3 time points for ID 1
# })
#
#
# test_that("derive_cfb_analyte handles empty baseline sets", {
#   # Create test data where baseline filter matches no rows
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  1,     10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  2,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  3,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with filter that matches no rows
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     baseline_filter = "TIME < 0",
#     silent = TRUE
#   )
#
#   # Check that CFB analyte was created but with NA baseline
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A" & ID == 1)
#   # When baseline is empty, summary_function(na.omit(empty)) = NA
#   expect_true(all(is.na(cfb_rows$DV)))
# })
#
#
# test_that("derive_cfb_analyte handles NA values in DV correctly", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     NA,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     baseline_filter = "TIME <= 0",
#     silent = TRUE
#   )
#
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A" & ID == 1) %>% arrange(TIME)
#   # Baseline = median(10) = 10 (NA is omitted)
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == -1], 0)   # 10 - 10 = 0
#   expect_true(is.na(cfb_rows$DV[cfb_rows$TIME == 0]))  # NA - 10 = NA
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == 1], 5)    # 15 - 10 = 5
# })
#
#
# test_that("derive_cfb_analyte assigns correct CMT", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     silent = TRUE
#   )
#
#   # Max CMT in original is 2, so new CMT should be 3
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
#   expect_equal(unique(cfb_rows$CMT), 3)
# })
#
#
# test_that("derive_cfb_analyte removes REF column", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC, ~REF,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00", 1,
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00", 2
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     silent = TRUE
#   )
#
#   # REF column should not be in CFB rows
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
#   expect_false("REF" %in% names(cfb_rows))
# })
#
#
# test_that("derive_cfb_analyte validates input parameters", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test non-nif object
#   expect_error(
#     derive_cfb_analyte(data.frame(), source_analyte = "A"),
#     "Input must be a nif object"
#   )
#
#   # Test NULL source_analyte
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = NULL),
#     "source_analyte must not be NULL"
#   )
#
#   # Test non-character source_analyte
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = 123),
#     "source_analyte must be a string value"
#   )
#
#   # Test non-character analyte
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "A", analyte = 123),
#     "analyte must be a string value"
#   )
#
#   # Test non-logical silent
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "A", silent = "yes"),
#     "silent must be a logical value"
#   )
# })
#
#
# test_that("derive_cfb_analyte validates source_analyte exists", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with non-existent source_analyte
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "NONEXISTENT"),
#     "Analyte NONEXISTENT not found in analytes!"
#   )
# })
#
#
# test_that("derive_cfb_analyte validates analyte does not already exist", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with analyte that already exists
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "A", analyte = "A"),
#     "Analyte A already in data set!"
#   )
# })
#
#
# test_that("derive_cfb_analyte validates summary_function is a function", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with non-function summary_function
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "A", summary_function = "mean"),
#     "summary_function must be a function"
#   )
# })
#
#
# test_that("derive_cfb_analyte validates baseline_filter", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with invalid filter expression
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "A", baseline_filter = "INVALID_SYNTAX !!!"),
#     "Invalid baseline_filter expression"
#   )
#
#   # Test with filter that doesn't evaluate to logical
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "A", baseline_filter = "TIME"),
#     "baseline_filter must evaluate to logical values"
#   )
#
#   # Test with filter that returns wrong length
#   # This is harder to test directly, but we can test with a filter that uses a column that doesn't exist
#   expect_error(
#     derive_cfb_analyte(test_nif, source_analyte = "A", baseline_filter = "NONEXISTENT_COL <= 0"),
#     "Invalid baseline_filter expression"
#   )
# })
#
#
# test_that("derive_cfb_analyte works with complex baseline filters", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC, ~FOOD,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00", "FED",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00", "FED",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00", "FASTED"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with complex filter using character column
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     analyte = "CFB_FED",
#     baseline_filter = "TIME <= 0 & FOOD == 'FED'",
#     silent = TRUE
#   )
#
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_FED" & ID == 1) %>% arrange(TIME)
#   # Baseline = median(10, 12) = 11 (only FED rows with TIME <= 0)
#   expect_equal(cfb_rows$DV[cfb_rows$TIME == 1], 4)  # 15 - 11 = 4
# })
#
#
# test_that("derive_cfb_analyte preserves original data structure", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC, ~PARENT, ~METABOLITE,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00", "DRUG", FALSE,
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00", "DRUG", FALSE,
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00", "DRUG", FALSE
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     silent = TRUE
#   )
#
#   # Check that original rows are preserved
#   original_rows <- result %>% filter(ANALYTE == "A")
#   expect_equal(nrow(original_rows), nrow(test_data))
#   expect_equal(original_rows$DV, test_data$DV)
#
#   # Check that result is still a nif object
#   expect_s3_class(result, "nif")
# })
#
#
# test_that("derive_cfb_analyte handles multiple subjects correctly", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     1,   "SUBJ-001",  0,     12,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     1,   "SUBJ-001",  1,     15,   "A",      0,    2,    0,     "2020-01-01 02:00:00",
#     2,   "SUBJ-002",  -1,    20,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     2,   "SUBJ-002",  0,     22,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     2,   "SUBJ-002",  1,     25,   "A",      0,    2,    0,     "2020-01-01 02:00:00",
#     3,   "SUBJ-003",  -1,    30,   "A",      0,    2,    0,     "2020-01-01 00:00:00",
#     3,   "SUBJ-003",  0,     32,   "A",      0,    2,    0,     "2020-01-01 01:00:00",
#     3,   "SUBJ-003",  1,     35,   "A",      0,    2,    0,     "2020-01-01 02:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   result <- derive_cfb_analyte(
#     test_nif,
#     source_analyte = "A",
#     silent = TRUE
#   )
#
#   # Check that each subject has correct baseline calculation
#   cfb_rows <- result %>% filter(ANALYTE == "CFB_A")
#
#   # ID 1: baseline = median(10, 12) = 11
#   id1_cfb <- cfb_rows %>% filter(ID == 1) %>% arrange(TIME)
#   expect_equal(id1_cfb$DV[1], -1)  # 10 - 11
#   expect_equal(id1_cfb$DV[2], 1)   # 12 - 11
#   expect_equal(id1_cfb$DV[3], 4)   # 15 - 11
#
#   # ID 2: baseline = median(20, 22) = 21
#   id2_cfb <- cfb_rows %>% filter(ID == 2) %>% arrange(TIME)
#   expect_equal(id2_cfb$DV[1], -1)  # 20 - 21
#   expect_equal(id2_cfb$DV[2], 1)   # 22 - 21
#   expect_equal(id2_cfb$DV[3], 4)   # 25 - 21
#
#   # ID 3: baseline = median(30, 32) = 31
#   id3_cfb <- cfb_rows %>% filter(ID == 3) %>% arrange(TIME)
#   expect_equal(id3_cfb$DV[1], -1)  # 30 - 31
#   expect_equal(id3_cfb$DV[2], 1)   # 32 - 31
#   expect_equal(id3_cfb$DV[3], 4)   # 35 - 31
# })
#
#
# test_that("derive_cfb_analyte handles silent parameter", {
#   test_data <- tibble::tribble(
#     ~ID, ~USUBJID,    ~TIME, ~DV,  ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DTC,
#     1,   "SUBJ-001",  -1,    10,   "A",      0,    2,    0,     "2020-01-01 00:00:00"
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(DTC))
#
#   test_nif <- nif(test_data)
#
#   # Test with silent = TRUE (should not produce messages)
#   expect_silent(
#     derive_cfb_analyte(test_nif, source_analyte = "A", silent = TRUE)
#   )
#
#   # Test with silent = FALSE (should produce messages)
#   expect_message(
#     derive_cfb_analyte(test_nif, source_analyte = "A", silent = FALSE),
#     "Compartment for"
#   )
# })
#
