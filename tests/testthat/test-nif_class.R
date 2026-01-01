test_that("nif works", {
  suppressMessages(expect_equal(
    dim(nif()),
    c(0, 6)))

  suppressMessages(expect_no_error(
    nif(examplinib_sad, RS2023 ~ EXAMPLINIB)
  ))
})


test_that("subject_info works", {
  expect_no_error(invisible(capture.output(subject_info(examplinib_sad_nif, 1))))
  expect_no_error(invisible(capture.output(
    print(subject_info(examplinib_sad_nif, 1))
  )))
})


test_that("parents works", {
  expect_true(length(parents(examplinib_sad_nif)) > 0)
})


test_that("rich_sampling_sbs works", {
  expect_equal(
    rich_sampling_sbs(examplinib_fe_nif),
    subjects(examplinib_fe_nif)$ID
  )
})


test_that("studies works", {
  expect_equal(length(studies(examplinib_fe_nif)), 1)
})


test_that("ensure works", {
  expect_contains(names(ensure_analyte(examplinib_sad_min_nif)), "ANALYTE")
  expect_contains(names(ensure_dose(examplinib_sad_min_nif)), "DOSE")
  expect_contains(names(ensure_parent(examplinib_sad_min_nif)), "PARENT")
  expect_contains(names(ensure_metabolite(examplinib_sad_min_nif)), "METABOLITE")
  expect_contains(names(ensure_tad(examplinib_sad_min_nif)), "TAD")
  expect_contains(names(ensure_tafd(examplinib_sad_min_nif)), "TAFD")
  expect_contains(names(ensure_time(examplinib_sad_min_nif)), "TIME")
  expect_contains(names(ensure_cfb(examplinib_sad_min_nif)), "DVCFB")
})


test_that("doses works", {
  expect_gt(length(doses(examplinib_sad_nif)), 0)
})


test_that("doses works with minimal NIF", {
  expect_no_error(doses(examplinib_poc_min_nif))
})

test_that("dose_levels works", {
  expect_gt(length(dose_levels(examplinib_sad_nif)), 0)
})


test_that("treatments works", {
  expect_gt(length(treatments(examplinib_sad_nif)), 0)
  expect_gt(length(treatments(examplinib_poc_min_nif)), 0)
})


test_that("dose_levels works with minimal NIF", {
  expect_no_error(dose_levels(examplinib_poc_min_nif))
})


test_that("analytes works", {
  expect_gt(length(analytes(examplinib_sad_nif)), 0)
})


test_that("analytes works with minimal NIF and rich NIF", {
  expect_no_error(analytes(examplinib_poc_min_nif))
  expect_no_error(analytes(examplinib_poc_nif))
})


test_that("analyte_overview works", {
  expect_gt(nrow(analyte_overview(examplinib_poc_nif)), 0)
  expect_gt(nrow(analyte_overview(examplinib_poc_min_nif)), 0)
})


test_that("cmt_mapping works", {
  expect_gt(nrow(compartments(examplinib_poc_nif)), 0)
  expect_gt(nrow(compartments(examplinib_poc_min_nif)), 0)
})


test_that("index_dosing_interval works with single parent", {
  nif <- tribble(
    ~ID, ~TIME,  ~AMT, ~RATE, ~EVID, ~DV,  ~CMT, ~MDV, ~PARENT,
    1,   0.00,   0,    0,     0,     0,   2,    0,    "A",
    1,   0,      100,  0,     1,     NA,  1,    0,    "A",
    1,   1.00,   0,    0,     0,     1,   2,    0,    "A",
    1,   2.00,   0,    0,     0,     2,   2,    0,    "A",
    1,   4.00,   0,    0,     0,     3,   2,    0,    "A",
    1,   24,     100,  0,     1,     NA,   1,    0,    "A",
    1,   48,     100,  0,     1,     NA,   1,    0,    "A",
    1,   72,     100,  0,     1,     NA,   1,    0,    "A",
    1,   73.00,  0,    0,     0,     1,   2,    0,    "A",
    1,   74.00,  0,    0,     0,     2,   2,    0,    "A",
    1,   76.00,  0,    0,     0,     3,   2,    0,    "A"
  ) %>%
    nif()

  temp <- nif %>%
    index_dosing_interval() %>%
    as.data.frame()

  expect_equal(max(temp$DI), 4)
})


test_that("index_dosing_interval works with multiple parents", {
  nif <- tribble(
    ~ID, ~TIME,  ~AMT, ~RATE, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT,
    1,   0.00,   0,    0,     0,     0,   2,    0,    "A",
    1,   0,      100,  0,     1,     NA,  1,    0,    "A",
    1,   1.00,   0,    0,     0,     1,   2,    0,    "A",
    1,   2.00,   0,    0,     0,     2,   2,    0,    "A",
    1,   4.00,   0,    0,     0,     3,   2,    0,    "A",
    1,   24,     100,  0,     1,     NA,  1,    0,    "A",
    1,   48,     100,  0,     1,     NA,  1,    0,    "A",
    1,   72,     100,  0,     1,     NA,  1,    0,    "A",
    1,   73.00,  0,    0,     0,     1,   2,    0,    "A",
    1,   74.00,  0,    0,     0,     2,   2,    0,    "A",
    1,   76.00,  0,    0,     0,     3,   2,    0,    "A",
    1,   0.00,   0,    0,     0,     0,   2,    0,    "B",
    1,   0,      100,  0,     1,     NA,  1,    0,    "B",
    1,   1.00,   0,    0,     0,     1,   2,    0,    "B",
    1,   2.00,   0,    0,     0,     2,   2,    0,    "B",
    1,   4.00,   0,    0,     0,     3,   2,    0,    "B",
    1,   48,     100,  0,     1,     NA,  1,    0,    "B",
    1,   96,     100,  0,     1,     NA,  1,    0,    "B",
    1,   97.00,  0,    0,     0,     1,   2,    0,    "B",
    1,   98.00,  0,    0,     0,     2,   2,    0,    "B",
    1,   100.00, 0,    0,     0,     3,   2,    0,    "B"
  ) %>%
    nif() %>%
    index_nif() %>%
    index_dosing_interval()

  temp <- nif %>%
    group_by(PARENT) %>%
    summarize(max_di = max(DI))

  expect_equal(temp$max_di, c(4, 3))

  nif %>%
    filter(PARENT == "A", DI == 4, EVID == 0) %>%
    nrow() %>%
    expect_equal(3)

  nif %>%
    filter(PARENT == "B", DI == 3, EVID == 0) %>%
    as.data.frame() %>%
    nrow() %>%
    expect_equal(3)
})


test_that("n_administrations, max_admin_time works, max_observation_time", {
  nif <- tribble(
    ~ID, ~TIME,  ~AMT, ~RATE, ~EVID, ~DV, ~CMT, ~MDV, ~PARENT,
    1,   0.00,   0,    0,     0,     0,   2,    0,    "A",
    1,   0,      100,  0,     1,     NA,  1,    0,    "A",
    1,   1.00,   0,    0,     0,     1,   2,    0,    "A",
    1,   2.00,   0,    0,     0,     2,   2,    0,    "A",
    1,   4.00,   0,    0,     0,     3,   2,    0,    "A",
    1,   24,     100,  0,     1,     NA,  1,    0,    "A",
    1,   48,     100,  0,     1,     NA,  1,    0,    "A",
    1,   72,     100,  0,     1,     NA,  1,    0,    "A",
    1,   73.00,  0,    0,     0,     1,   2,    0,    "A",
    1,   74.00,  0,    0,     0,     2,   2,    0,    "A",
    1,   76.00,  0,    0,     0,     3,   2,    0,    "A",
    1,   0.00,   0,    0,     0,     0,   2,    0,    "B",
    1,   0,      100,  0,     1,     NA,  1,    0,    "B",
    1,   1.00,   0,    0,     0,     1,   2,    0,    "B",
    1,   2.00,   0,    0,     0,     2,   2,    0,    "B",
    1,   4.00,   0,    0,     0,     3,   2,    0,    "B",
    1,   48,     100,  0,     1,     NA,  1,    0,    "B",
    1,   96,     100,  0,     1,     NA,  1,    0,    "B",
    1,   97.00,  0,    0,     0,     1,   2,    0,    "B",
    1,   98.00,  0,    0,     0,     2,   2,    0,    "B",
    1,   100.00, 0,    0,     0,     3,   2,    0,    "B"
  ) %>%
    nif() %>%
    index_nif() %>%
    index_dosing_interval()

  expect_equal(n_administrations(nif)$N, c(4, 3))
  expect_equal(max_admin_time(nif), 96)
  expect_equal(max_observation_time(nif), 100)
  expect_equal(max_time(nif), 100)
})


test_that("guess analyte, guess_parent works", {
  expect_equal(guess_analyte(examplinib_poc_nif), "RS2023")
  expect_equal(guess_parent(examplinib_poc_nif), "RS2023")
})


test_that("add_dose_level works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   24,    90,   1,    1,     NA,
    1,   48,    80,   1,    1,     NA,
    2,   0,     100,  1,    1,     NA,
    3,   0,     100,  1,    1,     NA,
    3,   24,    NA,   1,    1,     NA,
    3,   48,    150,  1,    1,     NA
  ) %>% nif()

  temp <- add_dose_level(nif) %>%
    distinct(ID, DL)
  expect_equal(nrow(temp), 3)
  expect_equal(unique(temp$DL), 100)
})


# test_that("add_tad, add_tafd works", {
#   nif <- tibble::tribble(
#     ~ID, ~TIME, ~AMT, ~CMT, ~EVID,
#     1,     0,  100,    1,     1,
#     1,     0,   NA,    1,     0,
#     1,     1,   NA,    2,     0,
#     1,     2,   NA,    2,     0,
#     1,    24,   90,    1,     1,
#     1,    48,   80,    1,     1,
#     1,    50,   NA,    2,     0,
#     2,    10,  100,    1,     1,
#     2,    11,   NA,    2,     0,
#     2,    12,   NA,    2,     0,
#     2,    34,   90,    1,     1,
#     2,    58,   80,    1,     1,
#     2,    60,   NA,    2,     0
#   ) %>%
#     nif()
#
#   expect_equal(
#     add_tad(nif)$TAD,
#     c(0, 0, 1, 2, 0, 0, 2, 0, 1, 2, 0, 0, 2))
#   expect_equal(
#     add_tafd(nif)$TAFD,
#     c(0, 0, 1, 2, 24, 48, 50, 0, 1, 2, 24, 48, 50))
# })


test_that("add_trtdy works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DTC,                        ~DV,
    1,   0,     100,  1,    1,     "2024-12-06 07:31:35.14839", NA,
    1,   0,     NA,   1,    0,     "2024-12-06 07:31:35.14839", 10,
    1,   1,     NA,   2,    0,     "2024-12-06 08:31:35.14839", 20,
    1,   2,     NA,   2,    0,     "2024-12-06 09:31:35.14839", 30,
    1,   24,    90,   1,    1,     "2024-12-07 07:31:35.14839", NA,
    1,   48,    80,   1,    1,     "2024-12-08 07:31:35.14839", NA,
    1,   50,    NA,   2,    0,     "2024-12-08 09:31:35.14839", 40,
    2,   10,    100,  1,    1,     "2024-12-06 17:31:35.14839", NA,
    2,   11,    NA,   2,    0,     "2024-12-06 18:31:35.14839", 50,
    2,   12,    NA,   2,    0,     "2024-12-06 19:31:35.14839", 60,
    2,   34,    90,   1,    1,     "2024-12-07 17:31:35.14839", NA,
    2,   58,    80,   1,    1,     "2024-12-08 17:31:35.14839", NA,
    2,   60,    NA,   2,    0,     "2024-12-08 19:31:35.14839", 70
  ) %>%
    mutate(DTC = as.POSIXct(DTC))
  expect_equal(
    add_trtdy(nif)$TRTDY,
    c(1, 1, 1, 1, 2, 3, 3, 1, 1, 1, 2, 3, 3)
  )
})


test_that("index_rich_sampling_intervals works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~DV,
    1,   0,     1,     1,    100,  NA,
    1,   0,     0,     2,    0,    10,
    1,   2,     0,     2,    0,    20,
    1,   4,     0,     2,    0,    30,
    1,   6,     0,     2,    0,    40,
    1,   8,     0,     2,    0,    50,
    1,   24,    1,     1,    100,  NA,
    1,   48,    1,     1,    100,  NA,
    1,   72,    1,     1,    100,  NA,
    1,   72,    0,     2,    0,    60,
    1,   72,    1,     1,    100,  NA,
    1,   96,    1,     1,    100,  NA,
    1,   96,    0,     2,    0,    70,
    1,   97,    0,     2,    0,    80,
    1,   98,    0,     2,    0,    90,
    1,   100,   0,     2,    0,    100,
    1,   102,   0,     2,    0,    110,
    1,   104,   0,     2,    0,    120
  ) %>%
    nif()

  temp <- as.data.frame(index_rich_sampling_intervals(nif))
  expect_equal(unique(temp$RICH_N), c(NA, 1, 2))
  temp1 <- distinct(temp, OPDI, RICH_N)
  expect_equal(temp1[which(temp1$OPDI == 5), "RICH_N"], 1)
  expect_equal(temp1[which(temp1$OPDI == 6), "RICH_N"], 2)
})


test_that("cfb works", {
  obj <- tribble(
    ~ID, ~ANALYTE, ~EVID, ~TIME, ~DV,  ~AMT, ~CMT,
    1,   "A",      0,     0,     NA,  0,    2,
    1,   "A",      0,     1,     1,   0,    2,
    1,   "A",      0,     2,     2,   0,    2,
    1,   "A",      0,     3,     3,   0,    2,
    1,   "A",      0,     4,     4,   0,    2,
    1,   "A",      0,     5,     5,   0,    2
  ) %>%
    nif()

  expect_no_error(
    test <- obj %>%
      # derive_cfb(baseline_filter = "TIME < 4", summary_function = last) %>%
      derive_cfb(baseline_filter = "TIME < 4", summary_function = last) %>%
      as.data.frame() %>%
      pull(DVBL)
  )

  expect_equal(unique(test), 3)
})


test_that("analyte_overview", {
  expect_no_error(
    temp <- analyte_overview(examplinib_poc_nif)
  )
  expect_equal(dim(temp), c(2, 2))
})


test_that("write_nif works", {
  expect_no_error(invisible(capture.output(write_nonmem(examplinib_sad_nif))))
  expect_no_error(invisible(capture.output(write_monolix(examplinib_sad_nif))))
})


test_that("print.nif works", {
  expect_no_error(invisible(capture.output(print(examplinib_sad_nif))))
})


test_that("add_rtb works", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT,
    1,   0,     1,     1,    NA,   100,
    1,   0,     0,     2,    100,  0,
    1,   2,     0,     2,    200,  0,
    1,   4,     0,     2,    300,  0,
    1,   24,    1,     1,    NA,   100,
    1,   48,    1,     1,    NA,   100,
    1,   72,    1,     1,    NA,   100,
    1,   72,    0,     2,    400,  0,
    2,   0,     1,     1,    NA,   100,
    2,   0,     0,     2,    10,   0,
    2,   2,     0,     2,    20,   0,
    2,   4,     0,     2,    30,   0,
    2,   72,    0,     2,    40,   0
  ) %>%
    nif()

  temp <- as.data.frame(add_rtb(nif))
  expect_equal(
    temp$DVRTB,
    c(NA, 1, 2, 3, NA, NA, NA, 4, NA, 1, 2, 3, 4)
  )
})


test_that("subjects.nif works with standard NIF object", {
  result <- subjects(examplinib_sad_nif)

  # Check return type
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("ID", "USUBJID"))

  # Check that we get unique subjects
  expect_equal(nrow(result), length(unique(examplinib_sad_nif$ID)))

  # Check that all IDs are present
  expect_equal(sort(result$ID), sort(unique(examplinib_sad_nif$ID)))

  # Check that USUBJID values are character
  expect_type(result$USUBJID, "character")
})


test_that("subjects.nif works with minimal NIF object", {
  # Create minimal NIF without USUBJID
  minimal_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    1,   0,     10,   1,    1,     10,    NA,
    1,   1,     0,    2,    0,     10,    5.2,
    1,   2,     0,    2,    0,     10,    4.8,
    2,   0,     10,   1,    1,     10,    NA,
    2,   1,     0,    2,    0,     10,    6.1,
    2,   2,     0,    2,    0,     10,    5.9
  ) %>%
    nif()

  result <- subjects(minimal_nif)

  # Check return type
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("ID", "USUBJID"))

  # Check that USUBJID column is filled with NA
  expect_true(all(is.na(result$USUBJID)))

  # Check that we get unique subjects
  expect_equal(nrow(result), length(unique(minimal_nif$ID)))
})


test_that("subjects.nif works with empty NIF object", {
  empty_nif <- nif()
  result <- subjects(empty_nif)

  # Check return type
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("ID", "USUBJID"))
  expect_equal(nrow(result), 0)
})


test_that("subjects.nif handles NA values in ID correctly", {
  na_id_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~USUBJID,
    1,   0,     10,   1,    1,     10,    NA,  "SUBJ001",
    NA,  1,     0,    2,    0,     10,    5.2, "SUBJ002", # NA ID
    2,   0,     10,   1,    1,     10,    NA,  "SUBJ003",
    2,   1,     0,    2,    0,     10,    6.1, "SUBJ003"
  ) %>%
    nif()

  result <- subjects(na_id_nif)

  # Should include NA ID as a distinct value
  expect_equal(nrow(result), 3)
  expect_true(any(is.na(result$ID)))
  expect_equal(sort(result$ID, na.last = TRUE), c(1, 2, NA))
})


test_that("subjects.nif works with only ID column", {
  id_only_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    1,   0,     10,   1,    1,     10,    NA,
    1,   1,     0,    2,    0,     10,    5.2,
    2,   0,     10,   1,    1,     10,    NA,
    2,   1,     0,    2,    0,     10,    6.1
  ) %>%
    nif()

  result <- subjects(id_only_nif)

  # Should create USUBJID column filled with NA
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("ID", "USUBJID"))
  expect_true(all(is.na(result$USUBJID)))
  expect_equal(nrow(result), 2)
})


test_that("subjects.nif works with only USUBJID column", {
  usubjid_only_nif <- tibble::tribble(
    ~USUBJID,  ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    "SUBJ001", 0,     10,   1,    1,     10,    NA,
    "SUBJ001", 1,     0,    2,    0,     10,    5.2,
    "SUBJ002", 0,     10,   1,    1,     10,    NA,
    "SUBJ002", 1,     0,    2,    0,     10,    6.1
  ) %>%
    nif()

  expect_error(
    subjects(usubjid_only_nif),
    "Missing essential fields in nif object: ID"
  )
})


test_that("subjects.nif works with neither ID nor USUBJID columns", {
  no_id_nif <- tibble::tribble(
    ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    0,     10,   1,    1,     10,    NA,
    1,     0,    2,    0,     10,    5.2,
    2,     0,    2,    0,     10,    4.8
  ) %>%
    nif()

  expect_error(
    result <- subjects(no_id_nif),
    "Missing essential fields in nif object: ID"
  )
})


test_that("subjects.nif preserves data types", {
  result <- subjects(examplinib_sad_nif)

  # ID should be numeric
  expect_type(result$ID, "double")

  # USUBJID should be character
  expect_type(result$USUBJID, "character")
})


test_that("usubjid works with valid single ID", {
  # Test with a valid single ID
  result <- usubjid(examplinib_sad_nif, 1)
  expect_type(result, "character")
  expect_equal(length(result), 1)

  # Verify it matches the subjects function result
  subjects_df <- subjects(examplinib_sad_nif)
  expected <- subjects_df$USUBJID[subjects_df$ID == 1]
  expect_equal(result, expected)
})


test_that("usubjid works with multiple IDs", {
  # Test with multiple valid IDs
  result <- usubjid(examplinib_sad_nif, c(1, 2))
  expect_type(result, "character")
  expect_equal(length(result), 2)

  # Verify it matches the subjects function result
  subjects_df <- subjects(examplinib_sad_nif)
  expected <- subjects_df$USUBJID[subjects_df$ID %in% c(1, 2)]
  expect_equal(result, expected)
})


test_that("usubjid works with minimal NIF", {
  # Create minimal NIF without USUBJID
  minimal_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV,
    1,   0,     10,   1,    1,     10,    NA,
    1,   1,     0,    2,    0,     10,    5.2,
    2,   0,     10,   1,    1,     10,    NA,
    2,   1,     0,    2,    0,     10,    6.1
  ) %>%
    nif()

  # Should error because USUBJID field is not found
  expect_error(
    usubjid(minimal_nif, 1),
    "USUBJID field not found"
  )
})


test_that("usubjid handles missing IDs gracefully", {
  # Test with IDs that don't exist in the data
  expect_message(
    result <- usubjid(examplinib_sad_nif, c(999, 1000), silent = FALSE),
    "IDs not found: 999 and 1000"
  )
  expect_type(result, "character")
  expect_equal(length(result), 0)
})


test_that("usubjid handles mixed valid and invalid IDs", {
  # Test with mix of valid and invalid IDs
  subjects_df <- subjects(examplinib_sad_nif)
  valid_ids <- subjects_df$ID[1:2] # First two valid IDs

  expect_message(
    result <- usubjid(examplinib_sad_nif, c(valid_ids, 999, 1000), silent = FALSE),
    "IDs not found: 999 and 1000"
  )
  expect_type(result, "character")
  expect_equal(length(result), 2) # Only valid IDs should be returned

  # Verify it matches the subjects function result for valid IDs
  expected <- subjects_df$USUBJID[subjects_df$ID %in% valid_ids]
  expect_equal(result, expected)
})


test_that("usubjid validates input parameters", {
  # Test with non-numeric ID
  expect_error(
    usubjid(examplinib_sad_nif, "1"),
    "must be a numeric value"
  )

  # Test with logical ID
  expect_error(
    usubjid(examplinib_sad_nif, TRUE),
    "must be a numeric value"
  )
})


test_that("usubjid works with empty NIF", {
  empty_nif <- nif()

  # Should error because USUBJID field is not found
  expect_error(
    usubjid(empty_nif, 1),
    "USUBJID field not found"
  )
})


test_that("usubjid works with NIF containing NA USUBJID", {
  # Create NIF with NA USUBJID values
  na_usubjid_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DOSE, ~DV, ~USUBJID,
    1,   0,     10,   1,    1,     10,    NA,  "SUBJ001",
    2,   0,     10,   1,    1,     10,    NA,  NA, # NA USUBJID
    3,   0,     10,   1,    1,     10,    NA,  "SUBJ003"
  ) %>%
    nif()

  # Test with ID that has NA USUBJID
  result <- usubjid(na_usubjid_nif, 2)
  expect_type(result, "character")
  expect_true(is.na(result))

  # Test with ID that has valid USUBJID
  result <- usubjid(na_usubjid_nif, 1)
  expect_type(result, "character")
  expect_equal(result, "SUBJ001")
})


test_that("usubjid works with silent parameter", {
  # Test with silent = TRUE
  result <- usubjid(examplinib_sad_nif, c(1, 999), silent = TRUE)
  expect_type(result, "character")
  expect_equal(length(result), 1) # Only valid ID returned

  # Test with silent = FALSE (should show message about missing ID)
  expect_message(
    usubjid(examplinib_sad_nif, c(1, 999), silent = FALSE),
    "ID not found"
  )
})


test_that("usubjid handles edge cases", {
  # Test with zero ID
  expect_message(
    result <- usubjid(examplinib_sad_nif, 0, silent = FALSE),
    "ID not found"
  )
  expect_type(result, "character")
  expect_equal(length(result), 0)

  # Test with negative ID
  expect_message(
    result <- usubjid(examplinib_sad_nif, -1, silent = FALSE),
    "ID not found"
  )
  expect_type(result, "character")
  expect_equal(length(result), 0)

  # Test with decimal ID
  expect_message(
    result <- usubjid(examplinib_sad_nif, 1.5, silent = FALSE),
    "ID not found"
  )
  expect_type(result, "character")
  expect_equal(length(result), 0)
})


test_that("usubjid validates NIF object", {
  # Test with non-NIF object
  non_nif_df <- data.frame(ID = 1, USUBJID = "TEST")
  expect_error(
    usubjid(non_nif_df, 1),
    "Input must be a nif object"
  )
})

