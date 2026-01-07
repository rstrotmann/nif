test_that("adam constructor creates adam object from list of data frames", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE, ~SEX,
    "SUBJ-001", "STUDY-001", 30, "M",
    "SUBJ-002", "STUDY-001", 25, "F"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~EXTRT, ~EXDOSE,
    "SUBJ-001", "STUDY-001", "DRUG A", 100,
    "SUBJ-002", "STUDY-001", "DRUG A", 100
  )

  adam_obj <- adam(list(dm = dm, ex = ex))

  expect_s3_class(adam_obj, "adam")
  expect_s3_class(adam_obj, "list")
  expect_equal(length(adam_obj), 2)
  expect_equal(names(adam_obj), c("dm", "ex"))
  expect_equal(adam_obj$dm, dm)
  expect_equal(adam_obj$ex, ex)
})


test_that("adam constructor works with single data frame", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30
  )

  adam_obj <- adam(list(dm = dm))

  expect_s3_class(adam_obj, "adam")
  expect_equal(length(adam_obj), 1)
  expect_equal(names(adam_obj), "dm")
})


test_that("adam constructor works with empty list", {
  adam_obj <- adam(list())

  expect_s3_class(adam_obj, "adam")
  expect_s3_class(adam_obj, "list")
  expect_equal(length(adam_obj), 0)
})


test_that("adam constructor works with named list", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))

  expect_equal(names(adam_obj), c("dm", "ex"))
})


test_that("adam constructor works with unnamed list", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm, ex))

  expect_s3_class(adam_obj, "adam")
  expect_equal(length(adam_obj), 2)
})


test_that("validate_adam passes for valid adam object", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm))

  expect_no_error(validate_adam(adam_obj))
})


test_that("validate_adam throws error for non-adam object", {
  regular_list <- list(a = 1, b = 2)
  expect_error(
    validate_adam(regular_list),
    "Input must be a adam object"
  )

  data_frame <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )
  expect_error(
    validate_adam(data_frame),
    "Input must be a adam object"
  )

  expect_error(
    validate_adam(NULL),
    "Input must be a adam object"
  )

  expect_error(
    validate_adam("string"),
    "Input must be a adam object"
  )
})


test_that("summary.adam extracts study IDs correctly", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-002"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_true("STUDY-001" %in% summary_obj$study)
  expect_true("STUDY-002" %in% summary_obj$study)
  expect_equal(length(summary_obj$study), 2)
})


test_that("summary.adam handles datasets without STUDYID", {
  dm <- tibble::tribble(
    ~USUBJID, ~AGE,
    "SUBJ-001", 30
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_true("STUDY-001" %in% summary_obj$study)
  expect_equal(length(summary_obj$study), 1)
})


test_that("summary.adam extracts subject IDs correctly", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-002", "STUDY-001",
    "SUBJ-003", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_true("SUBJ-001" %in% summary_obj$subjects)
  expect_true("SUBJ-002" %in% summary_obj$subjects)
  expect_true("SUBJ-003" %in% summary_obj$subjects)
  expect_equal(length(summary_obj$subjects), 3)
})


test_that("summary.adam handles datasets without USUBJID", {
  dm <- tibble::tribble(
    ~STUDYID, ~AGE,
    "STUDY-001", 30
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_true("SUBJ-001" %in% summary_obj$subjects)
  expect_equal(length(summary_obj$subjects), 1)
})


test_that("summary.adam counts observations correctly", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_s3_class(summary_obj$n_observations, "data.frame")
  expect_equal(nrow(summary_obj$n_observations), 2)
  expect_equal(summary_obj$n_observations$OBSERVATIONS[summary_obj$n_observations$DATASET == "dm"], 2)
  expect_equal(summary_obj$n_observations$OBSERVATIONS[summary_obj$n_observations$DATASET == "ex"], 3)
})


test_that("summary.adam handles empty datasets", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_equal(summary_obj$n_observations$OBSERVATIONS[summary_obj$n_observations$DATASET == "dm"], 0)
  expect_equal(summary_obj$n_observations$OBSERVATIONS[summary_obj$n_observations$DATASET == "ex"], 1)
})


test_that("summary.adam includes dataset names", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  pc <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex, pc = pc))
  summary_obj <- summary(adam_obj)

  expect_equal(summary_obj$datasets, c("dm", "ex", "pc"))
  expect_equal(summary_obj$n_observations$DATASET, c("dm", "ex", "pc"))
})


test_that("summary.adam handles duplicate study IDs across datasets", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_equal(length(summary_obj$study), 1)
  expect_true("STUDY-001" %in% summary_obj$study)
})


test_that("summary.adam handles duplicate subject IDs across datasets", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_equal(length(summary_obj$subjects), 1)
  expect_true("SUBJ-001" %in% summary_obj$subjects)
})


test_that("summary.adam preserves adam object reference", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm))
  summary_obj <- summary(adam_obj)

  expect_equal(summary_obj$adam, adam_obj)
})


test_that("print.summary_adam produces output", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_output(print(summary_obj), "ADaM data set summary")
  expect_output(print(summary_obj), "Data from")
  expect_output(print(summary_obj), "subjects")
  expect_output(print(summary_obj), "study")
  expect_output(print(summary_obj), "Data disposition")
})


test_that("print.summary_adam handles single study correctly", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm))
  summary_obj <- summary(adam_obj)

  expect_output(print(summary_obj), "study")
})


test_that("print.summary_adam handles multiple studies correctly", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-002", "STUDY-002"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_output(print(summary_obj), "studies")
})


test_that("print.summary_adam displays observation counts", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))
  summary_obj <- summary(adam_obj)

  expect_output(print(summary_obj), "DATASET")
  expect_output(print(summary_obj), "OBSERVATIONS")
})


test_that("print.adam calls summary and prints", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm))

  expect_output(print(adam_obj), "ADaM data set summary")
})


test_that("dataset retrieves correct dataset from adam object", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30,
    "SUBJ-002", "STUDY-001", 25
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~EXTRT,
    "SUBJ-001", "STUDY-001", "DRUG A"
  )

  adam_obj <- adam(list(dm = dm, ex = ex))

  result_dm <- dataset(adam_obj, "dm")
  result_ex <- dataset(adam_obj, "ex")

  expect_equal(result_dm, dm)
  expect_equal(result_ex, ex)
  expect_equal(nrow(result_dm), 2)
  expect_equal(nrow(result_ex), 1)
})


test_that("dataset throws error for non-adam object", {
  regular_list <- list(dm = data.frame(x = 1))
  expect_error(
    dataset(regular_list, "dm"),
    "Input must be a adam object"
  )
})


test_that("dataset throws error for missing dataset name", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm))

  expect_error(
    dataset(adam_obj, "nonexistent"),
    "Dataset nonexistent not found in adam object!"
  )
})


test_that("dataset throws error for invalid name parameter", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm))

  expect_error(
    dataset(adam_obj, NULL),
    "must not be NULL"
  )

  expect_error(
    dataset(adam_obj, 123),
    "must be a string value"
  )

  expect_error(
    dataset(adam_obj, c("dm", "ex")),
    "must be a single value"
  )
})


test_that("dataset works with empty dataset", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID
  )

  adam_obj <- adam(list(dm = dm))

  result <- dataset(adam_obj, "dm")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("USUBJID", "STUDYID"))
})


test_that("dataset preserves data frame attributes", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID, ~AGE,
    "SUBJ-001", "STUDY-001", 30
  )

  attr(dm, "label") <- "Demographics"
  adam_obj <- adam(list(dm = dm))

  result <- dataset(adam_obj, "dm")
  expect_equal(attr(result, "label"), "Demographics")
})


test_that("dataset works with multiple datasets", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  pc <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  vs <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm, ex = ex, pc = pc, vs = vs))

  expect_equal(dataset(adam_obj, "dm"), dm)
  expect_equal(dataset(adam_obj, "ex"), ex)
  expect_equal(dataset(adam_obj, "pc"), pc)
  expect_equal(dataset(adam_obj, "vs"), vs)
})


test_that("summary.adam handles NA values in STUDYID", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    "SUBJ-002", NA
  )

  adam_obj <- adam(list(dm = dm))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_true("STUDY-001" %in% summary_obj$study)
})


test_that("summary.adam handles NA values in USUBJID", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID,
    "SUBJ-001", "STUDY-001",
    NA, "STUDY-001"
  )

  adam_obj <- adam(list(dm = dm))
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_true("SUBJ-001" %in% summary_obj$subjects)
})


test_that("adam constructor works with data frames containing no rows", {
  dm <- tibble::tribble(
    ~USUBJID, ~STUDYID
  )

  adam_obj <- adam(list(dm = dm))

  expect_s3_class(adam_obj, "adam")
  expect_equal(length(adam_obj), 1)
  expect_equal(nrow(adam_obj$dm), 0)
})


test_that("summary.adam works with empty adam object", {
  adam_obj <- adam(list())
  summary_obj <- summary(adam_obj)

  expect_s3_class(summary_obj, "summary_adam")
  expect_equal(length(summary_obj$study), 0)
  expect_equal(length(summary_obj$subjects), 0)
  expect_equal(nrow(summary_obj$n_observations), 0)
  expect_equal(length(summary_obj$datasets), 0)
})

