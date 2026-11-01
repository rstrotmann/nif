test_that("validate_df_argument passes for plain data frames", {
  x <- data.frame(a = 1, b = "x", stringsAsFactors = FALSE)
  expect_invisible(validate_df_argument(x))
  expect_null(validate_df_argument(x))
})


test_that("validate_df_argument passes for tibbles", {
  x <- tibble::tibble(a = 1:3, b = letters[1:3])
  expect_invisible(validate_df_argument(x))
})


test_that("validate_df_argument passes for empty data frames", {
  x <- data.frame()
  expect_invisible(validate_df_argument(x))

  x <- data.frame(a = numeric(0), b = character(0), stringsAsFactors = FALSE)
  expect_invisible(validate_df_argument(x))
  expect_invisible(validate_df_argument(x, expected_fields = c("a", "b")))
})


test_that("validate_df_argument passes for single-column and multi-column frames", {
  x <- data.frame(USUBJID = "S1", stringsAsFactors = FALSE)
  expect_invisible(validate_df_argument(x))
  expect_invisible(validate_df_argument(x, expected_fields = "USUBJID"))

  x <- data.frame(
    USUBJID = c("S1", "S2"),
    SEX = c("F", "M"),
    ACTARMCD = c("TRT", "PBO"),
    stringsAsFactors = FALSE
  )
  expect_invisible(
    validate_df_argument(x, expected_fields = c("USUBJID", "SEX", "ACTARMCD"))
  )
})


test_that("validate_df_argument rejects non-data.frame values", {
  x <- list(a = 1)
  expect_error(validate_df_argument(x), "`x` must be a data frame")

  x <- matrix(1:4, nrow = 2)
  expect_error(validate_df_argument(x), "`x` must be a data frame")

  x <- c(a = 1, b = 2)
  expect_error(validate_df_argument(x), "`x` must be a data frame")

  x <- "not a frame"
  expect_error(validate_df_argument(x), "`x` must be a data frame")

  x <- 1
  expect_error(validate_df_argument(x), "`x` must be a data frame")

  x <- TRUE
  expect_error(validate_df_argument(x), "`x` must be a data frame")

  x <- function(z) z
  expect_error(validate_df_argument(x), "`x` must be a data frame")
})


test_that("validate_df_argument handles NULL", {
  x <- NULL
  expect_error(validate_df_argument(x), "`x` must be a data frame, not `NULL`")
  expect_invisible(validate_df_argument(x, allow_null = TRUE))
  expect_null(validate_df_argument(x, allow_null = TRUE))

  # allow_null short-circuits expected_fields checks
  expect_invisible(
    validate_df_argument(x, expected_fields = "USUBJID", allow_null = TRUE)
  )
})


test_that("validate_df_argument accepts all expected columns present", {
  x <- data.frame(
    USUBJID = "S1",
    SEX = "F",
    ACTARMCD = "TRT",
    EXTRA = 1,
    stringsAsFactors = FALSE
  )

  expect_invisible(validate_df_argument(x, expected_fields = NULL))
  expect_invisible(validate_df_argument(x, expected_fields = character(0)))
  expect_invisible(validate_df_argument(x, expected_fields = "USUBJID"))
  expect_invisible(
    validate_df_argument(x, expected_fields = c("USUBJID", "SEX", "ACTARMCD"))
  )

  # Extra columns beyond expected_fields are fine
  expect_invisible(
    validate_df_argument(x, expected_fields = c("USUBJID", "SEX"))
  )
})


test_that("validate_df_argument rejects missing expected columns", {
  x <- data.frame(
    USUBJID = "S1",
    SEX = "F",
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_argument(x, expected_fields = "ACTARMCD"),
    "Missing columns in x: ACTARMCD"
  )

  expect_error(
    validate_df_argument(x, expected_fields = c("USUBJID", "ACTARMCD")),
    "Missing columns in x: ACTARMCD"
  )

  expect_error(
    validate_df_argument(x, expected_fields = c("VSTESTCD", "VSSTRESN")),
    "Missing columns in x: VSTESTCD and VSSTRESN"
  )
})


test_that("validate_df_argument reports only missing columns", {
  x <- data.frame(
    USUBJID = "S1",
    VSTESTCD = "HEIGHT",
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_argument(
      x,
      expected_fields = c("USUBJID", "VSTESTCD", "VSSTRESN")
    ),
    "Missing columns in x: VSSTRESN"
  )
})


test_that("validate_df_argument allows duplicate expected field names", {
  x <- data.frame(a = 1, stringsAsFactors = FALSE)
  expect_invisible(validate_df_argument(x, expected_fields = c("a", "a")))
})


test_that("validate_df_argument is case-sensitive for column names", {
  x <- data.frame(Usujid = "S1", stringsAsFactors = FALSE)
  expect_error(
    validate_df_argument(x, expected_fields = "USUBJID"),
    "Missing columns in x: USUBJID"
  )
})


test_that("validate_df_argument uses the parameter name in error messages", {
  dm <- list(a = 1)
  expect_error(validate_df_argument(dm), "`dm` must be a data frame")

  vs <- NULL
  expect_error(validate_df_argument(vs), "`vs` must be a data frame, not `NULL`")

  subjects <- data.frame(USUBJID = "S1", stringsAsFactors = FALSE)
  expect_error(
    validate_df_argument(subjects, expected_fields = c("USUBJID", "SEX")),
    "Missing columns in subjects: SEX"
  )
})


test_that("validate_df_argument works with zero-row frames that have columns", {
  x <- data.frame(
    USUBJID = character(0),
    SEX = character(0),
    ACTARMCD = character(0),
    stringsAsFactors = FALSE
  )

  expect_invisible(
    validate_df_argument(x, expected_fields = c("USUBJID", "SEX", "ACTARMCD"))
  )
  expect_error(
    validate_df_argument(x, expected_fields = c("USUBJID", "AGE")),
    "Missing columns in x: AGE"
  )
})


test_that("validate_df_argument does not require rows, only structure", {
  x <- data.frame(a = 1:5, b = 6:10)
  expect_invisible(validate_df_argument(x, expected_fields = c("a", "b")))

  # Column presence is independent of NA values in cells
  x <- data.frame(a = NA_real_, b = NA_character_, stringsAsFactors = FALSE)
  expect_invisible(validate_df_argument(x, expected_fields = c("a", "b")))
})


test_that("validate_df_argument reports the calling function in errors", {
  f <- function(dm) validate_df_argument(dm, expected_fields = "SEX")

  err <- expect_error(f(list(a = 1)), "`dm` must be a data frame")
  expect_equal(err$call, quote(f(list(a = 1))))

  err <- expect_error(f(data.frame(a = 1)), "Missing columns in dm: SEX")
  expect_equal(err$call, quote(f(data.frame(a = 1))))
})


test_that("validate_fields checks data frame and required fields", {
  x <- data.frame(a = 1, b = 2)

  expect_invisible(validate_fields(x))
  expect_invisible(validate_fields(x, c("a", "b")))
  expect_error(validate_fields(x, c("a", "z")), "Missing required fields: z")
  expect_error(validate_fields(list(a = 1)), "`list\\(a = 1\\)` must be a data frame")
  expect_error(validate_fields(x, 1), "fields must be a character value")
})
