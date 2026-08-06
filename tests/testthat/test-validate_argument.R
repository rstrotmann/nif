test_that("validate_argument passes for valid character values", {
  x <- "hello"
  expect_invisible(validate_argument(x, "character"))
  expect_null(validate_argument(x, "character"))

  # default type is character
  expect_invisible(validate_argument(x))
})


test_that("validate_argument rejects non-character values", {
  x <- 123
  expect_error(validate_argument(x, "character"), "x must be a character value")

  x <- TRUE
  expect_error(validate_argument(x, "character"), "x must be a character value")

  x <- factor("a")
  expect_error(validate_argument(x, "character"), "x must be a character value")

  x <- list("a")
  expect_error(validate_argument(x, "character"), "x must be a character value")
})


test_that("validate_argument handles NULL for character", {
  x <- NULL
  expect_error(validate_argument(x, "character"), "x must not be NULL")
  expect_invisible(validate_argument(x, "character", allow_null = TRUE))
})


test_that("validate_argument handles NA for character", {
  x <- NA_character_
  expect_error(validate_argument(x, "character"), "x must not contain NA")
  expect_invisible(
    validate_argument(x, "character", allow_na = TRUE, allow_empty = TRUE)
  )

  x <- c("a", NA_character_)
  expect_error(
    validate_argument(x, "character", allow_multiple = TRUE),
    "x must not contain NA"
  )
  expect_invisible(
    validate_argument(x, "character", allow_multiple = TRUE, allow_na = TRUE)
  )
})


test_that("validate_argument handles empty character strings", {
  x <- ""
  expect_error(validate_argument(x, "character"), "x must be a non-empty string")
  expect_invisible(validate_argument(x, "character", allow_empty = TRUE))

  x <- c("", "a")
  expect_error(
    validate_argument(x, "character", allow_multiple = TRUE),
    "x must be a non-empty string"
  )
  expect_invisible(
    validate_argument(x, "character", allow_multiple = TRUE, allow_empty = TRUE)
  )

  x <- c("a", "")
  expect_error(
    validate_argument(x, "character", allow_multiple = TRUE),
    "x must be a non-empty string"
  )
})


test_that("validate_argument treats whitespace-only strings as non-empty", {
  x <- " "
  expect_invisible(validate_argument(x, "character"))

  x <- c("a", "  ")
  expect_invisible(validate_argument(x, "character", allow_multiple = TRUE))
})


test_that("validate_argument enforces single vs multiple character values", {
  x <- c("a", "b")
  expect_error(validate_argument(x, "character"), "x must be a single value")
  expect_invisible(
    validate_argument(x, "character", allow_multiple = TRUE)
  )

  x <- character(0)
  expect_error(validate_argument(x, "character"), "x must be a single value")
  expect_invisible(
    validate_argument(x, "character", allow_multiple = TRUE)
  )
})


test_that("validate_argument passes for valid logical values", {
  x <- TRUE
  expect_invisible(validate_argument(x, "logical"))

  x <- FALSE
  expect_invisible(validate_argument(x, "logical"))

  x <- c(TRUE, FALSE)
  expect_invisible(validate_argument(x, "logical", allow_multiple = TRUE))
})


test_that("validate_argument rejects non-logical values", {
  x <- "TRUE"
  expect_error(validate_argument(x, "logical"), "x must be a logical value")

  x <- 1
  expect_error(validate_argument(x, "logical"), "x must be a logical value")
})


test_that("validate_argument handles NULL and NA for logical", {
  x <- NULL
  expect_error(validate_argument(x, "logical"), "x must not be NULL")
  expect_invisible(validate_argument(x, "logical", allow_null = TRUE))

  x <- NA
  expect_error(validate_argument(x, "logical"), "x must not contain NA")
  expect_invisible(validate_argument(x, "logical", allow_na = TRUE))

  x <- c(TRUE, NA)
  expect_error(
    validate_argument(x, "logical", allow_multiple = TRUE),
    "x must not contain NA"
  )
  expect_invisible(
    validate_argument(x, "logical", allow_multiple = TRUE, allow_na = TRUE)
  )
})


test_that("validate_argument enforces single vs multiple logical values", {
  x <- c(TRUE, FALSE)
  expect_error(validate_argument(x, "logical"), "x must be a single value")

  x <- logical(0)
  expect_error(validate_argument(x, "logical"), "x must be a single value")
  expect_invisible(
    validate_argument(x, "logical", allow_multiple = TRUE)
  )
})


test_that("validate_argument passes for valid numeric values", {
  x <- 1
  expect_invisible(validate_argument(x, "numeric"))

  x <- 1.5
  expect_invisible(validate_argument(x, "numeric"))

  x <- 1L
  expect_invisible(validate_argument(x, "numeric"))

  x <- Inf
  expect_invisible(validate_argument(x, "numeric"))

  x <- -Inf
  expect_invisible(validate_argument(x, "numeric"))

  x <- c(1, 2, 3)
  expect_invisible(validate_argument(x, "numeric", allow_multiple = TRUE))
})


test_that("validate_argument rejects non-numeric values", {
  x <- "1"
  expect_error(validate_argument(x, "numeric"), "x must be a numeric value")

  x <- TRUE
  expect_error(validate_argument(x, "numeric"), "x must be a numeric value")
})


test_that("validate_argument handles NULL and NA for numeric", {
  x <- NULL
  expect_error(validate_argument(x, "numeric"), "x must not be NULL")
  expect_invisible(validate_argument(x, "numeric", allow_null = TRUE))

  x <- NA_real_
  expect_error(validate_argument(x, "numeric"), "x must not contain NA")
  expect_invisible(validate_argument(x, "numeric", allow_na = TRUE))

  x <- NA
  expect_error(validate_argument(x, "numeric"), "x must not contain NA")
  expect_invisible(validate_argument(x, "numeric", allow_na = TRUE))

  x <- NaN
  expect_error(validate_argument(x, "numeric"), "x must not contain NA")
  expect_invisible(validate_argument(x, "numeric", allow_na = TRUE))

  x <- c(1, NA_real_)
  expect_error(
    validate_argument(x, "numeric", allow_multiple = TRUE),
    "x must not contain NA"
  )
  expect_invisible(
    validate_argument(x, "numeric", allow_multiple = TRUE, allow_na = TRUE)
  )
})


test_that("validate_argument enforces single vs multiple numeric values", {
  x <- c(1, 2)
  expect_error(validate_argument(x, "numeric"), "x must be a single value")

  x <- numeric(0)
  expect_error(validate_argument(x, "numeric"), "x must be a single value")
  expect_invisible(
    validate_argument(x, "numeric", allow_multiple = TRUE)
  )
})


test_that("validate_argument passes for Date values", {
  x <- as.Date("2020-01-01")
  expect_invisible(validate_argument(x, "date"))

  x <- as.Date(c("2020-01-01", "2020-01-02"))
  expect_invisible(validate_argument(x, "date", allow_multiple = TRUE))
})


test_that("validate_argument rejects non-Date values", {
  x <- "2020-01-01"
  expect_error(validate_argument(x, "date"), "x must be a date value")

  x <- as.POSIXct("2020-01-01", tz = "UTC")
  expect_error(validate_argument(x, "date"), "x must be a date value")

  x <- 1
  expect_error(validate_argument(x, "date"), "x must be a date value")
})


test_that("validate_argument handles NULL and NA for date", {
  x <- NULL
  expect_error(validate_argument(x, "date"), "x must not be NULL")
  expect_invisible(validate_argument(x, "date", allow_null = TRUE))

  x <- as.Date(NA)
  expect_error(validate_argument(x, "date"), "x must not contain NA")
  expect_invisible(validate_argument(x, "date", allow_na = TRUE))

  x <- as.Date(c("2020-01-01", NA))
  expect_error(
    validate_argument(x, "date", allow_multiple = TRUE),
    "x must not contain NA"
  )
  expect_invisible(
    validate_argument(x, "date", allow_multiple = TRUE, allow_na = TRUE)
  )
})


test_that("validate_argument enforces single vs multiple date values", {
  x <- as.Date(c("2020-01-01", "2020-01-02"))
  expect_error(validate_argument(x, "date"), "x must be a single value")

  x <- as.Date(character(0))
  expect_error(validate_argument(x, "date"), "x must be a single value")
  expect_invisible(
    validate_argument(x, "date", allow_multiple = TRUE)
  )
})


test_that("validate_argument passes for function values", {
  x <- mean
  expect_invisible(
    suppressWarnings(validate_argument(x, "function"))
  )

  x <- function(z) z + 1
  expect_invisible(
    suppressWarnings(validate_argument(x, "function"))
  )
})


test_that("validate_argument rejects non-function values", {
  x <- "mean"
  expect_error(
    suppressWarnings(validate_argument(x, "function")),
    "x must be a function value"
  )

  x <- 1
  expect_error(
    suppressWarnings(validate_argument(x, "function")),
    "x must be a function value"
  )
})


test_that("validate_argument handles NULL for function", {
  x <- NULL
  expect_error(validate_argument(x, "function"), "x must not be NULL")
  expect_invisible(validate_argument(x, "function", allow_null = TRUE))
})


test_that("validate_argument does not apply NA check to functions", {
  # primitive / closure functions should not hit the NA branch
  x <- sum
  expect_invisible(suppressWarnings(validate_argument(x, "function")))
})


test_that("validate_argument enforces allowed values", {
  x <- "stop"
  expect_invisible(
    validate_argument(x, "character", values = c("stop", "identify", "resolve"))
  )

  expect_error(
    validate_argument(x, "character", values = c("identify", "resolve")),
    "x must be identify or resolve!"
  )

  expect_error(
    validate_argument(x, "character", values = "identify"),
    "x must be identify!"
  )

  x <- c("stop", "identify")
  expect_invisible(
    validate_argument(
      x, "character",
      allow_multiple = TRUE,
      values = c("stop", "identify", "resolve")
    )
  )
  expect_error(
    validate_argument(
      x, "character",
      allow_multiple = TRUE,
      values = c("stop", "resolve")
    ),
    "x must be stop or resolve!"
  )
})


test_that("validate_argument enforces allowed values for numeric", {
  x <- 1
  expect_invisible(validate_argument(x, "numeric", values = c(1, 2, 3)))

  expect_error(
    validate_argument(x, "numeric", values = c(2, 3)),
    "x must be 2 or 3!"
  )

  x <- c(1, 2)
  expect_invisible(
    validate_argument(x, "numeric", allow_multiple = TRUE, values = c(1, 2, 3))
  )
  expect_error(
    validate_argument(x, "numeric", allow_multiple = TRUE, values = c(1, 3)),
    "x must be 1 or 3!"
  )
})


test_that("validate_argument enforces allowed values for logical", {
  x <- TRUE
  expect_invisible(validate_argument(x, "logical", values = c(TRUE, FALSE)))
  expect_error(
    validate_argument(x, "logical", values = FALSE),
    "x must be FALSE!"
  )
})


test_that("validate_argument validates the type argument", {
  x <- "test"
  expect_error(
    validate_argument(x, "string"),
    "should be one of|sollte eines von"
  )
  expect_error(
    validate_argument(x, "invalid"),
    "should be one of|sollte eines von"
  )
})


test_that("validate_argument uses the parameter name in error messages", {
  analyte <- 1
  expect_error(
    validate_argument(analyte, "character"),
    "analyte must be a character value"
  )

  duplicates <- NULL
  expect_error(
    validate_argument(duplicates, "character"),
    "duplicates must not be NULL"
  )

  time <- ""
  expect_error(
    validate_argument(time, "character"),
    "time must be a non-empty string"
  )

  group <- c("A", "B")
  expect_error(
    validate_argument(group, "character"),
    "group must be a single value"
  )
})


test_that("validate_argument allow_empty does not apply to non-character types", {
  x <- TRUE
  expect_invisible(validate_argument(x, "logical", allow_empty = FALSE))

  x <- 1
  expect_invisible(validate_argument(x, "numeric", allow_empty = FALSE))
})


test_that("validate_argument allow_null short-circuits other checks", {
  x <- NULL
  expect_invisible(
    validate_argument(
      x, "character",
      allow_null = TRUE,
      values = c("a", "b")
    )
  )
  expect_invisible(
    validate_argument(x, "numeric", allow_null = TRUE, allow_multiple = TRUE)
  )
})


test_that("validate_argument default type rejects non-character input", {
  x <- 1
  expect_error(validate_argument(x), "x must be a character value")
})
