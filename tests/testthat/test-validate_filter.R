# --- Input validation ---

test_that("validate_filter rejects non-character input", {
  expect_error(validate_filter(42), "filter_string must be a single character string")
})

test_that("validate_filter rejects logical input", {
  expect_error(validate_filter(TRUE), "filter_string must be a single character string")
})

test_that("validate_filter rejects NULL input", {
  expect_error(validate_filter(NULL), "filter_string must be a single character string")
})

test_that("validate_filter rejects character vector of length > 1", {
  expect_error(
    validate_filter(c("A == 1", "B == 2")),
    "filter_string must be a single character string"
  )
})

test_that("validate_filter rejects empty string", {
  expect_error(validate_filter(""), "filter_string must not be empty")
})

test_that("validate_filter rejects whitespace-only string", {
  expect_error(validate_filter("   "), "filter_string must not be empty")
})


# --- Parse errors ---

test_that("validate_filter rejects unparseable expression", {
  expect_error(validate_filter("A >"), "Failed to parse filter expression")
})

test_that("validate_filter rejects unclosed parenthesis", {
  expect_error(validate_filter("(A == 1"), "Failed to parse filter expression")
})

test_that("validate_filter rejects trailing operator", {
  expect_error(validate_filter("A == 1 &"), "Failed to parse filter expression")
})


# --- Valid expressions without data ---

test_that("validate_filter accepts simple comparison", {
  result <- validate_filter("TAFD <= 0")
  expect_equal(result, rlang::parse_expr("TAFD <= 0"))
})

test_that("validate_filter accepts equality with string literal", {
  result <- validate_filter("VSBLFL == 'Y'")
  expect_equal(result, rlang::parse_expr("VSBLFL == 'Y'"))
})

test_that("validate_filter accepts compound AND expression", {
  result <- validate_filter("TAFD <= 0 & EVID == 0")
  expect_equal(result, rlang::parse_expr("TAFD <= 0 & EVID == 0"))
})

test_that("validate_filter accepts compound OR expression", {
  result <- validate_filter("TAFD <= 0 | is.na(TAFD)")
  expect_equal(result, rlang::parse_expr("TAFD <= 0 | is.na(TAFD)"))
})

test_that("validate_filter accepts negated expression", {
  result <- validate_filter("!(A > 0)")
  expect_equal(result, rlang::parse_expr("!(A > 0)"))
})

test_that("validate_filter accepts bare TRUE", {
  result <- validate_filter("TRUE")
  expect_equal(result, rlang::parse_expr("TRUE"))
})

test_that("validate_filter accepts bare FALSE", {
  result <- validate_filter("FALSE")
  expect_equal(result, rlang::parse_expr("FALSE"))
})

test_that("validate_filter accepts negative numeric literal", {
  result <- validate_filter("TIME <= -1")
  expect_equal(result, rlang::parse_expr("TIME <= -1"))
})

test_that("validate_filter accepts inequality with string", {
  result <- validate_filter("LBSPEC != 'URINE'")
  expect_equal(result, rlang::parse_expr("LBSPEC != 'URINE'"))
})

test_that("validate_filter accepts deeply nested expression", {
  filter <- "((A <= 0 & B == 0) | (is.na(C) & D > -1)) & E != 'X'"
  result <- validate_filter(filter)
  expect_equal(result, rlang::parse_expr(filter))
})


# --- Return type ---

test_that("validate_filter returns a language object", {
  result <- validate_filter("A == 1")
  expect_true(is.language(result))
})

test_that("validate_filter returns invisibly", {
  expect_invisible(validate_filter("A == 1"))
})


# --- Column validation with data ---

test_that("validate_filter passes when all columns exist in data", {
  df <- data.frame(TAFD = 1, EVID = 0, TIME = 2)
  result <- validate_filter("TAFD <= 0 & EVID == 0", data = df)
  expect_equal(result, rlang::parse_expr("TAFD <= 0 & EVID == 0"))
})

test_that("validate_filter rejects unknown column when data is provided", {
  df <- data.frame(TAFD = 1, TIME = 2)
  expect_error(
    validate_filter("NONEXISTENT <= 0", data = df),
    "Column 'NONEXISTENT' not found in data"
  )
})

test_that("validate_filter rejects unknown column in compound expression", {
  df <- data.frame(A = 1)
  expect_error(
    validate_filter("A == 1 & BAD == 2", data = df),
    "Column 'BAD' not found in data"
  )
})

test_that("validate_filter validates is.na column against data", {
  df <- data.frame(A = 1)
  expect_error(
    validate_filter("is.na(MISSING)", data = df),
    "Column 'MISSING' not found in data"
  )
})

test_that("validate_filter allows TRUE without it being in data columns", {
  df <- data.frame(A = 1)
  expect_no_error(validate_filter("TRUE", data = df))
})

test_that("validate_filter skips column check when data is NULL", {
  expect_no_error(validate_filter("ANY_COL <= 0", data = NULL))
})


# --- AST rejection is surfaced through validate_filter ---

test_that("validate_filter rejects arbitrary function calls", {
  expect_error(validate_filter("system('ls')"), "Disallowed construct")
})

test_that("validate_filter accepts lubridate::as_datetime in filter", {
  expr <- validate_filter("VSDTC >= lubridate::as_datetime('2023-06-01')")
  expect_true(is.call(expr))
})

test_that("validate_filter rejects non-allowlisted namespace calls", {
  expect_error(
    validate_filter("base::system('ls')"),
    "Disallowed construct.*base::system"
  )
})

test_that("validate_filter rejects assignment", {
  expect_error(validate_filter("x <- 5"), "Disallowed construct")
})

test_that("validate_filter rejects arithmetic", {
  expect_error(validate_filter("A + B"), "Disallowed construct")
})

test_that("validate_filter accepts %in% with c() of literals", {
  result <- validate_filter("A %in% c(1, 2)")
  expect_equal(result, rlang::parse_expr("A %in% c(1, 2)"))
})

test_that("validate_filter rejects %in% with non-literal in c()", {
  expect_error(validate_filter("A %in% c(mean(X))"), "c\\(\\) arguments must be literals")
})


# --- Real-world baseline_filter expressions ---

test_that("validate_filter accepts all real-world baseline filters", {
  filters <- c(
    "TAFD <= 0",
    "TAFD < 0",
    "TAFD == 1",
    "TIME <= 0",
    "TIME < 0",
    "TIME == 1",
    "TIME == -1",
    "VSBLFL == 'Y'",
    "LBBLFL == 'Y'",
    "LBLOBXFL == 'Y'",
    "FOOD == 'FED'",
    "TIME <= 0 & EVID == 0",
    "TAFD <= 0 & EVID == 0",
    "TIME <= 0 & FOOD == 'FED'",
    "TAFD <= 0 & FOOD == 'FED'",
    "TIME <= 0 | is.na(TIME)",
    "TAFD <= 0 | is.na(TAFD)",
    "TRUE",
    "FALSE"
  )
  for (f in filters) {
    expect_no_error(validate_filter(f))
  }
})
