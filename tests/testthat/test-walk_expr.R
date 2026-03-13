# --- Comparison operators ---

test_that("walk_expr accepts less-than-or-equal comparison", {
  expr <- rlang::parse_expr("TAFD <= 0")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts less-than comparison", {
  expr <- rlang::parse_expr("TIME < 0")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts greater-than-or-equal comparison", {
  expr <- rlang::parse_expr("TAFD >= 1")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts greater-than comparison", {
  expr <- rlang::parse_expr("VALUE > 100")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts equality comparison", {
  expr <- rlang::parse_expr("EVID == 0")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts inequality comparison", {
  expr <- rlang::parse_expr("LBSPEC != 'URINE'")
  expect_invisible(walk_expr(expr))
})


# --- Logical operators ---

test_that("walk_expr accepts single-ampersand AND", {
  expr <- rlang::parse_expr("TAFD <= 0 & EVID == 0")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts double-ampersand AND", {
  expr <- rlang::parse_expr("TAFD <= 0 && EVID == 0")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts single-pipe OR", {
  expr <- rlang::parse_expr("TAFD <= 0 | is.na(TAFD)")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts double-pipe OR", {
  expr <- rlang::parse_expr("A == 1 || B == 2")
  expect_invisible(walk_expr(expr))
})


# --- Negation ---

test_that("walk_expr accepts negation of a comparison", {
  expr <- rlang::parse_expr("!(A > 0)")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts negation of is.na", {
  expr <- rlang::parse_expr("!is.na(COL)")
  expect_invisible(walk_expr(expr))
})


# --- Parentheses ---

test_that("walk_expr accepts parenthesized grouping", {
  expr <- rlang::parse_expr("(A <= 0 | B == 1) & C > 2")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts nested parentheses", {
  expr <- rlang::parse_expr("((A <= 0))")
  expect_invisible(walk_expr(expr))
})


# --- Unary minus ---

test_that("walk_expr accepts negative numeric literal", {
  expr <- rlang::parse_expr("TAFD <= -1")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts negative float literal", {
  expr <- rlang::parse_expr("DV > -0.5")
  expect_invisible(walk_expr(expr))
})


# --- is.na ---

test_that("walk_expr accepts is.na with column name", {
  expr <- rlang::parse_expr("is.na(TAFD)")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr rejects is.na with non-symbol argument", {
  expr <- rlang::parse_expr("is.na(1 + 2)")
  expect_error(walk_expr(expr), "is.na\\(\\) must be called with a single column name")
})

test_that("walk_expr rejects is.na with string literal argument", {
  expr <- rlang::parse_expr("is.na('text')")
  expect_error(walk_expr(expr), "is.na\\(\\) must be called with a single column name")
})


# --- Literals ---

test_that("walk_expr accepts bare TRUE", {
  expr <- rlang::parse_expr("TRUE")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts bare FALSE", {
  expr <- rlang::parse_expr("FALSE")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts string comparison", {
  expr <- rlang::parse_expr("COL == 'Y'")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts numeric literal on right-hand side", {
  expr <- rlang::parse_expr("TIME == 1.5")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts integer literal", {
  expr <- rlang::parse_expr("CMT == 2L")
  expect_invisible(walk_expr(expr))
})


# --- Column name validation ---

test_that("walk_expr passes with valid column names", {
  col_names <- c("TAFD", "TIME", "EVID")
  expr <- rlang::parse_expr("TAFD <= 0 & EVID == 0")
  expect_invisible(walk_expr(expr, col_names))
})

test_that("walk_expr rejects unknown column name", {
  col_names <- c("TAFD", "TIME")
  expr <- rlang::parse_expr("NONEXISTENT <= 0")
  expect_error(walk_expr(expr, col_names), "Column 'NONEXISTENT' not found in data")
})

test_that("walk_expr rejects unknown column in compound expression", {
  col_names <- c("TAFD")
  expr <- rlang::parse_expr("TAFD <= 0 & BAD == 1")
  expect_error(walk_expr(expr, col_names), "Column 'BAD' not found in data")
})

test_that("walk_expr allows TRUE/FALSE even when not in col_names", {
  col_names <- c("A")
  expr <- rlang::parse_expr("TRUE")
  expect_invisible(walk_expr(expr, col_names))
})

test_that("walk_expr allows T/F shorthand even when not in col_names", {
  col_names <- c("A")
  expr <- rlang::parse_expr("T")
  expect_invisible(walk_expr(expr, col_names))
})

test_that("walk_expr validates is.na column against col_names", {
  col_names <- c("TAFD")
  expr <- rlang::parse_expr("is.na(MISSING)")
  expect_error(walk_expr(expr, col_names), "Column 'MISSING' not found in data")
})

test_that("walk_expr skips column validation when col_names is NULL", {
  expr <- rlang::parse_expr("ANY_COLUMN <= 0")
  expect_invisible(walk_expr(expr, col_names = NULL))
})

test_that("walk_expr error message lists available columns", {
  col_names <- c("A", "B", "C")
  expr <- rlang::parse_expr("X == 1")
  expect_error(walk_expr(expr, col_names), "Available columns: A, B, C")
})


# --- Rejected: arbitrary function calls ---

test_that("walk_expr rejects system()", {
  expr <- rlang::parse_expr("system('ls')")
  expect_error(walk_expr(expr), "Disallowed construct.*system")
})

test_that("walk_expr rejects file.remove()", {
  expr <- rlang::parse_expr("file.remove('x')")
  expect_error(walk_expr(expr), "Disallowed construct.*file.remove")
})

test_that("walk_expr rejects cat()", {
  expr <- rlang::parse_expr("cat('hello')")
  expect_error(walk_expr(expr), "Disallowed construct.*cat")
})

test_that("walk_expr rejects Sys.getenv()", {
  expr <- rlang::parse_expr("Sys.getenv('HOME')")
  expect_error(walk_expr(expr), "Disallowed construct.*Sys.getenv")
})

test_that("walk_expr rejects eval()", {
  expr <- rlang::parse_expr("eval(quote(1))")
  expect_error(walk_expr(expr), "Disallowed construct.*eval")
})

test_that("walk_expr rejects print()", {
  expr <- rlang::parse_expr("print('hi')")
  expect_error(walk_expr(expr), "Disallowed construct.*print")
})

test_that("walk_expr rejects mean()", {
  expr <- rlang::parse_expr("mean(X)")
  expect_error(walk_expr(expr), "Disallowed construct.*mean")
})

test_that("walk_expr rejects c()", {
  expr <- rlang::parse_expr("c(1, 2)")
  expect_error(walk_expr(expr), "Disallowed construct.*\\bc\\b")
})


# --- Allowed namespace calls ---

test_that("walk_expr accepts lubridate::as_datetime with string literal", {
  expr <- rlang::parse_expr("lubridate::as_datetime('2023-01-01')")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts lubridate::as_datetime in comparison", {
  expr <- rlang::parse_expr("VSDTC == lubridate::as_datetime('2023-01-01')")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr rejects lubridate::as_datetime with non-literal argument", {
  expr <- rlang::parse_expr("lubridate::as_datetime(COL)")
  expect_error(walk_expr(expr), "arguments must be literals")
})


# --- Rejected: namespace calls ---

test_that("walk_expr rejects non-allowlisted namespace calls", {
  expr <- rlang::parse_expr("base::system('ls')")
  expect_error(walk_expr(expr), "Disallowed construct.*base::system")
})

test_that("walk_expr rejects triple-colon namespace calls", {
  expr <- rlang::parse_expr("base:::system('ls')")
  expect_error(walk_expr(expr), "Disallowed construct")
})


# --- Rejected: assignment ---

test_that("walk_expr rejects left-arrow assignment", {
  expr <- rlang::parse_expr("x <- 5")
  expect_error(walk_expr(expr), "Disallowed construct.*<-")
})

test_that("walk_expr rejects equals assignment", {
  expr <- rlang::parse_expr("x = 5")
  expect_error(walk_expr(expr), "Disallowed construct.*=")
})


# --- Rejected: arithmetic ---

test_that("walk_expr rejects addition", {
  expr <- rlang::parse_expr("1 + 2")
  expect_error(walk_expr(expr), "Disallowed construct.*\\+")
})

test_that("walk_expr rejects multiplication", {
  expr <- rlang::parse_expr("A * 2")
  expect_error(walk_expr(expr), "Disallowed construct.*\\*")
})

test_that("walk_expr rejects division", {
  expr <- rlang::parse_expr("A / B")
  expect_error(walk_expr(expr), "Disallowed construct.*\\/")
})

test_that("walk_expr rejects binary minus (subtraction)", {
  expr <- rlang::parse_expr("A - B")
  expect_error(walk_expr(expr), "Disallowed construct.*-")
})


# --- %in% operator ---

test_that("walk_expr accepts %in% with numeric literals", {
  expr <- rlang::parse_expr("COL %in% c(1, 2, 3)")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts %in% with character literals", {
  expr <- rlang::parse_expr("FOOD %in% c('FED', 'FASTED')")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts %in% with single element", {
  expr <- rlang::parse_expr("COL %in% c(1)")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr validates %in% LHS column against col_names", {
  col_names <- c("A", "B")
  expr <- rlang::parse_expr("MISSING %in% c(1, 2)")
  expect_error(walk_expr(expr, col_names), "Column 'MISSING' not found in data")
})

test_that("walk_expr accepts %in% when LHS column is in col_names", {
  col_names <- c("STATUS", "TYPE")
  expr <- rlang::parse_expr("STATUS %in% c('active', 'pending')")
  expect_invisible(walk_expr(expr, col_names))
})

test_that("walk_expr rejects %in% with non-symbol LHS", {
  expr <- rlang::parse_expr("(A + 1) %in% c(1, 2)")
  expect_error(walk_expr(expr), "left-hand side must be a column name")
})

test_that("walk_expr rejects %in% without c() on RHS", {
  expr <- rlang::parse_expr("COL %in% 1")
  expect_error(walk_expr(expr), "right-hand side must be a c\\(\\) call")
})

test_that("walk_expr rejects %in% with function call in c()", {
  expr <- rlang::parse_expr("COL %in% c(mean(X), 2)")
  expect_error(walk_expr(expr), "c\\(\\) arguments must be literals")
})

test_that("walk_expr rejects %in% with symbol in c()", {
  expr <- rlang::parse_expr("COL %in% c(A, B)")
  expect_error(walk_expr(expr), "c\\(\\) arguments must be literals")
})

test_that("walk_expr accepts %in% combined with other operators", {
  expr <- rlang::parse_expr("A %in% c(1, 2) & B == 'Y'")
  expect_invisible(walk_expr(expr))
})


# --- Rejected: pipe ---

test_that("walk_expr rejects pipe operator", {
  expr <- rlang::parse_expr("x |> mean()")
  expect_error(walk_expr(expr), "Disallowed construct")
})


# --- Complex valid expressions ---

test_that("walk_expr accepts deeply nested valid expression", {
  expr <- rlang::parse_expr(
    "((TAFD <= 0 & EVID == 0) | (is.na(TAFD) & TIME < -1)) & COL != 'X'"
  )
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts expression matching real-world usage", {
  expr <- rlang::parse_expr("VSBLFL == 'Y'")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts expression with negative literal and OR", {
  expr <- rlang::parse_expr("TIME <= -2 | is.na(TIME)")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts chained AND comparisons", {
  expr <- rlang::parse_expr("A == 1 & B == 2 & C == 3")
  expect_invisible(walk_expr(expr))
})

test_that("walk_expr accepts negated compound expression", {
  expr <- rlang::parse_expr("!(A > 0 & B < 10)")
  expect_invisible(walk_expr(expr))
})
