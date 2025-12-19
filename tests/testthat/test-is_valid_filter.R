test_that("is_valid_filter works with basic filters", {
  # Test data frame with numeric and character columns
  df <- tibble::tribble(
    ~id, ~value, ~category,
    1,   10,     "A",
    2,   20,     "B",
    3,   30,     "A",
    4,   40,     "C"
  )

  # Valid filters
  expect_true(is_valid_filter(df, "value > 20"))
  expect_true(is_valid_filter(df, "category == 'A'"))
  expect_true(is_valid_filter(df, "value > 20 & category == 'A'"))
  expect_true(is_valid_filter(df, "value %in% c(10, 20)"))
  expect_true(is_valid_filter(df, "category %in% c('A', 'B')"))

  # Invalid filters
  expect_false(is_valid_filter(df, "value >")) # Incomplete expression
  expect_false(is_valid_filter(df, "nonexistent_column > 20")) # Non-existent column
  expect_false(is_valid_filter(df, "category == A")) # Missing quotes
})


test_that("is_valid_filter works with NA values", {
  # Test data frame with NA values
  df <- tibble::tribble(
    ~id, ~value, ~category,
    1,   NA,     "A",
    2,   20,     NA,
    3,   30,     "A",
    4,   NA,     "C"
  )

  # Valid filters with NA handling
  expect_true(is_valid_filter(df, "is.na(value)"))
  expect_true(is_valid_filter(df, "!is.na(category)"))
  expect_true(is_valid_filter(df, "is.na(value) | value > 20"))
  expect_true(is_valid_filter(df, "is.na(category) & value > 20"))

  # Invalid NA filters
  expect_false(is_valid_filter(df, "is.na()")) # Missing column
})


test_that("is_valid_filter works with date columns", {
  # Test data frame with date column
  df <- tibble::tribble(
    ~id, ~date, ~value,
    1, "2023-01-01", 10,
    2, "2023-02-01", 20,
    3, "2023-03-01", 30,
    4, "2023-04-01", 40
  )

  # Convert date column to Date type
  df$date <- as.Date(df$date)

  # Valid date filters
  expect_true(is_valid_filter(df, "date > '2023-02-01'"))
  expect_true(is_valid_filter(df, "date >= '2023-01-01' & date <= '2023-03-01'"))
  expect_true(is_valid_filter(df, "date %in% c('2023-01-01', '2023-02-01')"))

  # Invalid date filters
  expect_false(is_valid_filter(df, "date > 'invalid-date'"))
})


test_that("is_valid_filter works with complex expressions", {
  # Test data frame with multiple columns
  df <- tibble::tribble(
    ~id, ~value, ~category, ~status,
    1,   10,     "A",       "active",
    2,   20,     "B",       "inactive",
    3,   30,     "A",       "active",
    4,   40,     "C",       "pending"
  )

  # Valid complex filters
  expect_true(is_valid_filter(df, "value > 20 & (category == 'A' | status == 'active')"))
  expect_true(is_valid_filter(df, "value %in% c(10, 20) & category != 'C'"))
  expect_true(is_valid_filter(df, "status == 'active' & value > 20 | category == 'B'"))

  # Invalid complex filters
  expect_false(is_valid_filter(df, "value > 20 & (category == 'A'")) # Unclosed parenthesis
  expect_false(is_valid_filter(df, "value > 20 & category == 'A' |")) # Incomplete expression
  expect_false(is_valid_filter(df, "value > 20 & nonexistent == 'A'")) # Invalid column
})


test_that("is_valid_filter handles empty data frames", {
  # Empty data frame
  df <- tibble::tribble(
    ~id, ~value, ~category
  )

  # Valid filters on empty data frame
  expect_true(is_valid_filter(df, "value > 20"))
  expect_true(is_valid_filter(df, "category == 'A'"))
  expect_true(is_valid_filter(df, "value > 20 & category == 'A'"))

  # Invalid filters still return FALSE
  expect_false(is_valid_filter(df, "nonexistent > 20"))
  expect_false(is_valid_filter(df, "value >"))
})
