test_that("validate_param works correctly for string parameters", {
  # Valid string parameters
  expect_silent(validate_param("string", "hello", "test_param"))
  expect_silent(validate_param("string", "test", "test_param", allow_multiple = TRUE))

  # Invalid string parameters
  expect_error(validate_param("string", 123, "test_param"), "must be a string value")
  expect_error(validate_param("string", TRUE, "test_param"), "must be a string value")
  expect_error(validate_param("string", c("a", "b"), "test_param"), "must be a single value")

  # NULL handling
  expect_error(validate_param("string", NULL, "test_param"), "must not be NULL")
  expect_silent(validate_param("string", NULL, "test_param", allow_null = TRUE))

  # NA handling
  expect_error(validate_param("string", NA_character_, "test_param"), "must not contain NA")
  expect_error(validate_param("string", c("a", NA_character_), "test_param"), "must not contain NA")

  # Empty string handling
  expect_error(validate_param("string", "", "test_param"), "must be a non-empty string")
  expect_silent(validate_param("string", "", "test_param", allow_empty = TRUE))
  expect_error(validate_param("string", c("", "a"), "test_param", allow_multiple = TRUE), "must be a non-empty string")
  expect_silent(validate_param("string", c("", "a"), "test_param", allow_multiple = TRUE, allow_empty = TRUE))
  expect_error(validate_param("string", c("a", ""), "test_param", allow_multiple = TRUE), "must be a non-empty string")
  expect_silent(validate_param("string", c("a", ""), "test_param", allow_multiple = TRUE, allow_empty = TRUE))
})

test_that("validate_param works correctly for logical parameters", {
  # Valid logical parameters
  expect_silent(validate_param("logical", TRUE, "test_param"))
  expect_silent(validate_param("logical", FALSE, "test_param"))
  expect_silent(validate_param("logical", c(TRUE, FALSE), "test_param", allow_multiple = TRUE))

  # Invalid logical parameters
  expect_error(validate_param("logical", "TRUE", "test_param"), "must be a logical value")
  expect_error(validate_param("logical", 1, "test_param"), "must be a logical value")
  expect_error(validate_param("logical", c(TRUE, FALSE), "test_param"), "must be a single value")

  # NULL handling
  expect_error(validate_param("logical", NULL, "test_param"), "must not be NULL")
  expect_silent(validate_param("logical", NULL, "test_param", allow_null = TRUE))

  # NA handling
  expect_error(validate_param("logical", NA, "test_param"), "must not contain NA")
  expect_error(validate_param("logical", c(TRUE, NA), "test_param"), "must not contain NA")

  # Empty string check should not apply to logical
  expect_silent(validate_param("logical", TRUE, "test_param", allow_empty = FALSE))
})

test_that("validate_param works correctly for numeric parameters", {
  # Valid numeric parameters
  expect_silent(validate_param("numeric", 1, "test_param"))
  expect_silent(validate_param("numeric", 1.5, "test_param"))
  expect_silent(validate_param("numeric", c(1, 2, 3), "test_param", allow_multiple = TRUE))

  # Invalid numeric parameters
  expect_error(validate_param("numeric", "1", "test_param"), "must be a numeric value")
  expect_error(validate_param("numeric", TRUE, "test_param"), "must be a numeric value")
  expect_error(validate_param("numeric", c(1, 2), "test_param"), "must be a single value")

  # NULL handling
  expect_error(validate_param("numeric", NULL, "test_param"), "must not be NULL")
  expect_silent(validate_param("numeric", NULL, "test_param", allow_null = TRUE))

  # NA handling
  expect_error(validate_param("numeric", NA_real_, "test_param"), "must not contain NA")
  expect_error(validate_param("numeric", c(1, NA_real_), "test_param"), "must not contain NA")

  # Empty string check should not apply to numeric
  expect_silent(validate_param("numeric", 1, "test_param", allow_empty = FALSE))
})

test_that("validate_param validates type parameter", {
  # Valid type
  expect_silent(validate_param("string", "test", "test_param"))
  expect_silent(validate_param("logical", TRUE, "test_param"))
  expect_silent(validate_param("numeric", 1, "test_param"))

  # Invalid type
  expect_error(validate_param("invalid_type", "test", "test_param"), "should be one of")
})

test_that("validate_char_param works correctly", {
  # Valid character parameters
  expect_silent(validate_char_param("hello", "test_param"))
  expect_silent(validate_char_param("test", "test_param", allow_multiple = TRUE))

  # Invalid character parameters
  expect_error(validate_char_param(123, "test_param"), "must be a string value")
  expect_error(validate_char_param(TRUE, "test_param"), "must be a string value")
  expect_error(validate_char_param(c("a", "b"), "test_param"), "must be a single value")

  # NULL handling
  expect_error(validate_char_param(NULL, "test_param"), "must not be NULL")
  expect_silent(validate_char_param(NULL, "test_param", allow_null = TRUE))

  # NA handling
  expect_error(validate_char_param(NA_character_, "test_param"), "must not contain NA")

  # Empty string handling
  expect_error(validate_char_param("", "test_param"), "must be a non-empty string")
  expect_silent(validate_char_param("", "test_param", allow_empty = TRUE))
})

test_that("validate_logical_param works correctly", {
  # Valid logical parameters
  expect_silent(validate_logical_param(TRUE, "test_param"))
  expect_silent(validate_logical_param(FALSE, "test_param"))
  expect_silent(validate_logical_param(c(TRUE, FALSE), "test_param", allow_multiple = TRUE))

  # Invalid logical parameters
  expect_error(validate_logical_param("TRUE", "test_param"), "must be a logical value")
  expect_error(validate_logical_param(1, "test_param"), "must be a logical value")
  expect_error(validate_logical_param(c(TRUE, FALSE), "test_param"), "must be a single value")

  # NULL handling
  expect_error(validate_logical_param(NULL, "test_param"), "must not be NULL")
  expect_silent(validate_logical_param(NULL, "test_param", allow_null = TRUE))

  # NA handling
  expect_error(validate_logical_param(NA, "test_param"), "must not contain NA")
})

test_that("validate_numeric_param works correctly", {
  # Valid numeric parameters
  expect_silent(validate_numeric_param(1, "test_param"))
  expect_silent(validate_numeric_param(1.5, "test_param"))
  expect_silent(validate_numeric_param(c(1, 2, 3), "test_param", allow_multiple = TRUE))

  # Invalid numeric parameters
  expect_error(validate_numeric_param("1", "test_param"), "must be a numeric value")
  expect_error(validate_numeric_param(TRUE, "test_param"), "must be a numeric value")
  expect_error(validate_numeric_param(c(1, 2), "test_param"), "must be a single value")

  # NULL handling
  expect_error(validate_numeric_param(NULL, "test_param"), "must not be NULL")
  expect_silent(validate_numeric_param(NULL, "test_param", allow_null = TRUE))

  # NA handling
  expect_error(validate_numeric_param(NA_real_, "test_param"), "must not contain NA")
})

test_that("validate_param handles edge cases correctly", {
  # Zero-length vectors
  expect_error(validate_param("string", character(0), "test_param"), "must be a single value")
  expect_error(validate_param("logical", logical(0), "test_param"), "must be a single value")
  expect_error(validate_param("numeric", numeric(0), "test_param"), "must be a single value")

  # Zero-length vectors with allow_multiple
  expect_silent(validate_param("string", character(0), "test_param", allow_multiple = TRUE))
  expect_silent(validate_param("logical", logical(0), "test_param", allow_multiple = TRUE))
  expect_silent(validate_param("numeric", numeric(0), "test_param", allow_multiple = TRUE))

  # Mixed NA and valid values
  expect_error(validate_param("string", c("a", NA_character_), "test_param", allow_multiple = TRUE), "must not contain NA")
  expect_error(validate_param("logical", c(TRUE, NA), "test_param", allow_multiple = TRUE), "must not contain NA")
  expect_error(validate_param("numeric", c(1, NA_real_), "test_param", allow_multiple = TRUE), "must not contain NA")

  # All empty strings with allow_multiple
  expect_error(validate_param("string", c("", ""), "test_param", allow_multiple = TRUE), "must be a non-empty string")
  expect_silent(validate_param("string", c("", ""), "test_param", allow_multiple = TRUE, allow_empty = TRUE))
})

test_that("validate_param provides informative error messages", {
  # Test error message content
  expect_error(validate_param("string", 123, "my_param"), "my_param must be a string value")
  expect_error(validate_param("logical", "TRUE", "my_param"), "my_param must be a logical value")
  expect_error(validate_param("numeric", "1", "my_param"), "my_param must be a numeric value")
  expect_error(validate_param("string", NULL, "my_param"), "my_param must not be NULL")
  expect_error(validate_param("string", NA_character_, "my_param"), "my_param must not contain NA")
  expect_error(validate_param("string", "", "my_param"), "my_param must be a non-empty string")
  expect_error(validate_param("string", c("a", "b"), "my_param"), "my_param must be a single value")
})
