test_that("df_to_string basic functionality works", {
  # Create a simple test data frame
  df <- data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  # Test basic output with default parameters
  result <- df_to_string(df)
  expect_type(result, "character")
  expect_true(grepl("a   b", result))  # Check header exists
  expect_true(grepl("1   x", result))  # Check data exists

  # Test without header
  result_no_header <- df_to_string(df, header = FALSE)
  expect_false(grepl("a   b", result_no_header))
  expect_true(grepl("1   x", result_no_header))

  # Test with indentation
  result_indent <- df_to_string(df, indent = 2)
  expect_true(all(grepl("^  ", strsplit(result_indent, "\n")[[1]])))
})


test_that("df_to_string handles empty data frames", {
  empty_df <- data.frame()

  # Test empty df with show_none = TRUE
  result_none <- df_to_string(empty_df, show_none = TRUE)
  expect_equal(result_none, "none\n")

  # Test empty df with show_none = FALSE
  result_not_none <- df_to_string(empty_df, show_none = FALSE)
  expect_equal(result_not_none, "")
})


test_that("df_to_string respects n parameter", {
  df <- data.frame(
    a = 1:5,
    b = letters[1:5],
    stringsAsFactors = FALSE
  )

  # Test limiting rows
  result_limited <- df_to_string(df, n = 2)
  expect_equal(length(strsplit(result_limited, "\n")[[1]]), 3)  # header + 2 rows
})


test_that("df_to_string handles color formatting", {
  df <- data.frame(
    a = c(1, 2),
    b = c("x", "y"),
    stringsAsFactors = FALSE
  )

  # Test with color
  result_color <- df_to_string(df, color = TRUE)
  expect_true(grepl("\u001b\\[38;5;248m", result_color))  # Check for color code
  expect_true(grepl("\u001b\\[0m", result_color))  # Check for color reset
})


test_that("df_to_string handles NA values", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c("x", "y", NA),
    stringsAsFactors = FALSE
  )

  result <- df_to_string(df)
  expect_true(grepl("NA", result))
})


test_that("df_to_string maintains column alignment", {
  df <- data.frame(
    short = c(1, 2),
    very_long_name = c("a", "b"),
    stringsAsFactors = FALSE
  )

  result <- df_to_string(df)
  lines <- strsplit(result, "\n")[[1]]

  # Check that all lines have the same number of characters
  line_lengths <- sapply(lines, nchar)
  expect_equal(length(unique(line_lengths)), 1)
})

