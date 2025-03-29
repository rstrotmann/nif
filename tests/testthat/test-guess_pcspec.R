test_that("guess_pcspec works correctly", {
  # Test normal priority ordering
  df1 <- data.frame(PCSPEC = c("PLASMA", "SERUM", "BLOOD"))
  expect_equal(guess_pcspec(df1, silent = TRUE), "PLASMA")

  df2 <- data.frame(PCSPEC = c("SERUM", "BLOOD"))
  expect_equal(guess_pcspec(df2, silent = TRUE), "SERUM")

  df3 <- data.frame(PCSPEC = "BLOOD")
  expect_equal(guess_pcspec(df3, silent = TRUE), "BLOOD")

  # Test case insensitivity
  df4 <- data.frame(PCSPEC = c("plasma", "SERUM", "Blood"))
  expect_equal(guess_pcspec(df4, silent = TRUE), "plasma")

  # Test with duplicate values
  df5 <- data.frame(PCSPEC = c("SERUM", "SERUM", "BLOOD", "BLOOD"))
  expect_equal(guess_pcspec(df5, silent = TRUE), "SERUM")

  # Test with non-standard specimen types
  df6 <- data.frame(PCSPEC = c("TISSUE", "OTHER"))
  expect_warning(guess_pcspec(df6, silent = TRUE), "No standard specimen types found")
  expect_equal(suppressWarnings(guess_pcspec(df6, silent = TRUE)), "TISSUE")

  # Test mixed standard and non-standard types
  df7 <- data.frame(PCSPEC = c("TISSUE", "BLOOD", "OTHER"))
  expect_equal(guess_pcspec(df7, silent = TRUE), "BLOOD")
})


test_that("guess_pcspec handles errors correctly", {
  # Test invalid input
  expect_error(guess_pcspec(NULL, silent = TRUE),
               "Input must be a data frame")
  expect_error(guess_pcspec(data.frame(), silent = TRUE),
               "Input must be a data frame with PCSPEC column")
  expect_error(guess_pcspec(data.frame(OTHER = "BLOOD", silent = TRUE)),
               "Input must be a data frame with PCSPEC column")

  # Test empty PCSPEC column
  df_empty <- data.frame(PCSPEC = character(0))
  expect_error(guess_pcspec(df_empty, silent = TRUE), "No PCSPEC values found")

  # Test NA values
  df_na <- data.frame(PCSPEC = NA_character_)
  expect_warning(guess_pcspec(df_na, silent = TRUE), "No standard specimen types found")
})


test_that("guess_pcspec maintains data frame attributes", {
  # Test that the function doesn't modify the input data frame
  df_original <- data.frame(
    PCSPEC = c("PLASMA", "SERUM"),
    OtherCol = c("A", "B"))

  df_copy <- df_original
  result <- guess_pcspec(df_original, silent = TRUE)

  expect_equal(df_original, df_copy)
  expect_equal(result, "PLASMA")
})
