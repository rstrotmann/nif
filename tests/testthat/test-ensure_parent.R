test_that("ensure_parent() works correctly", {
  # Create test data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,     0,     1,    1, 100,
    1,     1,     0,    1,  50,
    1,     2,     0,    1,  25,
    2,     0,     1,    1, 100,
    2,     1,     0,    1,  50,
    2,     2,     0,    1,  25
  )
  test_nif <- nif(test_data)

  # Test basic functionality
  result <- ensure_parent(test_nif)
  expect_true("PARENT" %in% names(result))
  expect_equal(unique(result$PARENT), "CMT1")
  expect_true(inherits(result, "nif"))

  # Test with multiple CMT values
  multi_cmt_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,     0,     1,    1, 100,
    1,     1,     0,    1,  50,
    1,     2,     0,    1,  25,
    2,     0,     1,    2, 100,
    2,     1,     0,    2,  50,
    2,     2,     0,    2,  25
  ) %>% nif()

  result <- ensure_parent(multi_cmt_nif)
  expect_true("PARENT" %in% names(result))
  expect_equal(unique(result$PARENT), c("CMT1", "CMT2")) # Should use most common CMT

  # Test with existing PARENT column
  existing_parent_data <- test_data
  existing_parent_data$PARENT <- "test"
  existing_parent_nif <- nif(existing_parent_data)
  result <- ensure_parent(existing_parent_nif)
  expect_equal(unique(result$PARENT), "test")

  # Test with no administrations
  no_admin_nif <- nif(
    tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,     0,     0,    1,  50,
    1,     1,     0,    1,  25,
    1,     2,     0,    1,  12
    )
  )

  expect_warning(result <- ensure_parent(no_admin_nif))
  expect_false("PARENT" %in% names(result))

  # Test error conditions
  # Missing EVID column
  missing_evid_data <- test_data[, -which(names(test_data) == "EVID")]
  missing_evid_nif <- nif(missing_evid_data)
  expect_error(ensure_parent(missing_evid_nif), "Missing required columns: EVID")

  # Missing CMT column
  missing_cmt_data <- test_data[, -which(names(test_data) == "CMT")]
  missing_cmt_nif <- nif(missing_cmt_data)
  expect_error(ensure_parent(missing_cmt_nif), "Missing required columns: CMT")

  # Not a NIF object
  expect_error(ensure_parent(as.data.frame(test_nif)), "Input must be a nif object")
})
