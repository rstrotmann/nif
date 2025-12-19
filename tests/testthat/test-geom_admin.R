test_that("geom_admin works with valid inputs", {
  # Create test data
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  FALSE,
    3,  TRUE,
    4,  FALSE,
    5,  TRUE
  )

  # Test basic usage
  p <- ggplot2::ggplot(test_data) +
    geom_admin(ggplot2::aes(x = x, admin = admin))
  expect_s3_class(p, "ggplot")

  # Test with custom aesthetics
  p_custom <- ggplot2::ggplot(test_data) +
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      color = "red",
      linewidth = 1,
      linetype = "dashed",
      alpha = 0.5
    )
  expect_s3_class(p_custom, "ggplot")
})


test_that("geom_admin validates parameters correctly", {
  # Create test data
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  FALSE
  )

  # Test invalid mapping
  expect_error(
    geom_admin(mapping = list(x = "x", admin = "admin")),
    "mapping must be created using aes()"
  )

  # Test invalid na.rm
  expect_error(
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      na.rm = "TRUE"
    ),
    "na.rm must be a single logical value"
  )

  # Test invalid show.legend
  expect_error(
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      show.legend = "TRUE"
    ),
    "show.legend must be a single logical value"
  )

  # Test invalid inherit.aes
  expect_error(
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      inherit.aes = "TRUE"
    ),
    "inherit.aes must be a single logical value"
  )

  # Test invalid color
  expect_error(
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      color = 1
    ),
    "color must be a single character value"
  )

  # Test invalid linewidth
  expect_error(
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      linewidth = -1
    ),
    "linewidth must be a non-negative numeric value"
  )

  # Test invalid linetype
  expect_error(
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      linetype = TRUE
    ),
    "linetype must be a single numeric or character value"
  )

  # Test invalid alpha
  expect_error(
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      alpha = 2
    ),
    "alpha must be NA or a numeric value between 0 and 1"
  )
})


test_that("geom_admin handles different data types", {
  # Test with logical admin column
  test_data_logical <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  FALSE,
    3,  TRUE
  )
  p_logical <- ggplot2::ggplot(test_data_logical) +
    geom_admin(ggplot2::aes(x = x, admin = admin))
  expect_s3_class(p_logical, "ggplot")

  # Test with numeric admin column (0/1)
  test_data_numeric <- tibble::tribble(
    ~x, ~admin,
    1,  1,
    2,  0,
    3,  1
  )
  p_numeric <- ggplot2::ggplot(test_data_numeric) +
    geom_admin(ggplot2::aes(x = x, admin = admin))
  expect_s3_class(p_numeric, "ggplot")
})


test_that("geom_admin handles NA values", {
  # Test with NA values in admin column
  test_data_na <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  NA,
    3,  TRUE
  )
  p_na <- ggplot2::ggplot(test_data_na) +
    geom_admin(ggplot2::aes(x = x, admin = admin))
  expect_s3_class(p_na, "ggplot")
})


test_that("geom_admin works with different linetype values", {
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  TRUE
  )

  # Test numeric linetype
  p_numeric <- ggplot2::ggplot(test_data) +
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      linetype = 2
    )
  expect_s3_class(p_numeric, "ggplot")

  # Test character linetype
  p_char <- ggplot2::ggplot(test_data) +
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      linetype = "dashed"
    )
  expect_s3_class(p_char, "ggplot")
})


test_that("geom_admin works with different color values", {
  test_data <- tibble::tribble(
    ~x, ~admin,
    1,  TRUE,
    2,  TRUE
  )

  # Test named color
  p_named <- ggplot2::ggplot(test_data) +
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      color = "red"
    )
  expect_s3_class(p_named, "ggplot")

  # Test hex color
  p_hex <- ggplot2::ggplot(test_data) +
    geom_admin(
      ggplot2::aes(x = x, admin = admin),
      color = "#FF0000"
    )
  expect_s3_class(p_hex, "ggplot")
})


test_that("geom_admin works with empty data", {
  # Test with empty data frame
  test_data_empty <- tibble::tribble(
    ~x, ~admin
  )
  p_empty <- ggplot2::ggplot(test_data_empty) +
    geom_admin(ggplot2::aes(x = x, admin = admin))
  expect_s3_class(p_empty, "ggplot")
})
