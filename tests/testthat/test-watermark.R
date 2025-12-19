test_that("watermark creates correct grob with default parameters", {
  # Create a simple plot
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  # Add watermark
  p_with_watermark <- p + watermark("Test Watermark")

  # Check that the plot has an annotation layer
  expect_true(inherits(p_with_watermark$layers[[2]], "LayerInstance"))
  expect_equal(p_with_watermark$layers[[2]]$geom, ggplot2::GeomCustomAnn)
})


test_that("watermark handles empty or NA input correctly", {
  # Test with NULL
  expect_null(watermark(NULL))

  # Test with NA
  expect_null(watermark(NA))

  # Test with empty string
  expect_null(watermark(""))
})


test_that("watermark validates numeric parameters", {
  # Test invalid cex
  expect_error(watermark("test", cex = -1), "cex must be a positive numeric value")
  expect_error(watermark("test", cex = "invalid"), "cex must be a positive numeric value")

  # Test invalid alpha
  expect_error(watermark("test", alpha = -0.1), "alpha must be a numeric value between 0 and 1")
  expect_error(watermark("test", alpha = 1.5), "alpha must be a numeric value between 0 and 1")

  # Test invalid x position
  expect_error(watermark("test", x = -0.1), "x must be a numeric value between 0 and 1")
  expect_error(watermark("test", x = 1.5), "x must be a numeric value between 0 and 1")

  # Test invalid y position
  expect_error(watermark("test", y = -0.1), "y must be a numeric value between 0 and 1")
  expect_error(watermark("test", y = 1.5), "y must be a numeric value between 0 and 1")
})


test_that("watermark validates fontface parameter", {
  # Test valid fontfaces
  expect_silent(watermark("test", fontface = "plain"))
  expect_silent(watermark("test", fontface = "bold"))
  expect_silent(watermark("test", fontface = "italic"))
  expect_silent(watermark("test", fontface = "bold.italic"))

  # Test invalid fontface
  expect_error(
    watermark("test", fontface = "invalid"),
    "fontface must be one of: 'plain', 'bold', 'italic', 'bold.italic'"
  )
})


test_that("watermark adjusts text size for long text", {
  # Create a long watermark text
  long_text <- paste(rep("a", 30), collapse = "")

  # Add watermark with long text
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    watermark(long_text)

  # Check that the plot has an annotation layer
  expect_true(inherits(p$layers[[2]], "LayerInstance"))
  expect_equal(p$layers[[2]]$geom, ggplot2::GeomCustomAnn)
})


test_that("watermark handles custom parameters correctly", {
  # Test with custom color
  p1 <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    watermark("test", color = "red")
  expect_true(inherits(p1$layers[[2]], "LayerInstance"))

  # Test with custom alpha
  p2 <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    watermark("test", alpha = 0.5)
  expect_true(inherits(p2$layers[[2]], "LayerInstance"))

  # Test with custom position
  p3 <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    watermark("test", x = 0.8, y = 0.8)
  expect_true(inherits(p3$layers[[2]], "LayerInstance"))

  # Test with custom rotation
  p4 <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    watermark("test", rotation = 45)
  expect_true(inherits(p4$layers[[2]], "LayerInstance"))
})
