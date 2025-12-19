test_that("nif_disclaimer works", {
  expect_type(nif_disclaimer(), "character")
})


test_that("nif_disclaimer works with custom text", {
  expect_no_error(nif_disclaimer("custom"))
})


test_that("nif_option works", {
  expect_no_error(nif_option(test = "test"))
  expect_message(nif_option(test = FALSE))
  expect_message(nif_option(foo = TRUE))

  nif_option(silent = TRUE, debug = FALSE)
  expect_equal(get("silent", .nif_env), TRUE)
  expect_equal(get("debug", .nif_env), FALSE)
})


test_that("nif_option_value works", {
  nif_option(silent = TRUE)
  expect_equal(nif_option_value("silent"), TRUE)
})


test_that("nif_option with empty arguments returns list", {
  expect_type(nif_option(), "list")
})
