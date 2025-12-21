test_that("date_list returns correct structure", {
  result <- date_list("2025-01-01", "2025-01-03")

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, NULL)

  expect_s3_class(result[[1]], "Date")
  expect_type(result[[2]], "integer")
})


test_that("date_list generates correct date sequences", {
  # Three day range
  result <- date_list("2025-01-01", "2025-01-03")
  expect_equal(result[[1]], as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")))
  expect_length(result[[1]], 3)

  # Single day
  result <- date_list("2025-01-01", "2025-01-01")
  expect_equal(result[[1]], as.Date("2025-01-01"))
  expect_length(result[[1]], 1)

  # Longer range
  result <- date_list("2025-01-01", "2025-01-10")
  expect_equal(result[[1]], seq(as.Date("2025-01-01"), as.Date("2025-01-10"), by = "days"))
  expect_length(result[[1]], 10)

  # Cross month boundary
  result <- date_list("2025-01-30", "2025-02-02")
  expect_equal(result[[1]], as.Date(c("2025-01-30", "2025-01-31", "2025-02-01", "2025-02-02")))
  expect_length(result[[1]], 4)

  # Cross year boundary
  result <- date_list("2024-12-30", "2025-01-02")
  expect_equal(result[[1]], as.Date(c("2024-12-30", "2024-12-31", "2025-01-01", "2025-01-02")))
  expect_length(result[[1]], 4)
})


test_that("date_list handles missing endtc", {
  result <- date_list("2025-01-01", NA)

  expect_equal(result[[1]], as.Date("2025-01-01"))
  expect_length(result[[1]], 1)
  expect_length(result[[2]], 1)
})

test_that("date_list generates study days when both stdy and endy provided", {
  result <- date_list("2025-01-01", "2025-01-03", 1, 3)

  expect_equal(result[[1]], as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")))
  expect_equal(result[[2]], c(1, 2, 3))

  # Non-consecutive study days
  result <- date_list("2025-01-01", "2025-01-03", 10, 12)
  expect_equal(result[[2]], c(10, 11, 12))

  # Single day with study days
  result <- date_list("2025-01-01", "2025-01-01", 5, 5)
  expect_equal(result[[1]], as.Date("2025-01-01"))
  expect_equal(result[[2]], 5)
})

test_that("date_list generates study days when only stdy provided", {
  result <- date_list("2025-01-01", "2025-01-03", 1, NA)

  expect_equal(result[[1]], as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")))
  expect_equal(result[[2]], c(1, 2, 3))

  # Starting from non-1
  result <- date_list("2025-01-01", "2025-01-05", 10, NA)
  expect_equal(result[[2]], c(10, 11, 12, 13, 14))

  # Single day
  result <- date_list("2025-01-01", "2025-01-01", 7, NA)
  expect_equal(result[[2]], 7)
})

test_that("date_list handles missing study days", {
  result <- date_list("2025-01-01", "2025-01-03", NA, NA)

  expect_equal(result[[1]], as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")))
  expect_equal(result[[2]], c(NA_real_, NA_real_, NA_real_))

  # With missing endtc
  result <- date_list("2025-01-01", NA, NA, NA)
  expect_equal(result[[1]], as.Date("2025-01-01"))
  expect_equal(result[[2]], NA_real_)
})

test_that("date_list handles endy without stdy", {
  result <- date_list("2025-01-01", "2025-01-03", NA, 3)

  expect_equal(result[[1]], as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")))
  expect_equal(result[[2]], c(NA_real_, NA_real_, NA_real_))
})

test_that("date_list handles different date formats", {
  # Standard format
  result1 <- date_list("2025-01-01", "2025-01-03")

  # Alternative formats
  result2 <- date_list("2025/01/01", "2025/01/03")
  expect_equal(result1[[1]], result2[[1]])

  result3 <- date_list("2025-1-1", "2025-1-3")
  expect_equal(result1[[1]], result3[[1]])
})

test_that("date_list ensures dates and days have same length", {
  result <- date_list("2025-01-01", "2025-01-05", 1, 5)

  expect_length(result[[1]], 5)
  expect_length(result[[2]], 5)
  expect_equal(length(result[[1]]), length(result[[2]]))

  result <- date_list("2025-01-01", "2025-01-01", 1, 1)
  expect_length(result[[1]], 1)
  expect_length(result[[2]], 1)
})

test_that("date_list errors when end date before start date", {
  expect_error(
    date_list("2025-01-03", "2025-01-01"),
    "End date before start date"
  )

  expect_error(
    date_list("2025-12-31", "2025-01-01"),
    "End date before start date"
  )
})

test_that("date_list errors when end day before start day", {
  expect_error(
    date_list("2025-01-01", "2025-01-03", 3, 1),
    "End day before start day"
  )

  expect_error(
    date_list("2025-01-01", "2025-01-10", 10, 5),
    "End day before start day"
  )
})

test_that("date_list handles edge case: same date, different study days", {
  # Same date but different study days should error
  expect_error(
    date_list("2025-01-01", "2025-01-01", 5, 3),
    "End day before start day"
  )

  # Same date, same study day should work
  result <- date_list("2025-01-01", "2025-01-01", 3, 3)
  expect_equal(result[[1]], as.Date("2025-01-01"))
  expect_equal(result[[2]], 3)
})

test_that("date_list handles leap year dates", {
  result <- date_list("2024-02-28", "2024-03-01")

  expect_equal(result[[1]], as.Date(c("2024-02-28", "2024-02-29", "2024-03-01")))
  expect_length(result[[1]], 3)

  result <- date_list("2024-02-28", "2024-03-01", 1, 3)
  expect_equal(result[[2]], c(1, 2, 3))
})

test_that("date_list handles month boundaries correctly", {
  # End of month
  result <- date_list("2025-01-31", "2025-02-02")
  expect_equal(result[[1]], as.Date(c("2025-01-31", "2025-02-01", "2025-02-02")))

  # Beginning of month
  result <- date_list("2025-01-30", "2025-02-01")
  expect_equal(result[[1]], as.Date(c("2025-01-30", "2025-01-31", "2025-02-01")))
})

test_that("date_list handles large date ranges", {
  result <- date_list("2025-01-01", "2025-01-31")

  expect_length(result[[1]], 31)
  expect_equal(result[[1]][1], as.Date("2025-01-01"))
  expect_equal(result[[1]][31], as.Date("2025-01-31"))

  result <- date_list("2025-01-01", "2025-01-31", 1, 31)
  expect_equal(result[[2]], 1:31)
})

test_that("date_list handles non-sequential study day ranges", {
  # When stdy and endy are provided, they create a sequence that should match the date count
  # This tests that the function correctly uses the provided study days even when starting from non-1
  result <- date_list("2025-01-01", "2025-01-03", 100, 102)

  expect_length(result[[1]], 3)
  expect_length(result[[2]], 3)
  expect_equal(result[[2]], c(100, 101, 102))
})
