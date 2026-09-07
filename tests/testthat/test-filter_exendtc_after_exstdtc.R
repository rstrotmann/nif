filter_exendtc_after_exstdtc <- nif:::filter_exendtc_after_exstdtc


test_that("filter_exendtc_after_exstdtc removes end-before-start episodes", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,     ~EXSTDTC,     ~EXENDTC,
           1,      1,    "A", "2025-01-01", "2025-01-10",
           1,      2,    "A", "2025-01-15", "2025-01-20",
           2,      1,    "A", "2025-01-01",           NA,
           2,      2,    "A", "2025-01-20", "2025-01-10"
  )

  dm <- tibble::tribble(
    ~USUBJID,     ~RFENDTC,
           1, "2025-01-20",
           2, "2025-01-20"
  )

  expect_message(
    result <- filter_exendtc_after_exstdtc(ex, dm, "A"),
    "EXSTDTC after EXENDTC"
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$EXSEQ, c(1, 2))
  expect_equal(result$USUBJID, c(1, 1))
})


test_that("filter_exendtc_after_exstdtc removes same-day start after end time", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,           ~EXSTDTC,           ~EXENDTC,
           1,      1,    "A", "2025-01-01T10:00", "2025-01-01T08:00",
           1,      2,    "A", "2025-01-02T08:00", "2025-01-02T10:00"
  )

  dm <- tibble::tribble(
    ~USUBJID,     ~RFENDTC,
           1, "2025-01-02"
  )

  result <- filter_exendtc_after_exstdtc(ex, dm, "A", silent = TRUE)
  expect_equal(nrow(result), 1)
  expect_equal(result$EXSEQ, 2)
})


test_that("filter_exendtc_after_exstdtc keeps date-only same-day episodes", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,     ~EXSTDTC,     ~EXENDTC,
           1,      1,    "A", "2025-01-01", "2025-01-01"
  )

  dm <- tibble::tribble(
    ~USUBJID,     ~RFENDTC,
           1, "2025-01-01"
  )

  result <- filter_exendtc_after_exstdtc(ex, dm, "A", silent = TRUE)
  expect_equal(nrow(result), 1)
})


test_that("filter_exendtc_after_exstdtc keeps timed start with date-only end", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,           ~EXSTDTC,     ~EXENDTC,
           1,      1,    "A", "2025-01-01T10:00", "2025-01-01"
  )

  dm <- tibble::tribble(
    ~USUBJID,     ~RFENDTC,
           1, "2025-01-01"
  )

  result <- filter_exendtc_after_exstdtc(ex, dm, "A", silent = TRUE)
  expect_equal(nrow(result), 1)
})


test_that("filter_exendtc_after_exstdtc drops missing EXENDTC", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,     ~EXSTDTC, ~EXENDTC,
           1,      1,    "A", "2025-01-01",       NA
  )

  dm <- tibble::tribble(
    ~USUBJID,     ~RFENDTC,
           1, "2025-01-01"
  )

  expect_silent(
    result <- filter_exendtc_after_exstdtc(ex, dm, "A", silent = TRUE)
  )
  expect_equal(nrow(result), 0)
})


test_that("filter_exendtc_after_exstdtc keeps only the requested treatment", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,     ~EXSTDTC,     ~EXENDTC,
           1,      1,    "A", "2025-01-01", "2025-01-10",
           1,      1,    "B", "2025-01-01", "2025-01-10"
  )

  dm <- tibble::tribble(
    ~USUBJID,     ~RFENDTC,
           1, "2025-01-10"
  )

  result <- filter_exendtc_after_exstdtc(ex, dm, "A", silent = TRUE)
  expect_equal(nrow(result), 1)
  expect_equal(result$EXTRT, "A")
})


test_that("filter_exendtc_after_exstdtc is silent when silent = TRUE", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,     ~EXSTDTC,     ~EXENDTC,
           1,      1,    "A", "2025-01-20", "2025-01-10"
  )

  dm <- tibble::tribble(
    ~USUBJID,     ~RFENDTC,
           1, "2025-01-20"
  )

  expect_silent(filter_exendtc_after_exstdtc(ex, dm, "A", silent = TRUE))
})


test_that("filter_exendtc_after_exstdtc requires EX columns", {
  expect_error(
    filter_exendtc_after_exstdtc(
      tibble::tibble(USUBJID = 1),
      tibble::tibble(USUBJID = 1),
      "A"
    ),
    "Missing columns in domain EX"
  )
})
