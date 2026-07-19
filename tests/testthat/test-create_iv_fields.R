test_that("create_iv_fields requires a data frame", {
  expect_error(create_iv_fields("not a data frame"), "Input must be a data frame")
  expect_error(create_iv_fields(123), "Input must be a data frame")
  expect_error(create_iv_fields(list()), "Input must be a data frame")
  expect_error(create_iv_fields(NULL), "Input must be a data frame")
})


test_that("create_iv_fields validates argument types", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H"
  )

  expect_error(create_iv_fields(admin, iv_admin = "yes"), "logical")
  expect_error(create_iv_fields(admin, duration = "1"), "numeric")
  expect_error(create_iv_fields(admin, duration = c(1, 2)), "single value")
  expect_error(create_iv_fields(admin, silent = "true"), "logical")
})


test_that("create_iv_fields leaves non-IV data unchanged", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
      "ORAL",  "PT1H",
      "ORAL",  "PT2H"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result, admin)
  expect_false("DUR" %in% names(result))
})


test_that("create_iv_fields leaves data unchanged when iv_admin is FALSE", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H"
  )

  result <- create_iv_fields(admin, iv_admin = FALSE, silent = TRUE)

  expect_equal(result, admin)
  expect_false("DUR" %in% names(result))
})


test_that("create_iv_fields leaves data unchanged when EXROUTE is absent and iv_admin is NULL", {
  admin <- tribble(
    ~EXTRT, ~EXDUR,
    "DRUG", "PT1H"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result, admin)
  expect_false("DUR" %in% names(result))
})


test_that("create_iv_fields auto-detects IV from EXROUTE", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H",
        "IV", "PT30M"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result$DUR, c(1, 0.5))
})


test_that("create_iv_fields auto-detects INTRAVENOUS from EXROUTE", {
  admin <- tribble(
       ~EXROUTE,  ~EXDUR,
  "INTRAVENOUS",   "PT2H",
  "intravenous",  "PT15M"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result$DUR, c(2, 0.25))
})


test_that("create_iv_fields auto-detects mixed-case IV routes", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "iv", "PT1H",
        "Iv", "PT2H"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result$DUR, c(1, 2))
})


test_that("create_iv_fields processes all rows when any route is IV", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H",
      "ORAL", "PT2H"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result$DUR, c(1, 2))
})


test_that("create_iv_fields respects explicit iv_admin = TRUE without EXROUTE", {
  admin <- tribble(
    ~EXTRT, ~EXDUR,
    "DRUG", "PT1H",
    "DRUG", "PT45M"
  )

  result <- create_iv_fields(admin, iv_admin = TRUE, silent = TRUE)

  expect_equal(result$DUR, c(1, 0.75))
})


test_that("create_iv_fields converts EXDUR to DUR in hours", {
  admin <- tribble(
    ~EXROUTE,     ~EXDUR,
        "IV",     "PT1H",
        "IV",    "PT30M",
        "IV",  "PT1H15M",
        "IV",   "PT1.5H",
        "IV",    "-PT1H"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result$DUR, c(1, 0.5, 1.25, 1.5, -1))
})


test_that("create_iv_fields works with a single-row input", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$DUR, 1)
})


test_that("create_iv_fields allows NA in EXDUR", {
  admin <- tribble(
    ~EXROUTE,          ~EXDUR,
        "IV",          "PT1H",
        "IV",  NA_character_,
        "IV",         "PT30M"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result$DUR, c(1, NA_real_, 0.5))
})


test_that("create_iv_fields allows all-NA EXDUR", {
  admin <- tribble(
    ~EXROUTE,         ~EXDUR,
        "IV", NA_character_,
        "IV", NA_character_
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_true(all(is.na(result$DUR)))
  expect_equal(length(result$DUR), 2)
})


test_that("create_iv_fields errors on invalid EXDUR format", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV",   "1H",
        "IV", "PT1H"
  )

  expect_error(
    create_iv_fields(admin, silent = TRUE),
    "EXDUR must be an ISO8601-formatted duration"
  )
})


test_that("create_iv_fields errors on empty-string EXDUR", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV",    "",
        "IV", "PT1H"
  )

  expect_error(
    create_iv_fields(admin, silent = TRUE),
    "EXDUR must be an ISO8601-formatted duration"
  )
})


test_that("create_iv_fields replaces EXDUR with custom duration", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H",
        "IV", "PT2H"
  )

  expect_message(
    expect_message(
      result <- create_iv_fields(admin, duration = 0.5, silent = FALSE),
      "Treatment duration \\(EXDUR\\) was replaced with custom duration \\(0.5\\)"
    ),
    "IV administration"
  )
  expect_equal(result$DUR, c(0.5, 0.5))
})


test_that("create_iv_fields suppresses replacement message when silent", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H"
  )

  expect_no_message(
    result <- create_iv_fields(admin, duration = 0.5, silent = TRUE)
  )
  expect_equal(result$DUR, 0.5)
})


test_that("create_iv_fields uses duration when EXDUR is absent", {
  admin <- tribble(
    ~EXROUTE, ~EXDOSE,
        "IV",      10,
        "IV",      20
  )

  result <- create_iv_fields(admin, duration = 1.5, silent = TRUE)

  expect_equal(result$DUR, c(1.5, 1.5))
})


test_that("create_iv_fields sets DUR to 0 when EXDUR and duration are absent", {
  admin <- tribble(
    ~EXROUTE, ~EXDOSE,
        "IV",      10,
        "IV",      20
  )

  result <- create_iv_fields(admin, silent = TRUE)
  expect_equal(result$DUR, c(0, 0))
})


test_that("create_iv_fields suppresses DUR = 0 message when silent", {
  admin <- tribble(
    ~EXROUTE, ~EXDOSE,
        "IV",      10
  )

  expect_no_message(
    result <- create_iv_fields(admin, silent = TRUE)
  )
  expect_equal(result$DUR, 0)
})


test_that("create_iv_fields preserves existing columns", {
  admin <- tribble(
    ~USUBJID, ~EXTRT, ~EXROUTE, ~EXDUR, ~EXDOSE,
         "A", "DRUG",     "IV",  "PT1H",      10,
         "B", "DRUG",     "IV",  "PT2H",      20
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_equal(result$USUBJID, c("A", "B"))
  expect_equal(result$EXTRT, c("DRUG", "DRUG"))
  expect_equal(result$EXROUTE, c("IV", "IV"))
  expect_equal(result$EXDUR, c("PT1H", "PT2H"))
  expect_equal(result$EXDOSE, c(10, 20))
  expect_equal(result$DUR, c(1, 2))
})


test_that("create_iv_fields returns a data frame", {
  admin <- tribble(
    ~EXROUTE, ~EXDUR,
        "IV", "PT1H"
  )

  result <- create_iv_fields(admin, silent = TRUE)

  expect_s3_class(result, "data.frame")
})

