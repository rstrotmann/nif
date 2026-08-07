## Tests for remove_duplicate_administrations

remove_duplicate_administrations <- nif:::remove_duplicate_administrations


test_that("remove_duplicate_administrations leaves unique rows unchanged", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-02 08:00"),      "A",     1,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$AMT, c(100, 100))
  expect_false(any(c(".n", "EXCLUDE", "EXCLUDE_DIFFERENT_AMT") %in% names(result)))
})


test_that("remove_duplicate_administrations removes same-AMT duplicates keeping first .SEQ", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT, ~EXTRT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,  100,    "A",
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,    "A",
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     3,  100,    "A"
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
  expect_equal(result$AMT, 100)
})


test_that("remove_duplicate_administrations removes different-AMT duplicates keeping last .SEQ", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,   50,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     3,   75
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 3)
  expect_equal(result$AMT, 75)
})


test_that("remove_duplicate_administrations applies same-AMT pass before different-AMT pass", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     3,   50
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  # same-AMT pair collapses to .SEQ 1 (100), then different-AMT keeps last (.SEQ 3, 50)
  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 3)
  expect_equal(result$AMT, 50)
})


test_that("remove_duplicate_administrations treats subjects independently", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,  100,
       "002", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "002", as.POSIXct("2024-01-01 08:00"),      "A",     2,  200
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  result <- result |> dplyr::arrange(.data$USUBJID)
  expect_equal(nrow(result), 2)
  expect_equal(result$USUBJID, c("001", "002"))
  expect_equal(result$AMT, c(100, 200))
  expect_equal(result$.SEQ, c(1, 2))
})


test_that("remove_duplicate_administrations does not collapse different analytes", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "B",     1,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 2)
  expect_setequal(result$ANALYTE, c("A", "B"))
})


test_that("remove_duplicate_administrations does not collapse different DTCs", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-02 08:00"),      "A",     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 2)
})


test_that("remove_duplicate_administrations collapses NA DTCs in the same group", {
  obj <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~.SEQ, ~AMT,
       "001",      "A",     1,  100,
       "001",      "A",     2,  100
  ) |>
    dplyr::mutate(DTC = as.POSIXct(c(NA, NA)))

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
})


test_that("remove_duplicate_administrations handles empty input", {
  obj <- tibble::tibble(
    USUBJID = character(),
    DTC     = as.POSIXct(character()),
    ANALYTE = character(),
    .SEQ    = numeric(),
    AMT     = numeric()
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 0)
  expect_true(all(c("USUBJID", "DTC", "ANALYTE", ".SEQ", "AMT") %in% names(result)))
})


test_that("remove_duplicate_administrations silent suppresses same-AMT warning", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,  100
  )

  expect_silent(remove_duplicate_administrations(obj, silent = TRUE))

  expect_message(
    remove_duplicate_administrations(obj, silent = FALSE),
    "duplicate administrations"
  )
})


test_that("remove_duplicate_administrations always warns for different AMT", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,   50
  )

  expect_message(
    remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )
  expect_message(
    remove_duplicate_administrations(obj, silent = FALSE),
    "different AMT"
  )
})


test_that("remove_duplicate_administrations validates required fields", {
  expect_error(
    remove_duplicate_administrations(data.frame(USUBJID = "001"), silent = TRUE),
    "Missing columns"
  )
  expect_error(
    remove_duplicate_administrations(NULL, silent = TRUE),
    "must not be NULL"
  )
  expect_error(
    remove_duplicate_administrations(
      tibble::tribble(
        ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
           "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100
      ),
      silent = "yes"
    ),
    "silent"
  )
})


test_that("remove_duplicate_administrations preserves extra columns on kept rows", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT, ~EXTRT, ~NOTE,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,    "A", "keep",
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,  100,    "A", "drop"
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$NOTE, "keep")
  expect_equal(result$EXTRT, "A")
})


test_that("remove_duplicate_administrations keeps one row per USUBJID-DTC-ANALYTE after mixed overlaps", {
  obj <- tibble::tribble(
    ~USUBJID,                 ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-01 08:00"),      "A",     2,  100,
       "001", as.POSIXct("2024-01-02 08:00"),      "A",     1,  100,
       "001", as.POSIXct("2024-01-02 08:00"),      "A",     2,   50,
       "001", as.POSIXct("2024-01-03 08:00"),      "A",     1,  100
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(nrow(result), 3)
  expect_equal(
    result |>
      dplyr::count(.data$USUBJID, .data$DTC, .data$ANALYTE) |>
      dplyr::filter(.data$n > 1) |>
      nrow(),
    0
  )

  day2 <- result |> dplyr::filter(as.Date(.data$DTC) == as.Date("2024-01-02"))
  expect_equal(day2$AMT, 50)
  expect_equal(day2$.SEQ, 2)
})
