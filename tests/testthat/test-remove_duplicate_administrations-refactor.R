## Characterization tests for remove_duplicate_administrations()
##
## These tests pin current behavior so an efficiency refactor (one arrange,
## vectorized duplicated()/slice, early return when unique) cannot change
## which rows are kept, how NAs and tied .SEQ are resolved, output shape,
## or when warnings fire.

remove_duplicate_administrations <- nif:::remove_duplicate_administrations

d1  <- as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
d1b <- as.POSIXct("2024-01-01 08:00:01", tz = "UTC")
d2  <- as.POSIXct("2024-01-02 08:00:00", tz = "UTC")


# ---- identity and shape ------------------------------------------------------

test_that("unique rows are kept and helper columns are not returned", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d2,      "A",     1,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$AMT, c(100, 100))
  expect_false(any(c(".n", "EXCLUDE", "EXCLUDE_DIFFERENT_AMT") %in% names(result)))
})


test_that("output is arranged by USUBJID, ANALYTE, DTC, .SEQ even when unique", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "002",   d1,      "B",     1,   50,
       "001",   d1,      "B",     1,  100,
       "001",   d2,      "A",     2,  200,
       "001",   d1,      "A",     1,  300
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 4)
  expect_equal(result$USUBJID, c("001", "001", "001", "002"))
  expect_equal(result$ANALYTE, c("A", "A", "B", "B"))
  expect_equal(result$DTC, c(d1, d2, d1, d1))
  expect_equal(result$.SEQ, c(1, 2, 1, 1))
  expect_equal(result$AMT, c(300, 200, 100, 50))
})


test_that("single row input is returned unchanged aside from tibble class", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT, ~EXTRT,
       "001",   d1,      "A",     1,  100,    "A"
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$USUBJID, "001")
  expect_equal(result$AMT, 100)
  expect_equal(result$EXTRT, "A")
})


test_that("empty input keeps required columns and has zero rows", {
  obj <- tibble::tibble(
    USUBJID = character(),
    DTC     = as.POSIXct(character(), tz = "UTC"),
    ANALYTE = character(),
    .SEQ    = numeric(),
    AMT     = numeric()
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 0)
  expect_true(all(c("USUBJID", "DTC", "ANALYTE", ".SEQ", "AMT") %in% names(result)))
  expect_false(any(c(".n", "EXCLUDE", "EXCLUDE_DIFFERENT_AMT") %in% names(result)))
})


test_that("data.frame input is returned as a tibble", {
  obj <- data.frame(
    USUBJID = "001",
    DTC     = d1,
    ANALYTE = "A",
    .SEQ    = 1,
    AMT     = 100,
    stringsAsFactors = FALSE
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})


test_that("plain data.frame without tibble class is accepted", {
  obj <- data.frame(
    USUBJID = c("001", "001"),
    DTC     = c(d1, d1),
    ANALYTE = c("A", "A"),
    .SEQ    = c(2, 1),
    AMT     = c(100, 100),
    stringsAsFactors = FALSE
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
})


test_that("column order of original fields is preserved", {
  obj <- tibble::tribble(
    ~NOTE, ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT, ~EXTRT,
     "x",     "001",   d1,      "A",     1,  100,    "A"
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(
    names(result),
    c("NOTE", "USUBJID", "DTC", "ANALYTE", ".SEQ", "AMT", "EXTRT")
  )
})


test_that("ID, TIME, and EXTRT are optional on input", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_false("ID" %in% names(result))
  expect_false("TIME" %in% names(result))
})


test_that("output is ungrouped even when input is grouped by USUBJID", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     1,  100,
       "002",   d1,      "A",     1,   50
  ) |>
    dplyr::group_by(.data$USUBJID)

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_false(dplyr::is_grouped_df(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$.SEQ[result$USUBJID == "001"], 1)
})


# ---- same-AMT pass: keep first after arrange by .SEQ -------------------------

test_that("same-AMT duplicates keep the lowest .SEQ, not the first input row", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,  ~NOTE,
       "001",   d1,      "A",     2,  100, "drop",
       "001",   d1,      "A",     1,  100, "keep",
       "001",   d1,      "A",     3,  100, "drop"
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
  expect_equal(result$NOTE, "keep")
  expect_equal(result$AMT, 100)
})


test_that("same-AMT pass removes n-1 rows and keeps exactly one", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,  100,
       "001",   d1,      "A",     4,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
})


test_that("tied .SEQ with same AMT keeps the first row in original order", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,   ~NOTE,
       "001",   d1,      "A",     1,  100, "first",
       "001",   d1,      "A",     1,  100, "second"
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$NOTE, "first")
})


test_that("extra columns on the kept same-AMT row are copied, not aggregated", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT, ~EXTRT, ~IMPUTATION, ~CMT, ~EVID,
       "001",   d1,      "A",     1,  100,    "A",     "start",    1,     1,
       "001",   d1,      "A",     2,  100,    "A",       "end",    1,     1
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(result$IMPUTATION, "start")
  expect_equal(result$EXTRT, "A")
  expect_equal(result$CMT, 1)
  expect_equal(result$EVID, 1)
})


# ---- different-AMT pass: keep last after arrange by .SEQ ---------------------

test_that("different-AMT duplicates keep the highest .SEQ, not the first input row", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,  ~NOTE,
       "001",   d1,      "A",     3,   75, "keep",
       "001",   d1,      "A",     1,  100, "drop",
       "001",   d1,      "A",     2,   50, "drop"
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 3)
  expect_equal(result$AMT, 75)
  expect_equal(result$NOTE, "keep")
})


test_that("different-AMT pass removes n-1 rows among distinct doses", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,   50,
       "001",   d1,      "A",     3,   75,
       "001",   d1,      "A",     4,   25
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "3 duplicate administrations with different AMT"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 4)
  expect_equal(result$AMT, 25)
})


test_that("tied .SEQ with different AMT keeps the last row in original order", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,   ~NOTE,
       "001",   d1,      "A",     1,  100, "first",
       "001",   d1,      "A",     1,   50, "second"
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$AMT, 50)
  expect_equal(result$NOTE, "second")
})


# ---- two-pass interaction ----------------------------------------------------

test_that("same-AMT collapse runs before different-AMT keep-last", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,   50
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  # same-AMT keeps .SEQ 1 (100); different-AMT then keeps last remaining (.SEQ 3)
  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 3)
  expect_equal(result$AMT, 50)
})


test_that("two same-AMT pairs collapse to first of each, then last remaining .SEQ", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,   50,
       "001",   d1,      "A",     4,   50
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  # same-AMT keeps .SEQ 1 (100) and .SEQ 3 (50); different-AMT keeps .SEQ 3
  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 3)
  expect_equal(result$AMT, 50)
})


test_that("later same-AMT duplicate does not beat an earlier different AMT", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,   50,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,  100
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  # same-AMT keeps .SEQ 1 (50) and .SEQ 2 (100); different-AMT keeps .SEQ 2
  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 2)
  expect_equal(result$AMT, 100)
})


test_that("result has one row per USUBJID-DTC-ANALYTE after mixed overlaps", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d2,      "A",     1,  100,
       "001",   d2,      "A",     2,   50,
       "001", as.POSIXct("2024-01-03 08:00:00", tz = "UTC"),      "A",     1,  100
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(nrow(result), 3)
  n_dup <- result |>
    dplyr::count(.data$USUBJID, .data$DTC, .data$ANALYTE) |>
    dplyr::filter(.data$n > 1) |>
    nrow()
  expect_equal(n_dup, 0)

  day2 <- result[as.Date(result$DTC) == as.Date("2024-01-02"), ]
  expect_equal(day2$AMT, 50)
  expect_equal(day2$.SEQ, 2)
})


# ---- grouping keys -----------------------------------------------------------

test_that("subjects are independent", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "002",   d1,      "A",     1,  100,
       "002",   d1,      "A",     2,  200
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


test_that("different analytes at the same DTC are not collapsed", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "B",     1,  100,
       "001",   d1,      "B",     2,   50
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(sort(result$ANALYTE), c("A", "B"))
  expect_equal(result$AMT[result$ANALYTE == "A"], 100)
  expect_equal(result$AMT[result$ANALYTE == "B"], 50)
})


test_that("different DTCs are not collapsed", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d2,      "A",     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 2)
})


test_that("DTC one second apart are not treated as duplicates", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",  d1b,      "A",     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 2)
})


test_that("ID is not a grouping key", {
  obj <- tibble::tribble(
      ~ID, ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
        1,    "001",   d1,      "A",     1,  100,
        2,    "001",   d1,      "A",     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
  expect_equal(result$.SEQ, 1)
})


test_that("multiple subjects and analytes are resolved independently", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "B",     1,   10,
       "001",   d1,      "B",     2,   20,
       "002",   d1,      "A",     1,  200
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$AMT[result$USUBJID == "001" & result$ANALYTE == "A"], 100)
  expect_equal(result$.SEQ[result$USUBJID == "001" & result$ANALYTE == "A"], 1)
  expect_equal(result$AMT[result$USUBJID == "001" & result$ANALYTE == "B"], 20)
  expect_equal(result$AMT[result$USUBJID == "002"], 200)
})


# ---- NA grouping -------------------------------------------------------------

test_that("NA DTCs in the same USUBJID-ANALYTE group are collapsed", {
  obj <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~.SEQ, ~AMT,
       "001",      "A",     1,  100,
       "001",      "A",     2,  100
  ) |>
    dplyr::mutate(DTC = as.POSIXct(c(NA, NA), tz = "UTC"))

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
})


test_that("NA AMT values group together as same-AMT duplicates", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     2,   NA,
       "001",   d1,      "A",     1,   NA
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
  expect_true(is.na(result$AMT))
})


test_that("NA AMT versus numeric AMT is a different-AMT duplicate", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,   NA
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  # arrange puts NA .SEQ-equivalent AMT row last among remaining, keep .SEQ 2
  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 2)
  expect_true(is.na(result$AMT))
})


test_that("NA .SEQ with same AMT is sorted last so the non-NA .SEQ is kept", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",    NA,  100,
       "001",   d1,      "A",     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 2)
})


test_that("NA .SEQ with different AMT is sorted last and therefore kept", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",    NA,   50
  )

  expect_message(
    result <- remove_duplicate_administrations(obj, silent = TRUE),
    "different AMT"
  )

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$.SEQ))
  expect_equal(result$AMT, 50)
})


test_that("NA ANALYTE values group together", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,       NA,     1,  100,
       "001",   d1,       NA,     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
  expect_true(is.na(result$ANALYTE))
})


test_that("NA USUBJID values group together", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
          NA,   d1,      "A",     1,  100,
          NA,   d1,      "A",     2,  100
  )

  result <- remove_duplicate_administrations(obj, silent = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$.SEQ, 1)
  expect_true(is.na(result$USUBJID))
})


# ---- warnings ----------------------------------------------------------------

test_that("silent = TRUE suppresses same-AMT warning", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100
  )

  expect_silent(remove_duplicate_administrations(obj, silent = TRUE))
})


test_that("silent = FALSE emits same-AMT warning with removed-row count", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,  100
  )

  expect_message(
    remove_duplicate_administrations(obj, silent = FALSE),
    "2 duplicate administrations with respect to USUBJID, DTC and ANALYTE were removed"
  )
})


test_that("same-AMT warning lists removed rows, not the kept row", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,  100
  )

  msgs <- testthat::capture_messages(
    remove_duplicate_administrations(obj, silent = FALSE)
  )
  text <- paste(msgs, collapse = "\n")

  expect_match(text, "2 duplicate administrations")
  expect_match(text, "2     A")
  expect_match(text, "3     A")
  expect_no_match(text, "1     A")
})


test_that("different-AMT warning is emitted even when silent = TRUE", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,   50
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


test_that("mixed duplicates emit both warnings with silent = FALSE", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,   50
  )

  msgs <- testthat::capture_messages(
    remove_duplicate_administrations(obj, silent = FALSE)
  )
  text <- paste(msgs, collapse = "\n")

  expect_match(text, "duplicate administrations with respect to USUBJID")
  expect_match(text, "different AMT")
})


test_that("mixed duplicates with silent = TRUE emit only the different-AMT warning", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "001",   d1,      "A",     3,   50
  )

  msgs <- testthat::capture_messages(
    remove_duplicate_administrations(obj, silent = TRUE)
  )
  text <- paste(msgs, collapse = "\n")

  expect_false(grepl("with respect to USUBJID, DTC and ANALYTE were removed", text))
  expect_match(text, "different AMT")
})


test_that("same-AMT warning count is total removed rows, not number of groups", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100,
       "002",   d1,      "A",     1,  200,
       "002",   d1,      "A",     2,  200,
       "002",   d1,      "A",     3,  200
  )

  expect_message(
    remove_duplicate_administrations(obj, silent = FALSE),
    "3 duplicate administrations with respect to USUBJID, DTC and ANALYTE were removed"
  )
})


test_that("unique rows emit no messages when silent = FALSE", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d2,      "A",     1,  100
  )

  expect_silent(remove_duplicate_administrations(obj, silent = FALSE))
})


test_that("empty input emits no messages when silent = FALSE", {
  obj <- tibble::tibble(
    USUBJID = character(),
    DTC     = as.POSIXct(character(), tz = "UTC"),
    ANALYTE = character(),
    .SEQ    = numeric(),
    AMT     = numeric()
  )

  expect_silent(remove_duplicate_administrations(obj, silent = FALSE))
})


test_that("silent = NULL follows package silent option for same-AMT warnings", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100
  )

  old <- get("silent", envir = nif:::.nif_env)
  withr::defer(assign("silent", old, envir = nif:::.nif_env))

  assign("silent", TRUE, envir = nif:::.nif_env)
  expect_silent(remove_duplicate_administrations(obj, silent = NULL))

  assign("silent", FALSE, envir = nif:::.nif_env)
  expect_message(
    remove_duplicate_administrations(obj, silent = NULL),
    "duplicate administrations with respect to USUBJID"
  )
})


test_that("explicit silent overrides package silent option", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100,
       "001",   d1,      "A",     2,  100
  )

  old <- get("silent", envir = nif:::.nif_env)
  withr::defer(assign("silent", old, envir = nif:::.nif_env))

  assign("silent", FALSE, envir = nif:::.nif_env)
  expect_silent(remove_duplicate_administrations(obj, silent = TRUE))

  assign("silent", TRUE, envir = nif:::.nif_env)
  expect_message(
    remove_duplicate_administrations(obj, silent = FALSE),
    "duplicate administrations with respect to USUBJID"
  )
})


test_that("optional ID, TIME, and EXTRT appear in warning output when present", {
  obj <- tibble::tribble(
      ~ID, ~USUBJID, ~TIME, ~DTC, ~ANALYTE, ~.SEQ, ~AMT, ~EXTRT,
        9,    "001",     0,   d1,      "A",     1,  100,    "A",
        9,    "001",     0,   d1,      "A",     2,  100,    "A"
  )

  msgs <- testthat::capture_messages(
    remove_duplicate_administrations(obj, silent = FALSE)
  )
  text <- paste(msgs, collapse = "\n")

  expect_match(text, "ID")
  expect_match(text, "TIME")
  expect_match(text, "EXTRT")
})


# ---- validation --------------------------------------------------------------

test_that("NULL input is rejected", {
  expect_error(
    remove_duplicate_administrations(NULL, silent = TRUE),
    "must not be NULL"
  )
})


test_that("non-data.frame input is rejected", {
  expect_error(
    remove_duplicate_administrations(list(), silent = TRUE),
    "must be a data.frame"
  )
  expect_error(
    remove_duplicate_administrations(1:3, silent = TRUE),
    "must be a data.frame"
  )
})


test_that("each required field is validated individually", {
  base <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100
  )

  for (col in c("USUBJID", "DTC", "ANALYTE", ".SEQ", "AMT")) {
    obj <- base
    obj[[col]] <- NULL
    expect_error(
      remove_duplicate_administrations(obj, silent = TRUE),
      col,
      info = col
    )
  }
})


test_that("multiple missing required fields are reported together", {
  expect_error(
    remove_duplicate_administrations(
      data.frame(USUBJID = "001"),
      silent = TRUE
    ),
    "Missing columns"
  )
})


test_that("silent must be a single logical or NULL", {
  obj <- tibble::tribble(
    ~USUBJID, ~DTC, ~ANALYTE, ~.SEQ, ~AMT,
       "001",   d1,      "A",     1,  100
  )

  expect_error(
    remove_duplicate_administrations(obj, silent = "yes"),
    "silent"
  )
  expect_error(
    remove_duplicate_administrations(obj, silent = 1),
    "silent"
  )
  expect_error(
    remove_duplicate_administrations(obj, silent = c(TRUE, FALSE)),
    "silent"
  )
  expect_error(
    remove_duplicate_administrations(obj, silent = NA),
    "silent"
  )
})
