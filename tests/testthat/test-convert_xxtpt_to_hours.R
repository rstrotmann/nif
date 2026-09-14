eval_tbl <- function(tbl) {
  act <- vapply(
    seq_len(nrow(tbl)),
    function(i) {
      convert_xxtpt_to_hours(
        tbl$input[[i]],
        treatment_duration = tbl$treatment_duration[[i]],
        range_method = tbl$range_method[[i]]
      )
    },
    numeric(1)
  )
  expect_equal(act, tbl$expected, tolerance = 1e-8)
}


# ---- Input validation --------------------------------------------------------

test_that("convert_xxtpt_to_hours rejects non-character xxtpt", {
  expect_error(convert_xxtpt_to_hours(NULL), "xxtpt must not be NULL")
  expect_error(convert_xxtpt_to_hours(1), "xxtpt must be a character value")
  expect_error(convert_xxtpt_to_hours(factor("1H")), "xxtpt must be a character value")
})


test_that("convert_xxtpt_to_hours rejects missing or empty xxtpt strings", {
  expect_error(convert_xxtpt_to_hours(NA_character_), "xxtpt must not contain NA")
  expect_error(
    convert_xxtpt_to_hours(c("1H", NA_character_)),
    "xxtpt must not contain NA"
  )
  expect_error(convert_xxtpt_to_hours(""), "xxtpt must be a non-empty string")
  expect_error(
    convert_xxtpt_to_hours(c("1H", "")),
    "xxtpt must be a non-empty string"
  )
})


test_that("convert_xxtpt_to_hours rejects invalid treatment_duration", {
  expect_error(
    convert_xxtpt_to_hours(c("EOI", "EOT"), treatment_duration = c(1, 2, 3)),
    "treatment_duration"
  )
  expect_error(
    convert_xxtpt_to_hours("EOT", treatment_duration = numeric(0)),
    "treatment_duration"
  )
  expect_error(
    convert_xxtpt_to_hours("EOT", treatment_duration = -1),
    "non-negative"
  )
  expect_error(
    convert_xxtpt_to_hours(c("EOI", "EOT"), treatment_duration = c(1, -0.1)),
    "non-negative"
  )
  expect_error(
    convert_xxtpt_to_hours("EOT", treatment_duration = NA_real_),
    "treatment_duration must not contain NA"
  )
  expect_error(
    convert_xxtpt_to_hours("EOT", treatment_duration = "1"),
    "treatment_duration must be a numeric value"
  )
})


test_that("convert_xxtpt_to_hours rejects an invalid range_method", {
  expect_error(
    convert_xxtpt_to_hours("0-6h Post-dose", range_method = "mean"),
    "range_method must be start, end or midpoint"
  )
  expect_error(
    convert_xxtpt_to_hours("0-6h Post-dose", range_method = NA_character_),
    "range_method"
  )
})


test_that("convert_xxtpt_to_hours returns numeric(0) for empty input", {
  expect_identical(convert_xxtpt_to_hours(character(0)), numeric(0))
  expect_identical(
    convert_xxtpt_to_hours(character(0), treatment_duration = 2),
    numeric(0)
  )
})


# ---- Special cases -----------------------------------------------------------

test_that("convert_xxtpt_to_hours maps screening, predose, and 0H to 0", {
  tbl <- tibble::tribble(
                    ~input, ~treatment_duration, ~range_method, ~expected,
               "Screening",                   0,    "midpoint",         0,
                "SCREENING",                  0,    "midpoint",         0,
                "Pre-dose",                   0,    "midpoint",         0,
                 "Predose",                   0,    "midpoint",         0,
                 "PREDOSE",                   0,    "midpoint",         0,
                "PRE-DOSE",                   0,    "midpoint",         0,
           "pre-treatment",                   0,    "midpoint",         0,
           "Pre-infusion",                   0,    "midpoint",         0,
                "Pre-inf",                   0,    "midpoint",         0,
                "pre-inf",                   0,    "midpoint",         0,
                  "Before",                   0,    "midpoint",         0,
               "Infusion",                   5,    "midpoint",         0,
                      "0H",                   0,    "midpoint",         0,
                    "0 hr",                   0,    "midpoint",         0,
                 "0 hours",                   0,    "midpoint",         0,
                    "0HRS",                   0,    "midpoint",         0
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours maps EOI/EOT anchors to treatment_duration", {
  tbl <- tibble::tribble(
                            ~input, ~treatment_duration, ~range_method, ~expected,
                             "EOT",                   0,    "midpoint",         0,
                             "EOT",                 2.5,    "midpoint",       2.5,
                             "EOI",                   1,    "midpoint",         1,
                 "End of Infusion",                   3,    "midpoint",         3,
                "End of Treatment",                   4,    "midpoint",         4,
         "After End of Infusion",                   1,    "midpoint",         1,
          "After End of Treatment",                   0,    "midpoint",         0
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours returns NA for non-time descriptors", {
  tbl <- tibble::tribble(
         ~input, ~treatment_duration, ~range_method, ~expected,
      "Morning",                   0,    "midpoint", NA_real_,
      "Evening",                   0,    "midpoint", NA_real_,
        "bogus",                   0,    "midpoint", NA_real_,
           "2",                   0,    "midpoint", NA_real_
  )
  eval_tbl(tbl)
})


# ---- Days, hours+minutes, simple units ---------------------------------------

test_that("convert_xxtpt_to_hours converts days that include a unit or suffix", {
  tbl <- tibble::tribble(
                    ~input, ~treatment_duration, ~range_method, ~expected,
                   "Day 1",                   0,    "midpoint",        24,
                   "DAY 1",                   0,    "midpoint",        24,
                 "Day 1.5",                   0,    "midpoint",        36,
                      "2D",                   0,    "midpoint",        48,
                    "2.5D",                   0,    "midpoint",        60,
                  "2 days",                   0,    "midpoint",        48,
                  "2 DAYS",                   0,    "midpoint",        48,
      "30 DAYS AFTER LAST",                   0,    "midpoint",       720,
              "2 POST-DOSE",                   0,    "midpoint",        48
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours converts hours-plus-minutes combinations", {
  tbl <- tibble::tribble(
                    ~input, ~treatment_duration, ~range_method, ~expected,
                   "1H30M",                   0,    "midpoint",       1.5,
                "2HR15MIN",                   0,    "midpoint",      2.25,
               "2hr 15min",                   0,    "midpoint",      2.25,
             "1H 30M POST",                   0,    "midpoint",       1.5,
         "1H30M POST-DOSE",                   0,    "midpoint",       1.5
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours converts simple hours and minutes", {
  tbl <- tibble::tribble(
                    ~input, ~treatment_duration, ~range_method, ~expected,
                      "1H",                   0,    "midpoint",         1,
                     "2.5H",                   0,    "midpoint",       2.5,
                 "2 hours",                   0,    "midpoint",         2,
             "1 HOUR POST",                   0,    "midpoint",         1,
                  "1H AFTER",                   0,    "midpoint",         1,
             "2H POSTDOSE",                   0,    "midpoint",         2,
          "4hr After-dose",                   0,    "midpoint",         4,
          "4hr after dose",                   0,    "midpoint",         4,
    "2.5 hours post-dose",                   0,    "midpoint",       2.5,
                     "30M",                   0,    "midpoint",       0.5,
                  "45 min",                   0,    "midpoint",      0.75,
             "30 MIN POST",                   0,    "midpoint",       0.5,
            "30 MIN AFTER",                   0,    "midpoint",       0.5,
                  "30M POST",                   0,    "midpoint",       0.5
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours POST from start ignores treatment_duration", {
  tbl <- tibble::tribble(
         ~input, ~treatment_duration, ~range_method, ~expected,
      "1H POST",                   5,    "midpoint",         1,
      "2H POST",                   5,    "midpoint",         2,
      "4H POST",                   5,    "midpoint",         4,
     "30M POST",                   5,    "midpoint",       0.5
  )
  eval_tbl(tbl)
})


# ---- Ranges ------------------------------------------------------------------

test_that("convert_xxtpt_to_hours converts simple ranges with start/mid/end", {
  tbl <- tibble::tribble(
             ~input, ~treatment_duration, ~range_method, ~expected,
   "0-6h Post-dose",                   0,    "midpoint",         3,
   "0-6h Post-dose",                   0,       "start",         0,
   "0-6h Post-dose",                   0,         "end",         6,
             "0-6h",                   0,    "midpoint",         3,
             "0-6H",                   0,    "midpoint",         3,
          "0 - 6 h",                   0,    "midpoint",         3
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours converts directed ranges around start of infusion", {
  tbl <- tibble::tribble(
                             ~input, ~treatment_duration, ~range_method, ~expected,
     "0-4H PRIOR START OF INFUSION",                   0,    "midpoint",        -2,
     "0-4H PRIOR START OF INFUSION",                   0,       "start",         0,
     "0-4H PRIOR START OF INFUSION",                   0,         "end",        -4,
    "0-4H BEFORE START OF TREATMENT",                  0,    "midpoint",        -2,
     "8-16H POST START OF INFUSION",                   0,    "midpoint",        12
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours adds treatment_duration to EOI/EOT ranges", {
  tbl <- tibble::tribble(
                          ~input, ~treatment_duration, ~range_method, ~expected,
                "0-4H AFTER EOI",                   1,    "midpoint",         3,
                "0-4H AFTER EOI",                   1,       "start",         1,
                "0-4H AFTER EOI",                   1,         "end",         5,
                 "0-4H POST EOI",                   1,    "midpoint",         3,
                      "0-4H EOT",                   0,    "midpoint",         2,
    "4-8H AFTER END OF INFUSION",                   1,    "midpoint",         7,
   "4-8H AFTER END OF TREATMENT",                   1,    "midpoint",         7,
            "4-8H POST INFUSION",                   1,    "midpoint",         7,
                 "4-8H POST-INF",                   1,    "midpoint",         7,
                 "4-8H POST INF",                   1,    "midpoint",         7
  )
  eval_tbl(tbl)
})


# ---- Treatment-relative single timepoints ------------------------------------

test_that("convert_xxtpt_to_hours converts predose and before times as negative", {
  tbl <- tibble::tribble(
                 ~input, ~treatment_duration, ~range_method, ~expected,
         "5 MIN PREDOSE",                   0,    "midpoint",    -5 / 60,
        "5 MIN PRE-DOSE",                   0,    "midpoint",    -5 / 60,
          "5MIN PREDOSE",                   0,    "midpoint",    -5 / 60,
     "5 MINUTES PREDOSE",                   0,    "midpoint",    -5 / 60,
          "5 MIN BEFORE",                   0,    "midpoint",    -5 / 60,
         "1 HOUR BEFORE",                   0,    "midpoint",         -1,
            "1 HR BEFORE",                   0,    "midpoint",         -1,
             "1H BEFORE",                   0,    "midpoint",         -1
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours adds treatment_duration after EOI/EOT/infusion", {
  tbl <- tibble::tribble(
                           ~input, ~treatment_duration, ~range_method, ~expected,
                 "1 HOUR POST EOI",                   2,    "midpoint",         3,
                "1 HOUR AFTER EOT",                   2,    "midpoint",         3,
                  "24 HR POST INF",                   1,    "midpoint",        25,
                 "24 HR POST-INF",                   2,    "midpoint",        26,
           "1 HOUR POST INFUSION",                   2,    "midpoint",         3,
          "1 HOUR POST-INFUSION",                   2,    "midpoint",         3,
   "30MIN AFTER END OF INFUSION",                   1,    "midpoint",       1.5,
    "30 MIN AFTER END OF INFUSION",                  1,    "midpoint",       1.5,
             "30 MIN AFTER EOT",                   2,    "midpoint",       2.5
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours converts times relative to start of infusion", {
  tbl <- tibble::tribble(
                               ~input, ~treatment_duration, ~range_method, ~expected,
          "8H PRIOR START OF INFUSION",                   0,    "midpoint",        -8,
         "8 H PRIOR START OF INFUSION",                   0,    "midpoint",        -8,
        "8H BEFORE START OF TREATMENT",                   0,    "midpoint",        -8,
      "8 H BEFORE START OF TREATMENT",                   0,    "midpoint",        -8,
           "8H POST START OF INFUSION",                   0,    "midpoint",         8,
         "8H AFTER START OF TREATMENT",                   0,    "midpoint",         8
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours converts minutes after start of infusion", {
  tbl <- tibble::tribble(
                                 ~input, ~treatment_duration, ~range_method, ~expected,
        "60 MIN AFTER START OF INFUSION",                   0,    "midpoint",         1,
         "60MIN AFTER START OF INFUSION",                   0,    "midpoint",         1,
            "60MIN AFTER START INFUSION",                   0,    "midpoint",         1,
       "60 MIN AFTER START OF TREATMENT",                   0,    "midpoint",         1
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours does not match START INF without of/infusion", {
  expect_identical(
    convert_xxtpt_to_hours("60 MIN AFTER START INF"),
    NA_real_
  )
  expect_identical(
    convert_xxtpt_to_hours("60 MIN AFTER START OF INF"),
    NA_real_
  )
})


test_that("convert_xxtpt_to_hours subtracts minutes before EOI/EOT from duration", {
  tbl <- tibble::tribble(
               ~input, ~treatment_duration, ~range_method, ~expected,
       "10MIN PRE EOI",                   2,    "midpoint",  2 - 10 / 60,
      "10 MIN PRE EOI",                   2,    "midpoint",  2 - 10 / 60,
      "10MIN PRE EOT",                   2,    "midpoint",  2 - 10 / 60,
    "10MIN BEFORE EOT",                   2,    "midpoint",  2 - 10 / 60
  )
  eval_tbl(tbl)
})


# ---- Vectorization and cleanup -----------------------------------------------

test_that("convert_xxtpt_to_hours recycles a scalar treatment_duration", {
  out <- convert_xxtpt_to_hours(c("EOI", "1 HOUR POST EOI"), treatment_duration = 2)
  expect_equal(out, c(2, 3))
})


test_that("convert_xxtpt_to_hours uses a per-row treatment_duration vector", {
  out <- convert_xxtpt_to_hours(
    c("EOI", "1 HOUR POST EOI", "EOI", "1 HOUR POST EOI"),
    treatment_duration = c(1, 1, 2, 2)
  )
  expect_equal(out, c(1, 2, 2, 3))
})


test_that("convert_xxtpt_to_hours is vectorized and preserves length and order", {
  x <- c(
    "Screening",
    "1H",
    "unrecognized",
    "  30M  ",
    "EOI",
    "5 MIN PREDOSE"
  )
  out <- convert_xxtpt_to_hours(x, treatment_duration = 2)

  expect_length(out, length(x))
  expect_type(out, "double")
  expect_equal(out, c(0, 1, NA, 0.5, 2, -5 / 60), tolerance = 1e-8)
})


test_that("convert_xxtpt_to_hours trims surrounding whitespace", {
  expect_equal(convert_xxtpt_to_hours("  1H  "), 1)
  expect_equal(convert_xxtpt_to_hours("\tEOI\n", treatment_duration = 3), 3)
})
