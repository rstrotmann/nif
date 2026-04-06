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

# ---- Documented special cases and simple units --------------------------------

test_that("convert_xxtpt_to_hours: screening, pre-dose, EOI/EOT anchors", {
  tbl <- tibble::tribble(
                    ~input, ~treatment_duration, ~range_method, ~expected,
               "Screening",                   0,    "midpoint",         0,
                "Pre-dose",                   0,    "midpoint",         0,
           "pre-treatment",                   0,    "midpoint",         0,
                  "Before",                   0,    "midpoint",         0,
                      "0H",                   0,    "midpoint",         0,
                     "EOT",                   0,    "midpoint",         0,
                     "EOT",                 2.5,    "midpoint",       2.5,
                     "EOI",                   1,    "midpoint",         1,
         "End of Infusion",                   3,    "midpoint",         3,
  "After End of Treatment",                   0,    "midpoint",         0,
                 "Morning",                   0,    "midpoint",        NA,
                 "Evening",                   0,    "midpoint",        NA
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours: hours, minutes, day 1, combined H+M", {
  tbl <- tibble::tribble(
                    ~input, ~treatment_duration, ~range_method, ~expected,
                      "1H",                   0,    "midpoint",         1,
                     "30M",                   0,    "midpoint",       0.5,
                 "2 hours",                   0,    "midpoint",         2,
                   "Day 1",                   0,    "midpoint",        24,
                      "2D",                   0,    "midpoint",        48,
                   "1H30M",                   0,    "midpoint",       1.5,
             "1 HOUR POST",                   0,    "midpoint",         1,
             "30 MIN POST",                   0,    "midpoint",       0.5,
             "2H POSTDOSE",                   0,    "midpoint",         2,
      "30 DAYS AFTER LAST",                   0,    "midpoint",       720,
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours: empty character() returns numeric(0)", {
  expect_equal(convert_xxtpt_to_hours(character(0)), numeric(0))
})


test_that("convert_xxtpt_to_hours: rejects invalid treatment_duration length", {
  expect_error(
    convert_xxtpt_to_hours(c("a", "b"), treatment_duration = c(1, 2, 3)),
    "treatment_duration"
  )
})


test_that("convert_xxtpt_to_hours: rejects negative treatment_duration", {
  expect_error(convert_xxtpt_to_hours("EOT", treatment_duration = -1), "non-negative")
})


test_that("convert_xxtpt_to_hours: rejects invalid range_method", {
  expect_error(
    convert_xxtpt_to_hours("0-6h Post-dose", range_method = "mean"),
    "range_method must be"
  )
})

# ---- Ranges -------------------------------------------------------------------

test_that("convert_xxtpt_to_hours: simple and directed ranges (midpoint)", {
  tbl <- tibble::tribble(
                          ~input, ~treatment_duration, ~range_method, ~expected,
                "0-6h Post-dose",                   0,    "midpoint",         3,
                "0-6h Post-dose",                   0,       "start",         0,
                "0-6h Post-dose",                   0,         "end",         6,
  "0-4H PRIOR START OF INFUSION",                   0,    "midpoint",        -2,
  "8-16H POST START OF INFUSION",                   0,    "midpoint",        12
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours: EOI/EOT ranges add treatment_duration", {
  tbl <- tibble::tribble(
    ~input,                         ~treatment_duration, ~range_method, ~expected,
    "0-4H AFTER EOI",               1,                   "midpoint",    3,
    "0-4H POST EOI",                1,                   "midpoint",    3,
    "4-8H AFTER END OF INFUSION",   1,                   "midpoint",    7,
    "4-8H POST INFUSION",           1,                   "midpoint",    7,
    "4-8H POST-INF",                1,                   "midpoint",    7,
    "0-4H EOT",                     0,                   "midpoint",    2
  )
  eval_tbl(tbl)
})


# ---- Treatment-relative single timepoints ------------------------------------

test_that("convert_xxtpt_to_hours: post-end and pre-end relative to infusion", {
  tbl <- tibble::tribble(
    ~input,                         ~treatment_duration, ~range_method, ~expected,
    "1 HOUR POST EOI",              2,                   "midpoint",    3,
    "24 HR POST INF",               1,                   "midpoint",    25,
    "30MIN AFTER END OF INFUSION",  1,                   "midpoint",    1.5,
    "8H PRIOR START OF INFUSION",   0,                   "midpoint",    -8,
    "10MIN PRE EOI",                2,                   "midpoint",    2 - 10 / 60
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours: predose negative hours/minutes", {
  tbl <- tibble::tribble(
    ~input,           ~treatment_duration, ~range_method, ~expected,
    "5 MIN PREDOSE",  0,                   "midpoint",    -5 / 60,
    "1 HOUR BEFORE",  0,                   "midpoint",    -1
  )
  eval_tbl(tbl)
})


test_that("convert_xxtpt_to_hours: vectorized treatment_duration", {
  out <- convert_xxtpt_to_hours(
    c("EOI", "1 HOUR POST EOI", "EOI", "1 HOUR POST EOI"),
    treatment_duration = c(1, 1, 2, 2)
  )
  expect_equal(out, c(1, 2, 2, 3))
})


test_that("convert_xxtpt_to_hours: POST from start does not add treatment_duration", {
  tbl <- tibble::tribble(
    ~input,    ~treatment_duration, ~range_method, ~expected,
    "1H POST", 5,                   "midpoint",    1,
    "2H POST", 5,                   "midpoint",    2,
    "4H POST", 5,                   "midpoint",    4
  )
  eval_tbl(tbl)
})


# ---- KNOWN ISSUES (desired behavior vs current implementation) ---------------

test_that("KNOWN ISSUE: space between number and H in PRIOR START OF INFUSION (docs: flexible whitespace)", {
  # Documentation states flexible whitespace; pattern currently requires `8H` not `8 H`.
  tbl <- tibble::tribble(
    ~input,                          ~treatment_duration, ~range_method, ~expected,
    "8 H PRIOR START OF INFUSION",   0,                   "midpoint",    -8,
    "8H PRIOR START OF INFUSION",    0,                   "midpoint",    -8,
    "8 H BEFORE START OF TREATMENT", 0,                   "midpoint",    -8
  )
  eval_tbl(tbl)
})


test_that("KNOWN ISSUE: MIN AFTER START OF INFUSION (full phrase, not only START INF)", {
  # Docs mention start-of-infusion patterns; only `after start inf` matches today.
  tbl <- tibble::tribble(
    ~input,                              ~treatment_duration, ~range_method, ~expected,
    "60 MIN AFTER START OF INFUSION",   0,                   "midpoint",    1,
    "60MIN AFTER START OF INFUSION",    0,                   "midpoint",    1,
    "30MIN AFTER END OF INFUSION",      1,                   "midpoint",    1.5,
    "30 MIN AFTER END OF INFUSION",     1,                   "midpoint",    1.5,
    "60 MIN AFTER START OF TREATMENT",  0,                   "midpoint",    1
  )
  eval_tbl(tbl)
})


