test_that("make_ntime_from_tpt works with generic PCTPT", {
  test <- tibble::tribble(
                  ~PCTPT, ~NTIME,
                      NA,     NA,
              "PRE-DOSE",      0,
        "0.5H POST-DOSE",    0.5,
          "1H POST-DOSE",      1,
          "2H POST-DOSE",      2,
          "3H POST-DOSE",      3,
         "12H POST-DOSE",     12,
         "10H POST-DOSE",     10,
  "0.5H TO 2H POST-DOSE",      2,
          "END OF VISIT",     NA,
     "0 TO 4H POST-DOSE",      4,
   "8H TO 24H POST-DOSE",     24
  ) %>% mutate(DOMAIN = "PC")

  expect_equal(
    make_ntime_from_tpt(test)$NTIME,
    test$NTIME
  )
})


test_that("make_ntime_from_tpt works with irregular hour unit", {
  test <- tibble::tribble(
    ~PCTPT, ~NTIME,
    "PRE DOSE",             0,
    "1.0 HR POST DOSE",     1,
    "1.5 HRS POST DOSE",    1.5,
    "2.0 HRS POST DOSE",    2,
    "8.0 HRS POST DOSE",    8,
    "12.0 HRS POST DOSE",   12,
    "96.0 HRS POST DOSE",   96,
    "168.0 HRS POST DOSE",  168,
    "PREDOSE",              0,
    "0.5 HOUR POST DOSE",   0.5,
    "1 HOUR POST DOSE",     1,
    "2 HOUR POST DOSE",     2,
    "3 HOUR POST DOSE",     3,
    "4 HOUR POST DOSE",     4,
    "6 HOUR POST DOSE",     6,
    "8 HOUR POST DOSE",     8,
    "2-4 HOUR POST DOSE",   4
  ) %>% mutate(DOMAIN = "PC")

  expect_equal(
    make_ntime_from_tpt(test)$NTIME,
    test$NTIME
  )
})


test_that("make_ntime_from_tpt works with minutes", {
  test <- tibble::tribble(
    ~PCTPT, ~NTIME,
    "PRE DOSE",             0,
    "15 MIN POST DOSE",     15/60,
    "30 MIN POST DOSE",     30/60,
    "1.0 HR POST DOSE",     1,
    "1.5 HRS POST DOSE",    1.5,
    "2.0 HRS POST DOSE",    2,
    "2.5 HRS POST DOSE",    2.5,
    "3.0 HRS POST DOSE",    3,
    "8.0 HRS POST DOSE",    8,
    "24.0 HRS POST DOSE",   24
  ) %>% mutate(DOMAIN = "PC")

  expect_equal(
    make_ntime_from_tpt(test)$NTIME,
    test$NTIME
  )
})


test_that("make_ntime_from_tpt works with day information", {
  test <- tibble::tribble(
    ~PCTPT, ~NTIME,
    NA,     NA,
    "PREDOSE",     0,
    "DAY1 - 1 HOUR POST ADMINISTRATION",       1,
    "DAY1 - 2 HOURS POST ADMINISTRATION",      2,
    "DAY1 - 16 HOURS POST ADMINISTRATION",     16,
    "DAY2 - 24 HOURS POST ADMINISTRATION",     24,
    "DAY3 - 48 HOURS POST ADMINISTRATION",     48,
    "DAY11 - 240 HOURS POST ADMINISTRATION",   240,
    "DAY44 - 1032 HOURS POST ADMINISTRATION",  1032,
    "DAY1 - 0.5 HOUR POST ADMINISTRATION",     0.5,
    "DAY1 - 1.5 HOURS POST ADMINISTRATION",    1.5,
  ) %>% mutate(DOMAIN = "PC")

  expect_equal(
    make_ntime_from_tpt(test)$NTIME,
    test$NTIME
  )
})








