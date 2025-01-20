
test_that("is_col_nonmem_numeric works correctly", {
  expect_true(is_col_nonmem_numeric(
    c("1", "1.2", "-2.4", "+7", ".", "")))
  expect_true(is_col_nonmem_numeric(
    c("3.1e-3", "3.1e-03", "2e3", "2e03", "5e+6", "5e+06")))

  expect_false(is_col_nonmem_numeric("1.1.2"))
  expect_false(is_col_nonmem_numeric("1-"))
  expect_false(is_col_nonmem_numeric("1.3-"))
  expect_false(is_col_nonmem_numeric("1.3+"))
})


test_that("is_char_datetime works correctly", {
  expect_true(all(is_char_datetime(
    c("2009-12-02T11:45", "2009-12-02T12:00:10", "2025-01-17T08:00")
  )))
  expect_false(is_char_datetime("2009-12-02"))
  expect_false(is_char_datetime("2009-12-02T"))
  expect_false(is_char_datetime("2009-12-02 8:00"))
  expect_false(is_char_datetime("2025-01-17T8:00"))
})


test_that("col_convert_numeric works correctly", {
  test <- tibble::tribble(
    ~USUBJID,     ~A,  ~B,  ~C,  ~D,
       "001",    ".", "x", "x", "x",
       "001",    "1", "x", "x", "x",
       "001",  "2.2", "x", "x", "x",
       "001", "1e-3",  NA,  NA,  NA,
       "001",    "4",  NA,  NA,  NA,
       "001",    "5",  NA,  NA,  NA,
       "001",    "6",  NA,  NA,  NA,
       "001",   "+7",  NA,  NA,  NA,
       "001",   "-8",  NA,  NA,  NA,
       "001",    "9",  NA,  NA,  NA,
       "001",    "A",  NA,  NA,  NA
  )

  expectation <- tibble::tribble(
    ~USUBJID,    ~A,  ~B,  ~C,  ~D,
       "001",    NA, "x", "x", "x",
       "001",     1, "x", "x", "x",
       "001",   2.2, "x", "x", "x",
       "001", 0.001,  NA,  NA,  NA,
       "001",     4,  NA,  NA,  NA,
       "001",     5,  NA,  NA,  NA,
       "001",     6,  NA,  NA,  NA,
       "001",     7,  NA,  NA,  NA,
       "001",    -8,  NA,  NA,  NA,
       "001",     9,  NA,  NA,  NA,
       "001",    NA,  NA,  NA,  NA
  )

  expect_equal(
    convert_all_numeric(test, silent=TRUE),
    expectation)
})


test_that("col_convert_datetime works correctly", {
  test <- c(
    "2025-01-20 08:04:43",
    "2025-01-20 08:04",
    "2025-01-20T08:04:43",
    "2025-01-20T08:04",
    "25-01-20 08:04",
    "2025-01-20",
    "A"
  )

  convert_char_datetime(test, silent = T)

})


test_that("import from connection works", {
  test <- r"(
  USUBJID  TIME EVID DV RACE
  # comment line
  0001     0    0    .  WHITE
  0001     1    0    1. ASIAN

  0001     2    0    2  WHITE
  )"

  connection <- textConnection(test, open="r")
  temp <- import_from_connection(connection, format = "NONMEM")
})


