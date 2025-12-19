test_that("nca() input validation", {
  # Test that non-nif objects are rejected
  expect_error(nca(data.frame()), "Input must be a NIF object")

  # Test that missing required columns are caught
  test_nif <- structure(
    tibble::tribble(
      ~ID,
      1, 2, 3
    ),
    class = c("nif", "data.frame")
  )
  expect_error(nca(test_nif), "Missing required columns: TIME, DV, EVID, ANALYTE")
})


test_that("nca() analyte handling", {
  # Create a minimal valid nif object
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DOSE,
      1, 0, 0, 1, "DRUG", 100,
      1, 0, 10, 0, "DRUG", 100,
      1, 2, 5, 0, "DRUG", 100,
      1, 3, 2, 0, "DRUG", 100,
      2, 0, 0, 1, "DRUG", 100,
      2, 0, 12, 0, "DRUG", 100,
      2, 2, 6, 0, "DRUG", 100,
      2, 3, 3, 0, "DRUG", 100,
      3, 0, 0, 1, "DRUG", 100,
      3, 0, 8, 0, "DRUG", 100,
      3, 2, 4, 0, "DRUG", 100,
      3, 3, 1, 0, "DRUG", 100
    ),
    class = c("nif", "data.frame")
  )

  # Test automatic analyte selection
  expect_no_error(
    capture_warning(
      result <- nca(test_nif, silent = TRUE)
    )
  )

  # Test explicit analyte selection
  expect_no_error(
    capture_warning(
      result <- nca(test_nif, analyte = "DRUG", silent = TRUE)
    )
  )
})


test_that("nca() grouping functionality", {
  # Create a nif object with grouping variable
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DOSE, ~GROUP,
      1, 0, 0, 1, "DRUG", 100, "A",
      1, 0, 0, 0, "DRUG", 100, "A",
      1, 1, 10, 0, "DRUG", 100, "A",
      1, 2, 5, 0, "DRUG", 100, "A",
      1, 3, 2, 0, "DRUG", 100, "A",
      2, 0, 0, 1, "DRUG", 100, "B",
      2, 0, 0, 0, "DRUG", 100, "B",
      2, 1, 12, 0, "DRUG", 100, "B",
      2, 2, 6, 0, "DRUG", 100, "B",
      2, 3, 3, 0, "DRUG", 100, "B",
      3, 0, 0, 1, "DRUG", 100, "A",
      3, 0, 0, 0, "DRUG", 100, "A",
      3, 1, 8, 0, "DRUG", 100, "A",
      3, 2, 4, 0, "DRUG", 100, "A",
      3, 3, 1, 0, "DRUG", 100, "A"
    ),
    class = c("nif", "data.frame")
  )

  # Test grouping
  expect_no_error(
    expect_warning(expect_warning(expect_warning(
      result <- nca(test_nif, group = "GROUP", silent = TRUE)
    )))
  )

  expect_true("GROUP" %in% names(result))
  expect_equal(length(unique(result$GROUP)), 2)
})


test_that("nca() time handling", {
  # Create a nif object with both TIME and NTIME
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~NTIME, ~DV, ~EVID, ~ANALYTE, ~DOSE,
      1, 0, 0, 0, 1, "DRUG", 100,
      1, 1, 1.1, 10, 0, "DRUG", 100,
      1, 2, 2.2, 5, 0, "DRUG", 100,
      1, 3, 3.3, 2, 0, "DRUG", 100,
      2, 0, 0, 0, 1, "DRUG", 100,
      2, 1, 1.1, 12, 0, "DRUG", 100,
      2, 2, 2.2, 6, 0, "DRUG", 100,
      2, 3, 3.3, 3, 0, "DRUG", 100,
      3, 0, 0, 0, 1, "DRUG", 100,
      3, 1, 1.1, 8, 0, "DRUG", 100,
      3, 2, 2.2, 4, 0, "DRUG", 100,
      3, 3, 3.3, 1, 0, "DRUG", 100
    ),
    class = c("nif", "data.frame")
  )

  # Test nominal time usage
  expect_no_error(
    capture_warning(
      result_nominal <- nca(test_nif, nominal_time = TRUE, silent = TRUE)
    )
  )

  expect_no_error(
    capture_warning(
      result_actual <- nca(test_nif, nominal_time = FALSE, silent = TRUE)
    )
  )

  # Results should be different when using different time scales
  # expect_false(identical(result_nominal, result_actual))
})


test_that("nca() duplicate handling", {
  # Create a nif object with duplicate time points
  test_nif <- structure(
    tibble::tribble(
      ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DOSE,
      1, 0, 0, 1, "DRUG", 100,
      1, 1, 10, 0, "DRUG", 100,
      1, 1, 11, 0, "DRUG", 100,
      1, 2, 5, 0, "DRUG", 100,
      1, 2, 6, 0, "DRUG", 100,
      1, 3, 2, 0, "DRUG", 100,
      2, 0, 0, 1, "DRUG", 100,
      2, 1, 12, 0, "DRUG", 100,
      2, 1, 13, 0, "DRUG", 100,
      2, 2, 6, 0, "DRUG", 100,
      2, 2, 7, 0, "DRUG", 100,
      2, 3, 3, 0, "DRUG", 100
    ),
    class = c("nif", "data.frame")
  )

  # Test with and without averaging duplicates
  testthat::capture_warnings({
    expect_no_error(
      result_avg <- nca(test_nif, average_duplicates = TRUE, silent = TRUE)
    )
  })

  expect_error(
    result_no_avg <- nca(test_nif, average_duplicates = FALSE, silent = TRUE),
    "Rows that are not unique per group and time"
  )
})


# test_that("nca() basic calculations", {
#   # Create a simple nif object for basic NCA calculations
#   test_nif <- structure(
#     tibble::tribble(
#       ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DOSE,
#       1,     0,   0,     1,   "DRUG",   100,
#       1,     1,  10,     0,   "DRUG",   100,
#       1,     2,   5,     0,   "DRUG",   100,
#       1,     3,   2,     0,   "DRUG",   100,
#       2,     0,   0,     1,   "DRUG",   100,
#       2,     1,  12,     0,   "DRUG",   100,
#       2,     2,   6,     0,   "DRUG",   100,
#       2,     3,   3,     0,   "DRUG",   100
#     ),
#     class = c("nif", "data.frame")
#   )
#
#   # Test basic NCA calculations
#   result <- nca(test_nif, silent = TRUE)
#
#   # Check that expected NCA parameters are present
#   expected_params <- c("auclast", "cmax", "tmax", "half.life", "aucinf.obs")
#   expect_true(all(expected_params %in% result$PPTESTCD))
#
#   # Check that results are numeric and not NA
#   expect_true(all(!is.na(result$PPORRES)))
#   expect_true(is.numeric(result$PPORRES))
# })


test_that("nca works with the whale data set", {
  # Whale data copied from https://github.com/billdenney/pknca
  # Original source: https://doi.org/10.1638/03-078
  conc <- read.csv(test_path("fixtures", "whale_conc.csv"))
  dose <- read.csv(test_path("fixtures", "whale_dose.csv"))

  expected_nca <- PKNCA::pk.nca(
    PKNCA::PKNCAdata(
      PKNCA::PKNCAconc(conc, concentration ~ time | Animal),
      PKNCA::PKNCAdose(dose, dose ~ time | Animal) # ,
      # impute = "start_conc0"
    )
  )$result %>%
    arrange(Animal, PPTESTCD)

  whale_nif <- structure(
    bind_rows(
      select(conc, ID = Animal, TIME = time, DV = concentration) %>%
        mutate(EVID = 0, ANALYTE = "amikacin", PARENT = "amikacin") %>%
        mutate(CMT = 0),
      select(dose, ID = Animal, TIME = time, DOSE = dose) %>%
        mutate(EVID = 1, ANALYTE = "amikacin", PARENT = "amikacin") %>%
        mutate(AMT = DOSE, CMT = 1)
    ) %>%
      mutate(TAFD = TIME) %>%
      arrange(ID, TIME, -EVID),
    class = c("nif", "data.frame")
  )

  nca <- nca(whale_nif, silent = TRUE) %>%
    arrange(ID, PPTESTCD)

  expect_equal(nca$PPORRES, expected_nca$PPORRES)
})
