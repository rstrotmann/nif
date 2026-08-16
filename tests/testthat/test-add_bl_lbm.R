## Tests for add_bl_lbm
##
## Adds BL_LBM from WEIGHT, HEIGHT, and SEX using the chosen LBM method.


test_that("add_bl_lbm adds BL_LBM with the default Boer method", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~HEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,      70,     170,    0,
      1,     1,    0,    2,     0,  10,      70,     170,    0
  ) |>
    nif()

  result <- add_bl_lbm(nif_obj)

  expect_s3_class(result, "nif")
  expect_true("BL_LBM" %in% names(result))
  expect_equal(result$BL_LBM, rep(lbm_boer(70, 170, 0), 2))
})


test_that("add_bl_lbm works with character SEX codes", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~HEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,      70,     170,  "M",
      2,     0,  100,    1,     1,  NA,      60,     165,  "F"
  ) |>
    nif()

  result <- add_bl_lbm(nif_obj)

  expect_equal(result$BL_LBM[result$ID == 1], lbm_boer(70, 170, "M"))
  expect_equal(result$BL_LBM[result$ID == 2], lbm_boer(60, 165, "F"))
})


test_that("add_bl_lbm accepts lbm_hume as method", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~HEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,      70,     170,    0
  ) |>
    nif()

  result <- add_bl_lbm(nif_obj, method = lbm_hume)

  expect_equal(result$BL_LBM, lbm_hume(70, 170, 0))
  expect_false(isTRUE(all.equal(result$BL_LBM, lbm_boer(70, 170, 0))))
})


test_that("add_bl_lbm accepts lbm_peters as method", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~HEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,      70,     170,    0
  ) |>
    nif()

  result <- add_bl_lbm(nif_obj, method = lbm_peters)

  expect_equal(result$BL_LBM, lbm_peters(70, 170, 0))
})


test_that("add_bl_lbm keeps BL_LBM constant within a subject", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~HEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,      80,     180,    0,
      1,     1,    0,    2,     0,  10,      80,     180,    0,
      1,    24,  100,    1,     1,  NA,      80,     180,    0,
      2,     0,  100,    1,     1,  NA,      65,     170,    1,
      2,     1,    0,    2,     0,  20,      65,     170,    1
  ) |>
    nif()

  result <- add_bl_lbm(nif_obj)

  expect_equal(length(unique(result$BL_LBM[result$ID == 1])), 1)
  expect_equal(length(unique(result$BL_LBM[result$ID == 2])), 1)
  expect_true(result$BL_LBM[result$ID == 1][1] != result$BL_LBM[result$ID == 2][1])
})


test_that("add_bl_lbm propagates NA covariates as NA LBM", {
  nif_obj <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~HEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,      NA,     170,    0,
      2,     0,  100,    1,     1,  NA,      70,      NA,    0,
      3,     0,  100,    1,     1,  NA,      70,     170,   NA
  ) |>
    nif()

  result <- add_bl_lbm(nif_obj)

  expect_true(all(is.na(result$BL_LBM)))
})


test_that("add_bl_lbm requires WEIGHT, HEIGHT, and SEX", {
  missing_weight <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~HEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,     170,    0
  ) |>
    nif()

  missing_height <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~SEX,
      1,     0,  100,    1,     1,  NA,      70,    0
  ) |>
    nif()

  missing_sex <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~WEIGHT, ~HEIGHT,
      1,     0,  100,    1,     1,  NA,      70,     170
  ) |>
    nif()

  expect_error(add_bl_lbm(missing_weight), "Missing required fields: WEIGHT")
  expect_error(add_bl_lbm(missing_height), "Missing required fields: HEIGHT")
  expect_error(add_bl_lbm(missing_sex), "Missing required fields: SEX")
})


test_that("add_bl_lbm validates that input is a nif object", {
  expect_error(
    add_bl_lbm(data.frame(ID = 1, WEIGHT = 70, HEIGHT = 170, SEX = 0)),
    "nif object"
  )
})


test_that("add_bl_lbm works with examplinib data when covariates are present", {
  skip_if_not(all(c("WEIGHT", "HEIGHT", "SEX") %in% names(examplinib_poc_nif)))

  result <- add_bl_lbm(examplinib_poc_nif)

  expect_s3_class(result, "nif")
  expect_true("BL_LBM" %in% names(result))
  expect_false(all(is.na(result$BL_LBM)))
})
