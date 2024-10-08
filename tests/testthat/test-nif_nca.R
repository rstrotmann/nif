test_that("nca_from_pp", {
  nif <- examplinib_sad_nif
  sdtm <- examplinib_sad

  expect_no_error(
    temp <- nif %>%
      nca_from_pp(sdtm, analyte = "RS2023")
  )

  expect_equal(
    length(unique(temp$USUBJID)),
    length(unique(nif$USUBJID)))
})
