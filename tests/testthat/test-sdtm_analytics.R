test_that("sdtm_missing_times works", {
  temp <- sdtm_missing_times(examplinib_poc, c("EXSTDTC", "EXENDTC"))
  expect_equal(nrow(temp), 2)
})
