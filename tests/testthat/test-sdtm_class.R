test_that("guess_ntime works", {
  sdtm = list()
  sdtm$pc <- tribble(
    ~PCTPT, ~time,
    "0.5h Pre Dose", -0.5,
    "6h Post Dose", 6,
    "PRE-DOSE", 0,
    "4H POST-DOSE", 4,
    "END OF VISIT", NA,
    "DAY1 - 4 HOURS POST ADMINISTRATION", 4,
    "PREDOSE", 0,
    "8 HOUR POST DOSE", 8,
    "2-4 HOUR POST DOSE", 4, ### caution: only the right end of the interval
    "12.0 HRS POST DOSE", 12,
    "PRE DOSE", 0
  )

  expect_equal(sdtm$pc$time, guess_ntime(sdtm)$NTIME)
})


test_that("new_sdtm", {
  temp <- examplinib_sad
  expect_no_error(
    new_sdtm(list(
      dm = domain(temp, "dm"),
      vs = domain(temp, "vs"),
      lb = domain(temp, "lb"),
      ex = domain(temp, "ex"),
      pc = domain(temp, "pc")
    ))
  )
})


test_that("sdtm summary", {
  expect_no_error(
    summary(examplinib_sad)
  )
})
