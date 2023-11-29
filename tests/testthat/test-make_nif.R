
test_that("make_obs generally works", {
  sdtm <- examplinib_fe
  pc <- sdtm$pc

  obs <- make_obs(pc, time_mapping=sdtm$time_mapping,
           spec="PLASMA", silent=T, use_pctptnum=T)
  expect_gt(nrow(obs), 0)
})

# test_that("make_admin generally works", {
#   admin <- make_admin(
#     ex,
#     drug_mapping = drug_mapping,
#     cut.off.date,
#     impute.missing.end.time=impute.missing.end.time,
#     silent=silent)
# })

test_that("date conversion works correctly", {
  test <- data.frame(
    RFICDTC=c("2000-12-20T11:11", "2000-12-22T10:09:53"),
    RFSTDTC=c("2001-01-01", "2000-12-30T09:31:19"),
    LABEL= c("A", "B")
  )
  expect_no_error(isofy_dates(lubrify_dates(test)))
})
