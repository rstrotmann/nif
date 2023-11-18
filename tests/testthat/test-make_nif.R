
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
