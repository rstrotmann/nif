test_that("make_administration works for examplinib_poc", {
  expect_no_error(
    make_administration(
      examplinib_poc, "EXAMPLINIB", "RS2023", silent = TRUE)
  )
})


# test_that("make_administration uses correct time imputations", {
#   sdtm <- new_sdtm(list(
#     dm = tibble::tribble(
#       ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,  ~STUDYID,
#       1,    1, "2024-12-16T7:50", "2024-12-19",   "ARM A", "STUDY 1",
#       2,    1, "2024-12-16T7:50", "2024-12-18",   "ARM A", "STUDY 1",
#       3,    1, "2024-12-16T7:50", "2024-12-17",   "ARM A", "STUDY 1"
#     ),
#     ex = tibble::tribble(
#       ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,     ~EXENDTC, ~EXDOSE,
#       1,      1,    "A", "2024-12-16T7:50", "2024-12-19",     100,
#       2,      2,    "A", "2024-12-16T7:50", "2024-12-18",     100,
#       3,      3,    "A", "2024-12-16T7:50", "2024-12-17",     100,
#       3,      4,    "A", "2024-12-20",      "2024-12-22",     100
#     ),
#     pc = tibble::tribble(
#       ~USUBJID, ~PCTESTCD,        ~PCRFTDTC,
#       1,       "A", "2024-12-19T8:10",
#       3,       "A", "2024-12-21T10:00",
#       3,       "A", "2024-12-22"
#     )
#   ))
#
#   expect_no_error(
#     test <- as.data.frame(make_administration(sdtm, "A"))
#   )
#
#   # number of administrations is correct:
#   expect_equal(
#     reframe(test, n = n_distinct(DTC), .by = "USUBJID")$n,
#     c(4, 3, 5)
#   )
#
#   # check time carry forward
#   expect_equal(
#     test$IMPUTATION,
#     c("",
#       "time carried forward",
#       "time carried forward",
#       "admin time copied from PCRFTDTC",
#       "",
#       "time carried forward",
#       "time carried forward",
#       "",
#       "time carried forward",
#       "time carried forward",
#       "admin time copied from PCRFTDTC",
#       "time carried forward")
#   )
# })


test_that("make_administration works without pc", {
  sdtm <- new_sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
      1,    1, "2024-12-16T7:50", "2024-12-19",   "ARM A",
      2,    1, "2024-12-16T7:50", "2024-12-18",   "ARM A",
      3,    1, "2024-12-16T7:50", "2024-12-17",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,     ~EXENDTC, ~EXDOSE,
      1,      1,    "A", "2024-12-16T7:50", "2024-12-19",     100,
      2,      2,    "A", "2024-12-16T7:50", "2024-12-18",     100,
      3,      3,    "A", "2024-12-16T7:50", "2024-12-17",     100,
      3,      4,    "A", "2024-12-20",      "2024-12-22",     100
    )
  ))

  expect_no_error(
    test <- as.data.frame(make_administration(sdtm, "A", silent = TRUE))
  )

  # number of administrations is correct:
  expect_equal(
    reframe(test, n = n_distinct(DTC), .by = "USUBJID")$n,
    c(4, 3, 5)
  )

  # check time carry forward
  expect_equal(
    test$IMPUTATION,
    c("",
      "time carried forward",
      "time carried forward",
      "time carried forward",
      "",
      "time carried forward",
      "time carried forward",
      "",
      "time carried forward",
      "time carried forward",
      "time carried forward",
      "time carried forward")
  )
})


test_that("make_administration imputes missing last EXENDTC", {
  sdtm <- new_sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
      1,    1, "2024-12-16T7:50", "2024-12-19",   "ARM A",
      2,    1, "2024-12-16T7:50", "2024-12-18",   "ARM A",
      3,    1, "2024-12-16T7:50", "2024-12-22",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,     ~EXENDTC, ~EXDOSE,
      1,      1,    "A", "2024-12-16T7:50", "2024-12-19",     100,
      2,      2,    "A", "2024-12-16T7:50", "2024-12-18",     100,
      3,      3,    "A", "2024-12-16T7:50", "2024-12-17",     100,
      3,      4,    "A",      "2024-12-20",           "",     100
    )
  ))


  expect_no_error(
    expect_message(
      expect_message(
        test <- as.data.frame(
          make_administration(sdtm, "A", silent = FALSE)),
      "A global cut-off-date of 2024-12-22 was automatically assigned!")
    ))
})

