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


# test_that("nca1", {
#   expect_no_error(
#     temp <- nca1(examplinib_sad_nif, time = "TAD")
#   )
#
#   temp %>%
#     filter(is.na(exclude)) %>%
#     filter(PPTESTCD == "aucinf.obs") %>%
#     ggplot2::ggplot(ggplot2::aes(x = DOSE, y = PPORRES)) +
#     ggplot2::geom_point() +
#     ggplot2::geom_smooth(method = "lm", formula = y ~ x + 0) +
#     ggplot2::theme_bw()
# })
#
#
# test_that("nca1", {
#   expect_no_error(
#     temp <- nca1(examplinib_fe_nif, time = "TAD", group = "FASTED")
#   )
#
#   temp %>%
#     filter(is.na(exclude)) %>%
#     filter(PPTESTCD == "cmax") %>%
#     ggplot2::ggplot(ggplot2::aes(x = FASTED, y = PPORRES)) +
#     ggplot2::geom_boxplot() +
#     ggplot2::geom_point() +
#     ggplot2::theme_bw()
# })
