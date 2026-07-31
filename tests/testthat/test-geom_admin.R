## Tests for geom_admin


test_that("geom_admin draws administration times for the selected analyte", {
  test_data <- examplinib_sad_nif |>
    dplyr::filter(.data$ID == 1)

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
    geom_admin(ggplot2::aes(admin = "RS2023")) +
    ggplot2::geom_point(na.rm = TRUE)

  built <- ggplot2::ggplot_build(p)
  admin_layer <- built$data[[1]]

  expect_s3_class(p, "ggplot")
  expect_equal(admin_layer$xintercept, 0)
  expect_equal(unique(admin_layer$colour), "grey")
})


test_that("geom_admin requires a nif object", {
  test_data <- tibble::tribble(
     ~TIME, ~DV, ~EVID, ~ANALYTE,
         0,  NA,     1,  "A",
         1,  10,     0,  "A"
  )

  expect_error(
    ggplot2::ggplot_build(
      ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
        geom_admin(ggplot2::aes(admin = "A"))
    ),
    "Input must be a nif object"
  )

  expect_error(
    geom_admin(
      ggplot2::aes(admin = "A"),
      data = test_data
    ),
    "Input must be a nif object"
  )
})


test_that("geom_admin requires an admin aesthetic", {
  test_data <- examplinib_sad_nif |>
    dplyr::filter(.data$ID == 1)

  expect_error(
    geom_admin(),
    "requires an 'admin' aesthetic"
  )

  expect_error(
    ggplot2::ggplot_build(
      ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
        geom_admin(ggplot2::aes(x = .data$TIME))
    ),
    "requires an 'admin' aesthetic"
  )
})


test_that("geom_admin validates mapping and parameters", {
  expect_error(
    geom_admin(mapping = list(admin = "A")),
    "mapping must be created using aes\\(\\)"
  )

  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), na.rm = "yes"),
    "na.rm must be a single logical value"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), na.rm = c(TRUE, FALSE)),
    "na.rm must be a single logical value"
  )

  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), show.legend = "yes"),
    "show.legend must be a single logical value"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), show.legend = c(TRUE, FALSE)),
    "show.legend must be a single logical value"
  )

  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), inherit.aes = "yes"),
    "inherit.aes must be a single logical value"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), inherit.aes = c(TRUE, FALSE)),
    "inherit.aes must be a single logical value"
  )

  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), color = 1),
    "color must be a single character value"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), color = c("red", "blue")),
    "color must be a single character value"
  )

  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), linewidth = -1),
    "linewidth must be a non-negative numeric value"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), linewidth = "wide"),
    "linewidth must be a non-negative numeric value"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), linewidth = c(0.5, 1)),
    "linewidth must be a non-negative numeric value"
  )

  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), linetype = TRUE),
    "linetype must be a single numeric or character value"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), linetype = c(1, 2)),
    "linetype must be a single numeric or character value"
  )

  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), alpha = -0.1),
    "alpha must be NA or a numeric value between 0 and 1"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), alpha = 1.5),
    "alpha must be NA or a numeric value between 0 and 1"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), alpha = "transparent"),
    "alpha must be NA or a numeric value between 0 and 1"
  )
  expect_error(
    geom_admin(ggplot2::aes(admin = "A"), alpha = c(0.1, 0.2)),
    "alpha must be NA or a numeric value between 0 and 1"
  )
})


test_that("geom_admin accepts valid non-default parameters", {
  test_data <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA
  ))

  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
      geom_admin(
        ggplot2::aes(admin = "A"),
        na.rm = TRUE,
        show.legend = FALSE,
        inherit.aes = TRUE,
        color = "blue",
        linewidth = 1.5,
        linetype = "dashed",
        alpha = 0.5
      )
  )

  layer_data <- built$data[[1]]
  expect_equal(layer_data$xintercept, 0)
  expect_equal(unique(layer_data$colour), "blue")
  expect_equal(unique(layer_data$linewidth), 1.5)
  expect_equal(unique(layer_data$linetype), "dashed")
  expect_equal(unique(layer_data$alpha), 0.5)
})


test_that("geom_admin works with explicit nif data", {
  test_data <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
       1,    24,   50,    1,     1,  NA,      "A",     "A",       FALSE,    50
  ))

  built <- ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_admin(
        ggplot2::aes(x = .data$TIME, admin = "A"),
        data = test_data
      )
  )

  expect_equal(sort(built$data[[1]]$xintercept), c(0, 24))
})


test_that("geom_admin returns no lines for an unknown analyte", {
  test_data <- examplinib_sad_nif |>
    dplyr::filter(.data$ID == 1)

  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
      geom_admin(ggplot2::aes(admin = "NOT_AN_ANALYTE"))
  )

  expect_equal(nrow(built$data[[1]]), 0)
})


test_that("geom_admin uses unique administration times", {
  test_data <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE, ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     0,  100,    1,     1,  NA,      "A",     "A",       FALSE,   100,
       1,     1,    0,    2,     0,  10,      "A",     "A",       FALSE,    NA,
       1,    24,   50,    1,     1,  NA,      "A",     "A",       FALSE,    50
  ))

  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
      geom_admin(ggplot2::aes(admin = "A"), color = "red")
  )

  expect_equal(sort(built$data[[1]]$xintercept), c(0, 24))
  expect_equal(unique(built$data[[1]]$colour), "red")
})


test_that("geom_admin only plots administrations for the selected analyte", {
  test_data <- nif(tibble::tribble(
     ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,  ~PARENT, ~METABOLITE, ~DOSE,
       1,     0,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100,
       1,     0,   50,    2,     1,  NA, "DRUG_B", "DRUG_B",       FALSE,    50,
       1,     1,    0,    3,     0,  10, "DRUG_A", "DRUG_A",       FALSE,    NA,
       1,    24,  100,    1,     1,  NA, "DRUG_A", "DRUG_A",       FALSE,   100
  ))

  built_a <- ggplot2::ggplot_build(
    ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
      geom_admin(ggplot2::aes(admin = "DRUG_A"))
  )
  built_b <- ggplot2::ggplot_build(
    ggplot2::ggplot(test_data, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
      geom_admin(ggplot2::aes(admin = "DRUG_B"))
  )

  expect_equal(sort(built_a$data[[1]]$xintercept), c(0, 24))
  expect_equal(built_b$data[[1]]$xintercept, 0)
})


test_that("stat_admin_proto works with valid character and factor admin", {
  result_chr <- stat_admin_proto$compute_group(
    data.frame(
      x = c(0, 1, 24),
      admin = c("A", "A", "A"),
      EVID = c(1, 0, 1),
      ANALYTE = c("A", "A", "A")
    ),
    NULL
  )
  expect_equal(sort(result_chr$xintercept), c(0, 24))

  result_fct <- stat_admin_proto$compute_group(
    data.frame(
      x = c(0, 1),
      admin = factor(c("A", "A")),
      EVID = c(1, 0),
      ANALYTE = c("A", "A")
    ),
    NULL
  )
  expect_equal(result_fct$xintercept, 0)
})


test_that("stat_admin_proto ignores NA EVID, ANALYTE, and x", {
  result <- stat_admin_proto$compute_group(
    data.frame(
      x = c(0, NA, 24, 48),
      admin = c("A", "A", "A", "A"),
      EVID = c(1, 1, NA, 1),
      ANALYTE = c("A", "A", "A", NA)
    ),
    NULL
  )

  expect_equal(result$xintercept, 0)
})


test_that("stat_admin_proto returns empty for no matching administrations", {
  result <- stat_admin_proto$compute_group(
    data.frame(
      x = c(0, 1),
      admin = c("A", "A"),
      EVID = c(0, 0),
      ANALYTE = c("A", "A")
    ),
    NULL
  )

  expect_equal(nrow(result), 0)
  expect_named(result, "xintercept")
})


test_that("stat_admin_proto throws appropriate errors", {
  expect_error(
    stat_admin_proto$compute_group(list(x = 1, admin = "A"), NULL),
    "data must be a data frame"
  )

  expect_error(
    stat_admin_proto$compute_group(
      data.frame(x = 1, admin = "A", EVID = 1),
      NULL
    ),
    "Missing required columns: ANALYTE"
  )

  expect_error(
    stat_admin_proto$compute_group(
      data.frame(
        x = 1,
        admin = 1,
        EVID = 1,
        ANALYTE = "A"
      ),
      NULL
    ),
    "'admin' must be a character analyte name"
  )

  expect_error(
    stat_admin_proto$compute_group(
      data.frame(
        x = c(0, 1),
        admin = c("A", "B"),
        EVID = c(1, 1),
        ANALYTE = c("A", "B")
      ),
      NULL
    ),
    "'admin' must specify a single non-missing analyte name"
  )

  expect_error(
    stat_admin_proto$compute_group(
      data.frame(
        x = 0,
        admin = NA_character_,
        EVID = 1,
        ANALYTE = "A"
      ),
      NULL
    ),
    "'admin' must specify a single non-missing analyte name"
  )

  expect_error(
    stat_admin_proto$compute_group(
      data.frame(
        x = 0,
        admin = "",
        EVID = 1,
        ANALYTE = "A"
      ),
      NULL
    ),
    "'admin' must specify a single non-missing analyte name"
  )
})
