point_data <- function(p) {
  ggplot2::ggplot_build(p)$data[[1]]
}


mapping_label <- function(p, aesthetic) {
  rlang::as_label(p$mapping[[aesthetic]])
}


time_plot_nif <- function() {
  as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~NTIME, ~TAD, ~TAFD, ~ANALYTE, ~CHECK,
     1L,     0,  100,    1,    1L,   NA_real_,      0,    0,     0,   "DRUG",      "",
     1L,     1,    0,    2,    0L,         10,      1,    1,     1,   "DRUG",      "",
     1L,    10,    0,    2,    0L,         20,     10,   10,    10,   "DRUG",  "flag",
     1L,    24,    0,    2,    0L,         30,     24,   24,    24,   "DRUG",      ""
  ))
}


# ---- Input validation --------------------------------------------------------

test_that("time_plot rejects non-nif input", {
  expect_error(time_plot(mtcars), "Input must be a nif object")
})


test_that("time_plot rejects invalid xtime and ytime", {
  obj <- time_plot_nif()

  expect_error(time_plot(obj, xtime = NULL), "xtime must not be NULL")
  expect_error(time_plot(obj, ytime = NULL), "ytime must not be NULL")
  expect_error(
    time_plot(obj, xtime = "FOO"),
    "xtime must be TIME, TAFD, TAD or NTIME!"
  )
  expect_error(
    time_plot(obj, ytime = "FOO"),
    "ytime must be TIME, TAFD, TAD or NTIME!"
  )
  expect_error(
    time_plot(obj, xtime = "time"),
    "xtime must be TIME, TAFD, TAD or NTIME!"
  )
  expect_error(
    time_plot(obj, xtime = c("TIME", "TAD")),
    "xtime must be a single value"
  )
  expect_error(
    time_plot(obj, ytime = c("TIME", "TAD")),
    "ytime must be a single value"
  )
})


test_that("time_plot errors when the selected time columns are missing", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,    1,   "DRUG"
  ))
  no_tad <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,   "DRUG",
     1L,     1,    0,    2,    0L,         10,   "DRUG"
  ))

  expect_error(time_plot(obj, xtime = "TAFD"), "TAFD")
  expect_error(time_plot(obj, ytime = "NTIME"), "NTIME")
  expect_error(time_plot(no_tad), "TAD")
})


test_that("time_plot validates analyte", {
  obj <- time_plot_nif()

  expect_error(time_plot(obj, analyte = 1L), "character")
  expect_error(time_plot(obj, analyte = NA_character_), "NA")
  expect_error(time_plot(obj, analyte = ""), "non-empty")
  expect_error(
    time_plot(obj, analyte = c("DRUG", "MET")),
    "analyte must be a single value"
  )
  expect_error(
    time_plot(obj, analyte = "NOPE"),
    "Analyte NOPE not found in nif object!"
  )
})


test_that("time_plot validates max_time", {
  obj <- time_plot_nif()

  expect_error(time_plot(obj, max_time = "10"), "numeric")
  expect_error(time_plot(obj, max_time = NA_real_), "NA")
  expect_error(time_plot(obj, max_time = c(1, 2)), "single value")
})


# ---- Return value ------------------------------------------------------------

test_that("time_plot returns a ggplot object", {
  p <- time_plot(time_plot_nif())

  expect_s3_class(p, "ggplot")
})


test_that("time_plot maps default TIME vs TAD and groups by ID", {
  p <- time_plot(time_plot_nif())

  expect_equal(mapping_label(p, "x"), "TIME")
  expect_equal(mapping_label(p, "y"), "TAD")
  expect_true("group" %in% names(p$mapping))
})


test_that("time_plot uses the requested xtime and ytime columns", {
  p <- time_plot(time_plot_nif(), xtime = "NTIME", ytime = "TAFD")

  expect_equal(mapping_label(p, "x"), "NTIME")
  expect_equal(mapping_label(p, "y"), "TAFD")
  expect_equal(point_data(p)$x, c(1, 10, 24))
  expect_equal(point_data(p)$y, c(1, 10, 24))
})


test_that("time_plot accepts all allowed time metrics", {
  obj <- time_plot_nif()
  metrics <- c("TIME", "TAFD", "TAD", "NTIME")

  for (xtime in metrics) {
    for (ytime in metrics) {
      expect_s3_class(
        time_plot(obj, xtime = xtime, ytime = ytime),
        "ggplot"
      )
    }
  }
})


test_that("time_plot draws points with theme_bw and legend at the bottom", {
  p <- time_plot(time_plot_nif())

  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_equal(p$theme$legend.position, "bottom")
  expect_s3_class(p$theme$panel.border, "element_rect")
})


# ---- Observation filtering ---------------------------------------------------

test_that("time_plot plots observation rows and excludes dose rows", {
  p <- time_plot(time_plot_nif())
  pts <- point_data(p)

  expect_equal(pts$x, c(1, 10, 24))
  expect_equal(pts$y, c(1, 10, 24))
})


test_that("time_plot drops rows with missing xtime or ytime", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,    NA,    0,    2,    0L,         10,    1,   "DRUG",
     1L,     2,    0,    2,    0L,         20,   NA,   "DRUG",
     1L,     3,    0,    2,    0L,         30,    3,   "DRUG"
  ))

  pts <- point_data(time_plot(obj, max_time = 100))

  expect_equal(pts$x, 3)
  expect_equal(pts$y, 3)
})


test_that("time_plot keeps rows at max_time (inclusive) and drops later times", {
  obj <- time_plot_nif()

  at_limit <- point_data(time_plot(obj, max_time = 10))
  expect_equal(at_limit$x, c(1, 10))

  below_limit <- point_data(time_plot(obj, max_time = 9))
  expect_equal(below_limit$x, 1)
})


test_that("time_plot default max_time is the maximum observation xtime", {
  pts <- point_data(time_plot(time_plot_nif()))

  expect_equal(max(pts$x), 24)
  expect_equal(pts$x, c(1, 10, 24))
})


test_that("time_plot filters max_time on xtime, not ytime", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,  999,   "DRUG"
  ))

  pts <- point_data(time_plot(obj, max_time = 5))

  expect_equal(pts$x, 1)
  expect_equal(pts$y, 999)
})


test_that("time_plot includes TIME = 0 observations when max_time is 0", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     0,    0,    2,    0L,          5,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,    1,   "DRUG"
  ))

  pts <- point_data(time_plot(obj, max_time = 0))

  expect_equal(pts$x, 0)
  expect_equal(pts$y, 0)
})


test_that("time_plot with max_time before all observations returns an empty plot", {
  pts <- point_data(time_plot(time_plot_nif(), max_time = -1))

  expect_equal(nrow(pts), 0L)
})


test_that("time_plot with Inf max_time includes all finite observation times", {
  pts <- point_data(time_plot(time_plot_nif(), max_time = Inf))

  expect_equal(pts$x, c(1, 10, 24))
})


test_that("time_plot groups points by ID", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,    1,   "DRUG",
     2L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     2L,     5,    0,    2,    0L,         20,    5,   "DRUG"
  ))

  pts <- point_data(time_plot(obj))

  expect_equal(pts$x, c(1, 5))
  expect_equal(as.integer(pts$group), c(1L, 2L))
})


# ---- Analyte filter ----------------------------------------------------------

test_that("time_plot defaults to all observation analytes", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,    1,   "DRUG",
     1L,     2,    0,    2,    0L,         20,    2,    "MET"
  ))

  pts <- point_data(time_plot(obj))

  expect_equal(pts$x, c(1, 2))
  expect_equal(pts$y, c(1, 2))
})


test_that("time_plot restricts points to the requested analyte", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,    1,   "DRUG",
     1L,     2,    0,    2,    0L,         20,    2,    "MET"
  ))

  pts <- point_data(time_plot(obj, analyte = "MET"))

  expect_equal(pts$x, 2)
  expect_equal(pts$y, 2)
})


# ---- Color aesthetic ---------------------------------------------------------

test_that("time_plot colors by CHECK when that column is present", {
  p <- time_plot(time_plot_nif())
  pts <- point_data(p)

  expect_equal(mapping_label(p, "colour"), "CHECK")
  expect_null(p$labels$colour)
  expect_equal(length(unique(pts$colour)), 2L)
})


test_that("time_plot does not map color when the color field is absent", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,    1,   "DRUG"
  ))

  p <- time_plot(obj)

  expect_false("colour" %in% names(p$mapping))
  expect_equal(names(p$mapping), c("x", "y", "group"))
})


test_that("time_plot can color by a different existing field", {
  obj <- as_nif_test(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID,        ~DV, ~TAD, ~ANALYTE,
     1L,     0,  100,    1,    1L,   NA_real_,    0,   "DRUG",
     1L,     1,    0,    2,    0L,         10,    1,   "DRUG",
     1L,     2,    0,    2,    0L,         20,    2,    "MET"
  ))

  p <- time_plot(obj, color = "ANALYTE")
  pts <- point_data(p)

  expect_equal(mapping_label(p, "colour"), "ANALYTE")
  expect_equal(length(unique(pts$colour)), 2L)
})


test_that("time_plot ignores a color field that is not in the nif", {
  p <- time_plot(time_plot_nif(), color = "NOPE")

  expect_false("colour" %in% names(p$mapping))
  expect_equal(names(p$mapping), c("x", "y", "group"))
})


test_that("time_plot errors when color is NULL", {
  expect_error(
    time_plot(time_plot_nif(), color = NULL),
    "argument is of length zero"
  )
})


# ---- Graphical parameters and watermark --------------------------------------

test_that("time_plot forwards graphical parameters to geom_point", {
  p <- time_plot(time_plot_nif(), alpha = 0.3, size = 4)

  expect_equal(p$layers[[1]]$aes_params$alpha, 0.3)
  expect_equal(p$layers[[1]]$aes_params$size, 4)
})


test_that("time_plot has no watermark layer when the watermark option is unset", {
  local_nif_option()
  assign("watermark", NA, envir = nif:::.nif_env)

  p <- time_plot(time_plot_nif())

  expect_length(p$layers, 1L)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
})


test_that("time_plot adds a watermark layer when nif_option watermark is set", {
  local_nif_option(watermark = "CONFIDENTIAL")

  p <- time_plot(time_plot_nif())

  expect_length(p$layers, 2L)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$geom, "GeomCustomAnn")
})
