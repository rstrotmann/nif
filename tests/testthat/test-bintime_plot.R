test_nif <- filter(examplinib_poc_nif, DOSE == 500)


test_that("bintime_plot returns a ggplot object", {
  p <- bintime_plot(test_nif, "RS2023", facet = NULL)
  expect_s3_class(p, "ggplot")
})


test_that("bintime_plot works with the example from docs", {
  expect_no_error(
    test_nif |>
      filter(TAFD < 48) |>
      bintime_plot("RS2023", points = TRUE, alpha = 0.2)
  )
})


test_that("bintime_plot labels reflect analyte and time", {
  p <- bintime_plot(test_nif, "RS2023", facet = NULL)
  expect_equal(p$labels$y, "RS2023")
  expect_equal(p$labels$x, "TAFD")
})


test_that("bintime_plot auto-generates title from analyte", {
  p <- bintime_plot(test_nif, "RS2023")
  expect_true(grepl("RS2023", p$labels$title))
})


test_that("bintime_plot uses custom title", {
  p <- bintime_plot(test_nif, "RS2023", title = "My Title")
  expect_equal(p$labels$title, "My Title by DOSE")
})


test_that("bintime_plot with empty title suppresses auto title", {
  p <- bintime_plot(test_nif, "RS2023", title = "", facet = NULL)
  expect_equal(p$labels$title, "")
})


test_that("bintime_plot shows caption by default", {
  p <- bintime_plot(test_nif, "RS2023")
  expect_true(grepl("Mean with 90% CI", p$labels$caption))
  expect_true(grepl("kmeans", p$labels$caption))
})


test_that("bintime_plot caption reflects method", {
  p <- bintime_plot(test_nif, "RS2023", method = "fisher")
  expect_true(grepl("fisher", p$labels$caption))
})


test_that("bintime_plot suppresses caption when caption = FALSE", {
  p <- bintime_plot(test_nif, "RS2023", caption = FALSE)
  expect_null(p$labels$caption)
})


test_that("bintime_plot handles caption = NULL as FALSE", {
  p <- bintime_plot(test_nif, "RS2023", caption = NULL)
  expect_null(p$labels$caption)
})


# --- Faceting ---

test_that("bintime_plot facets by DOSE by default", {
  p <- bintime_plot(test_nif, "RS2023")
  expect_true(!is.null(p$facet))
  expect_true(grepl("by DOSE", p$labels$title))
})


test_that("bintime_plot with facet = NULL produces no faceting", {
  p <- bintime_plot(test_nif, "RS2023", facet = NULL)
  expect_false(grepl("by", p$labels$title))
})


test_that("bintime_plot facets by custom field", {
  p <- bintime_plot(test_nif, "RS2023", facet = "SEX")
  expect_true(grepl("by SEX", p$labels$title))
})


test_that("bintime_plot with multiple facet fields", {
  p <- bintime_plot(test_nif, "RS2023", facet = c("SEX", "DOSE"))
  expect_true(grepl("by SEX and DOSE", p$labels$title))
})


test_that("bintime_plot scales parameter is passed to facet_wrap", {
  expect_no_error(
    bintime_plot(test_nif, "RS2023", scales = "free")
  )
  expect_no_error(
    bintime_plot(test_nif, "RS2023", scales = "free_y")
  )
})


# --- Points ---

test_that("bintime_plot with points = TRUE adds geom_point layer", {
  p <- bintime_plot(test_nif, "RS2023", points = TRUE)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% layer_classes)
})


test_that("bintime_plot with points = FALSE has no geom_point layer", {
  p <- bintime_plot(test_nif, "RS2023", points = FALSE)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPoint" %in% layer_classes)
})


test_that("bintime_plot with points and color", {
  expect_no_error(
    bintime_plot(test_nif, "RS2023", points = TRUE, color = "SEX")
  )
})


test_that("bintime_plot with points respects alpha and size", {
  expect_no_error(
    bintime_plot(
      test_nif, "RS2023",
      points = TRUE, alpha = 0.3, size = 2
    )
  )
})


# --- Color ---

test_that("bintime_plot with color parameter", {
  p <- bintime_plot(test_nif, "RS2023", color = "SEX")
  expect_equal(p$labels$colour, "SEX")
})


test_that("bintime_plot with multiple color fields", {
  p <- bintime_plot(
    test_nif, "RS2023", color = c("SEX", "DOSE")
  )
  expect_true(grepl("SEX", p$labels$colour))
  expect_true(grepl("DOSE", p$labels$colour))
})


test_that("bintime_plot without color hides legend", {
  p <- bintime_plot(test_nif, "RS2023")
  expect_equal(p$theme$legend.position, "none")
})


test_that("bintime_plot with color shows legend at bottom", {
  p <- bintime_plot(test_nif, "RS2023", color = "SEX")
  expect_equal(p$theme$legend.position, "bottom")
})


test_that("bintime_plot legend = FALSE hides legend even with color", {
  p <- bintime_plot(
    test_nif, "RS2023", color = "SEX", legend = FALSE
  )
  expect_equal(p$theme$legend.position, "none")
})


# --- Time limits ---

test_that("bintime_plot respects min_time and max_time", {
  expect_no_error(
    bintime_plot(test_nif, "RS2023", min_time = 0, max_time = 48)
  )
})


test_that("bintime_plot with only min_time set", {
  expect_no_error(
    bintime_plot(test_nif, "RS2023", min_time = 10)
  )
})


test_that("bintime_plot with only max_time set", {
  expect_no_error(
    bintime_plot(test_nif, "RS2023", max_time = 100)
  )
})


# --- Methods ---

test_that("bintime_plot works with different binning methods", {
  methods <- c("kmeans", "fisher", "pretty", "jenks")
  for (m in methods) {
    p <- bintime_plot(test_nif, "RS2023", method = m)
    expect_true(inherits(p, "ggplot"), info = paste("Method:", m))
  }
})


# --- Reference lines ---

test_that("bintime_plot with single refline", {
  p <- bintime_plot(test_nif, "RS2023", refline = 100)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_classes)
})


test_that("bintime_plot with multiple reflines", {
  p <- bintime_plot(test_nif, "RS2023", refline = c(50, 100, 200))
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_classes)
})


test_that("bintime_plot without refline has no GeomHline", {
  p <- bintime_plot(test_nif, "RS2023")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomHline" %in% layer_classes)
})


# --- Change from baseline ---

test_that("bintime_plot with cfb = TRUE works when DVCFB present", {
  obj_cfb <- test_nif |> derive_cfb(silent = TRUE)
  p <- bintime_plot(obj_cfb, "RS2023", cfb = TRUE)
  expect_s3_class(p, "ggplot")
  expect_true(grepl("change from baseline", p$labels$title))
})


test_that("bintime_plot with cfb = TRUE errors when DVCFB missing", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", cfb = TRUE),
    "Missing DVCFB column"
  )
})


test_that("bintime_plot cfb title not overridden by custom title", {
  obj_cfb <- test_nif |> derive_cfb(silent = TRUE)
  p <- bintime_plot(obj_cfb, "RS2023", cfb = TRUE, title = "Custom")
  expect_equal(p$labels$title, "Custom by DOSE")
})


# --- Time field ---

test_that("bintime_plot with custom time field", {
  expect_no_error(
    bintime_plot(test_nif, "RS2023", time = "TAD")
  )
})


test_that("bintime_plot errors with non-existent time field", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", time = "NONEXISTENT"),
    "not found in data set"
  )
})


# --- Analyte ---

test_that("bintime_plot works with secondary analyte", {
  expect_no_error(
    bintime_plot(test_nif, "RS2023487A")
  )
})


# --- ggplot structure ---

test_that("bintime_plot always has geom_segment and geom_rect layers", {
  p <- bintime_plot(test_nif, "RS2023")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% layer_classes)
  expect_true("GeomRect" %in% layer_classes)
})


# --- Input validation ---

test_that("bintime_plot rejects non-nif input", {
  expect_error(
    bintime_plot(data.frame(x = 1), "RS2023")
  )
})


test_that("bintime_plot rejects invalid analyte", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "NONEXISTENT")
  )
})


test_that("bintime_plot rejects invalid method", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", method = "invalid"),
    "not implemented"
  )
})


test_that("bintime_plot rejects non-logical points", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", points = "yes")
  )
})


test_that("bintime_plot rejects non-logical cfb", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", cfb = "yes")
  )
})


test_that("bintime_plot rejects non-numeric alpha", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", alpha = "high")
  )
})


test_that("bintime_plot rejects non-numeric min_time", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", min_time = "early")
  )
})


test_that("bintime_plot rejects non-numeric max_time", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", max_time = "late")
  )
})


test_that("bintime_plot rejects non-character time", {
  expect_error(
    bintime_plot(examplinib_poc_nif, "RS2023", time = 123)
  )
})


# --- Combined parameters ---

test_that("bintime_plot with all options combined", {
  obj_cfb <- test_nif |> derive_cfb(silent = TRUE)
  expect_no_error(
    bintime_plot(
      obj_cfb, "RS2023",
      method = "fisher",
      time = "TAFD",
      color = "SEX",
      facet = "DOSE",
      min_time = 0,
      max_time = 200,
      points = TRUE,
      cfb = TRUE,
      caption = TRUE,
      title = "Full test",
      size = 2,
      alpha = 0.5,
      scales = "free_y",
      refline = c(-10, 0, 10),
      legend = TRUE
    )
  )
})


test_that("bintime_plot with facet = NULL and color = NULL", {
  p <- bintime_plot(
    test_nif, "RS2023", facet = NULL, color = NULL
  )
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "RS2023")
})
