## Tests for nif_viewer

make_viewer_nif <- function() {
  nif(tibble::tribble(
    ~ID, ~TIME, ~TAFD, ~TAD, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~PARENT, ~DOSE,
      1,     0,     0,    0,  100,    1,     1,  NA,     "S1",     "A",     "A",   100,
      1,     1,     1,    1,    0,    2,     0,  10,     "S1",     "A",     "A",   100,
      1,     2,     2,    2,    0,    2,     0,   8,     "S1",     "A",     "A",   100,
      2,     0,     0,    0,  200,    1,     1,  NA,     "S2",     "A",     "A",   200,
      2,     1,     1,    1,    0,    2,     0,  20,     "S2",     "A",     "A",   200,
      2,     2,     2,    2,    0,    2,     0,  15,     "S2",     "A",     "A",   200
  ), silent = TRUE)
}


set_viewer_inputs <- function(
    session,
    subject = "S1",
    timeselect = "global",
    time = "TIME",
    maxtime = 24,
    analytes = "A",
    log_yscale = FALSE,
    dose = "all",
    admin = "A"
) {
  session$setInputs(
    subject = subject,
    timeselect = timeselect,
    time = time,
    maxtime = maxtime,
    analytes = analytes,
    log_yscale = log_yscale,
    dose = dose,
    admin = admin
  )
}


test_that("nif_viewer requires a nif object", {
  expect_error(
    nif_viewer(data.frame(ID = 1, TIME = 0, AMT = 0, CMT = 1, EVID = 0, DV = 0)),
    "nif object"
  )
})


test_that("nif_viewer errors when required fields are missing", {
  minimal_nif <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
      1,     0,    0,    1,     0,   0
  ), silent = TRUE)

  expect_error(
    nif_viewer(minimal_nif),
    "Missing required fields"
  )
  expect_error(
    nif_viewer(minimal_nif),
    "USUBJID"
  )
})


test_that("nif_viewer errors when numeric fields have wrong type", {
  bad <- make_viewer_nif()
  bad$TIME <- as.character(bad$TIME)

  expect_error(
    nif_viewer(bad),
    "TIME.*must be numeric"
  )
})


test_that("nif_viewer warns when ID, TIME, or EVID contain NA", {
  missing_nif <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~PARENT, ~DOSE,
      1,     0,  100,    1,     1,  NA,     "S1",     "A",     "A",   100,
      1,     1,    0,    2,    NA,   0,     "S1",     "A",     "A",   100
  ), silent = TRUE)

  expect_warning(
    nif_viewer(missing_nif),
    "Dataset contains missing values in ID, TIME, or EVID columns"
  )
})


test_that("nif_viewer returns a shiny app object", {
  app <- nif_viewer(make_viewer_nif())

  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource))
})


test_that("nif_viewer builds when doses are empty", {
  obs_only <- nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~PARENT,
      1,     0,    0,    2,     0,   1,     "S1",     "A",     "A"
  ), silent = TRUE)

  expect_warning(
    app <- nif_viewer(obs_only),
    "max"
  )
  expect_s3_class(app, "shiny.appobj")
})


test_that("nif_viewer works with package example data", {
  expect_s3_class(nif_viewer(examplinib_sad_nif), "shiny.appobj")
  expect_s3_class(nif_viewer(examplinib_fe_nif), "shiny.appobj")
})


test_that("nif_viewer server renders plots for default inputs", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(session)
    expect_no_error({
      suppressWarnings({
        output$plot.pc
        output$plot.dose
      })
    })
  })
})


test_that("nif_viewer server renders plots for TAFD and TAD", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(session, time = "TAFD", timeselect = "indiv")
    expect_no_error({
      suppressWarnings({
        output$plot.pc
        output$plot.dose
      })
    })

    set_viewer_inputs(session, time = "TAD", timeselect = "global")
    expect_no_error({
      suppressWarnings({
        output$plot.pc
        output$plot.dose
      })
    })
  })
})


test_that("nif_viewer server supports custom max time and log scale", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(
      session,
      timeselect = "custom",
      maxtime = 5,
      log_yscale = TRUE
    )
    expect_no_error({
      suppressWarnings({
        output$plot.pc
        output$plot.dose
      })
    })
  })
})


test_that("nif_viewer server accepts admin = none without error", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(session, admin = "none")
    expect_no_error({
      suppressWarnings(output$plot.pc)
    })
  })
})


test_that("nif_viewer server dose filter does not error", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(session, dose = "all")
    expect_no_error(session$setInputs(dose = "200"))
    expect_no_error({
      suppressWarnings({
        output$plot.pc
        output$plot.dose
      })
    })

    expect_no_error(session$setInputs(dose = "all"))
    expect_no_error({
      suppressWarnings({
        output$plot.pc
        output$plot.dose
      })
    })
  })
})


test_that("nif_viewer server subject navigation does not error", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(session, subject = "S1")
    expect_no_error(session$setInputs(next.sb = 1))
    expect_no_error({
      suppressWarnings(output$plot.pc)
    })

    set_viewer_inputs(session, subject = "S2")
    expect_no_error(session$setInputs(prev.sb = 1))
    expect_no_error({
      suppressWarnings(output$plot.pc)
    })

    # first subject: previous should be a no-op
    set_viewer_inputs(session, subject = "S1")
    expect_no_error(session$setInputs(prev.sb = 2))
  })
})


test_that("nif_viewer server timeselect toggle does not error", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(session, timeselect = "indiv")
    expect_no_error(session$setInputs(timeselect = "custom"))
    expect_no_error(session$setInputs(timeselect = "global"))
  })
})


test_that("nif_viewer server handles invalid custom max time", {
  app <- nif_viewer(make_viewer_nif())

  shiny::testServer(app, {
    set_viewer_inputs(session, timeselect = "custom", maxtime = -1)
    # max_time() returns NA and notifies; plot path should not throw from testServer
    expect_no_error({
      suppressWarnings(output$plot.pc)
    })
  })
})


test_that("nif_viewer server handles missing time field", {
  nif_no_tad <- make_viewer_nif() |>
    dplyr::select(-"TAD") |>
    nif(silent = TRUE)
  app <- nif_viewer(nif_no_tad)

  shiny::testServer(app, {
    set_viewer_inputs(session, time = "TAD")
    expect_no_error({
      suppressWarnings(output$plot.pc)
    })
  })
})
