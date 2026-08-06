# Test file for add_ae_observation function

# Shared fixture helpers -------------------------------------------------------

create_ae_fixture <- function(
    ae = NULL,
    dm = NULL,
    vs = NULL,
    ex = NULL,
    include_subj3 = TRUE
) {
  if (is.null(ae)) {
    ae <- tibble::tribble(
      ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
      "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      ,
      "SUBJ-002", "Headache" , "2023-01-02T09:00", 1       , 1      ,
      "SUBJ-003", "Nausea"   , "2023-01-01T10:00", 3       , 1
    )
  }

  subjects <- unique(ae$USUBJID)
  if (!include_subj3) {
    subjects <- setdiff(subjects, "SUBJ-003")
  }

  if (is.null(dm)) {
    dm <- tibble::tribble(
      ~USUBJID  , ~RFSTDTC         , ~ACTARMCD , ~SEX , ~RFENDTC         ,
      "SUBJ-001", "2023-01-01T00:00", "TRT1"   , "F"  , "2023-01-02T00:00",
      "SUBJ-002", "2023-01-01T00:00", "TRT1"   , "F"  , "2023-01-02T00:00",
      "SUBJ-003", "2023-01-01T00:00", "TRT1"   , "F"  , "2023-01-02T00:00"
    ) |>
      dplyr::filter(.data$USUBJID %in% subjects)
  }

  if (is.null(vs)) {
    vs <- tibble::tribble(
      ~USUBJID  , ~VSTESTCD , ~VSSTRESN , ~VSBLFL ,
      "SUBJ-001", "HEIGHT"  , 170       , "Y"     ,
      "SUBJ-001", "WEIGHT"  , 70        , "Y"     ,
      "SUBJ-002", "HEIGHT"  , 165       , "Y"     ,
      "SUBJ-002", "WEIGHT"  , 65        , "Y"     ,
      "SUBJ-003", "HEIGHT"  , 175       , "Y"     ,
      "SUBJ-003", "WEIGHT"  , 75        , "Y"
    ) |>
      dplyr::filter(.data$USUBJID %in% unique(dm$USUBJID))
  }

  if (is.null(ex)) {
    ex <- tibble::tribble(
      ~USUBJID  , ~EXTRT , ~EXDOSE , ~EXSTDTC         , ~EXENDTC         , ~EXSEQ ,
      "SUBJ-001", "DRUG" , 100     , "2023-01-01T00:00", "2023-01-01T01:00", 1      ,
      "SUBJ-002", "DRUG" , 100     , "2023-01-01T00:00", "2023-01-01T01:00", 1      ,
      "SUBJ-003", "DRUG" , 100     , "2023-01-01T00:00", "2023-01-01T01:00", 1
    ) |>
      dplyr::filter(.data$USUBJID %in% unique(dm$USUBJID))
  }

  test_sdtm <- sdtm(list(ae = ae, dm = dm, vs = vs, ex = ex))
  base_nif <- nif() |>
    add_administration(test_sdtm, "DRUG", analyte = "DRUG", silent = TRUE)

  list(sdtm = test_sdtm, nif = base_nif, ae = ae, dm = dm, vs = vs, ex = ex)
}


test_that("add_ae_observation handles basic case correctly", {
  fx <- create_ae_fixture()

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  expect_s3_class(result, "nif")
  expect_true(all(c(
    "USUBJID", "DTC", "DV", "ANALYTE", "TIME", "CMT",
    "AMT", "PARENT", "EVID", "MDV", "ID"
  ) %in% names(result)))

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(nrow(ae_rows), 2)
  expect_equal(
    ae_rows$DV[order(ae_rows$USUBJID)],
    c(2, 1)
  )
  expect_equal(unique(ae_rows$PARENT), "DRUG")
  expect_equal(unique(ae_rows$CMT), 2)
  expect_equal(unique(ae_rows$EVID), 0)
  expect_equal(unique(ae_rows$AMT), 0)
  expect_true(all(ae_rows$MDV == 0))
})


test_that("add_ae_observation preserves administration rows", {
  fx <- create_ae_fixture()
  n_admin_before <- sum(fx$nif$EVID == 1)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  admin_rows <- result |> dplyr::filter(.data$ANALYTE == "DRUG", .data$EVID == 1)
  expect_equal(nrow(admin_rows), n_admin_before)
  expect_true(all(c("SUBJ-001", "SUBJ-002", "SUBJ-003") %in% result$USUBJID))
})


test_that("add_ae_observation fills TIME via normalize_nif", {
  fx <- create_ae_fixture()

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_true(all(is.finite(ae_rows$TIME)))
  expect_true(all(ae_rows$TIME >= 0))
})


test_that("add_ae_observation reassigns ID after binding", {
  fx <- create_ae_fixture()

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  id_map <- result |>
    dplyr::distinct(.data$USUBJID, .data$ID) |>
    dplyr::arrange(.data$USUBJID)

  expect_equal(id_map$ID, as.numeric(as.factor(id_map$USUBJID)))
  expect_equal(length(unique(result$ID)), length(unique(result$USUBJID)))
})


test_that("add_ae_observation handles different ae_fields correctly", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD , ~AELLT , ~AEHLT , ~AESTDTC         , ~AETOXGR ,
    "SUBJ-001", "Term1"  , "LLT1" , "HLT1" , "2023-01-01T08:00", 2        ,
    "SUBJ-002", "Term2"  , "LLT1" , "HLT2" , "2023-01-02T09:00", 1
  )
  fx <- create_ae_fixture(ae = ae)

  result_llt <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "LLT1",
    ae_field = "AELLT",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )
  expect_equal(nrow(dplyr::filter(result_llt, .data$ANALYTE == "AE_LLT1")), 2)

  result_hlt <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "HLT1",
    ae_field = "AEHLT",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )
  expect_equal(nrow(dplyr::filter(result_hlt, .data$ANALYTE == "AE_HLT1")), 1)
})


test_that("add_ae_observation applies default subject filter", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      ,
    "SUBJ-002", "Headache" , "2023-01-02T09:00", 1       , 1
  )
  dm <- tibble::tribble(
    ~USUBJID  , ~RFSTDTC         , ~ACTARMCD  , ~SEX , ~RFENDTC         ,
    "SUBJ-001", "2023-01-01T00:00", "SCRNFAIL" , "M"  , "2023-01-02T00:00",
    "SUBJ-002", "2023-01-01T00:00", "TRT1"     , "M"  , "2023-01-02T00:00"
  )
  fx <- create_ae_fixture(ae = ae, dm = dm)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(nrow(ae_rows), 1)
  expect_equal(unique(ae_rows$USUBJID), "SUBJ-002")
})


test_that("add_ae_observation applies observation_filter", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      ,
    "SUBJ-002", "Headache" , "2023-01-02T09:00", 1       , 1
  )
  fx <- create_ae_fixture(ae = ae)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    observation_filter = "AETOXGR > 1",
    silent = TRUE
  )

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(nrow(ae_rows), 1)
  expect_equal(ae_rows$USUBJID, "SUBJ-001")
  expect_equal(ae_rows$DV, 2)
})


test_that("add_ae_observation applies custom subject_filter", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      ,
    "SUBJ-002", "Headache" , "2023-01-02T09:00", 1       , 1
  )
  dm <- tibble::tribble(
    ~USUBJID  , ~RFSTDTC         , ~ACTARMCD , ~SEX , ~RFENDTC         ,
    "SUBJ-001", "2023-01-01T00:00", "TRT1"   , "F"  , "2023-01-02T00:00",
    "SUBJ-002", "2023-01-01T00:00", "TRT1"   , "M"  , "2023-01-02T00:00"
  )
  fx <- create_ae_fixture(ae = ae, dm = dm)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    subject_filter = "SEX == 'F'",
    silent = TRUE
  )

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(nrow(ae_rows), 1)
  expect_equal(ae_rows$USUBJID, "SUBJ-001")
})


test_that("add_ae_observation keeps only AE subjects present in nif", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      ,
    "SUBJ-002", "Headache" , "2023-01-02T09:00", 1       , 1      ,
    "SUBJ-003", "Headache" , "2023-01-01T10:00", 3       , 1
  )
  # nif built from subjects 001 and 002 only; AE domain still has 003
  fx <- create_ae_fixture(ae = ae, include_subj3 = FALSE)
  full_sdtm <- sdtm(list(
    ae = ae,
    dm = tibble::tribble(
      ~USUBJID  , ~RFSTDTC         , ~ACTARMCD , ~SEX , ~RFENDTC         ,
      "SUBJ-001", "2023-01-01T00:00", "TRT1"   , "F"  , "2023-01-02T00:00",
      "SUBJ-002", "2023-01-01T00:00", "TRT1"   , "F"  , "2023-01-02T00:00",
      "SUBJ-003", "2023-01-01T00:00", "TRT1"   , "F"  , "2023-01-02T00:00"
    ),
    vs = tibble::tribble(
      ~USUBJID  , ~VSTESTCD , ~VSSTRESN , ~VSBLFL ,
      "SUBJ-001", "HEIGHT"  , 170       , "Y"     ,
      "SUBJ-001", "WEIGHT"  , 70        , "Y"     ,
      "SUBJ-002", "HEIGHT"  , 165       , "Y"     ,
      "SUBJ-002", "WEIGHT"  , 65        , "Y"     ,
      "SUBJ-003", "HEIGHT"  , 175       , "Y"     ,
      "SUBJ-003", "WEIGHT"  , 75        , "Y"
    ),
    ex = fx$ex
  ))

  result <- add_ae_observation(
    fx$nif,
    full_sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(nrow(ae_rows), 2)
  expect_false("SUBJ-003" %in% ae_rows$USUBJID)
  expect_false("SUBJ-003" %in% result$USUBJID)
})


test_that("add_ae_observation handles debug mode correctly", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1
  )
  fx <- create_ae_fixture(ae = ae)

  result_debug <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    debug = TRUE,
    silent = TRUE
  )

  expect_true("SRC_DOMAIN" %in% names(result_debug))
  expect_true("SRC_SEQ" %in% names(result_debug))
  expect_equal(
    unique(result_debug$SRC_DOMAIN[result_debug$ANALYTE == "AE_Headache"]),
    "AE"
  )
})


test_that("add_ae_observation handles keep parameter correctly", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ , ~CUSTOM_COL ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      , "Value1"
  )
  fx <- create_ae_fixture(ae = ae)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    keep = "CUSTOM_COL",
    silent = TRUE
  )

  expect_true("CUSTOM_COL" %in% names(result))
  expect_equal(
    result$CUSTOM_COL[result$ANALYTE == "AE_Headache"],
    "Value1"
  )
})


test_that("add_ae_observation accepts multiple keep columns", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ , ~CUSTOM_A , ~CUSTOM_B ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      , "A"       , "B"
  )
  fx <- create_ae_fixture(ae = ae)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    keep = c("CUSTOM_A", "CUSTOM_B"),
    silent = TRUE
  )

  expect_true(all(c("CUSTOM_A", "CUSTOM_B") %in% names(result)))
  ae_row <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(ae_row$CUSTOM_A, "A")
  expect_equal(ae_row$CUSTOM_B, "B")
})


test_that("add_ae_observation assigns parent and cmt automatically", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1
  )
  fx <- create_ae_fixture(ae = ae)
  expected_cmt <- max(fx$nif$CMT) + 1

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    silent = TRUE
  )

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(unique(ae_rows$PARENT), "DRUG")
  expect_equal(unique(ae_rows$CMT), expected_cmt)
})


test_that("add_ae_observation messages when parent or cmt are guessed", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1
  )
  fx <- create_ae_fixture(ae = ae)

  expect_message(
    expect_message(
      add_ae_observation(fx$nif, fx$sdtm, "Headache", silent = FALSE),
      "Parent for AE_Headache was set to DRUG"
    ),
    "Compartment for AE_Headache was not specified"
  )

  expect_message(
    add_ae_observation(
      fx$nif,
      fx$sdtm,
      "Headache",
      parent = "DRUG",
      silent = FALSE
    ),
    "Compartment for AE_Headache"
  )

  expect_silent(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", silent = TRUE)
  )
})


test_that("add_ae_observation uses custom analyte name", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1
  )
  fx <- create_ae_fixture(ae = ae)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    analyte = "AE_HX",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  expect_equal(sum(result$ANALYTE == "AE_HX"), 1)
  expect_equal(sum(result$ANALYTE == "AE_Headache"), 0)
})


test_that("add_ae_observation sanitizes spaces in default analyte name", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD        , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Head ache"     , "2023-01-01T08:00", 2       , 1
  )
  fx <- create_ae_fixture(ae = ae)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Head ache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  expect_true("AE_Head_ache" %in% result$ANALYTE)
})


test_that("add_ae_observation uses coding_table when AETOXGR is absent", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AESEV     , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", "MILD"    , 1      ,
    "SUBJ-002", "Headache" , "2023-01-02T09:00", "SEVERE"  , 1
  )
  fx <- create_ae_fixture(ae = ae)
  coding <- tibble::tribble(
    ~AESEV    , ~DV ,
    "MILD"    , 1   ,
    "MODERATE", 2   ,
    "SEVERE"  , 3
  )

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    coding_table = coding,
    silent = TRUE
  )

  ae_rows <- result |>
    dplyr::filter(.data$ANALYTE == "AE_Headache") |>
    dplyr::arrange(.data$USUBJID)
  expect_equal(ae_rows$DV, c(1, 3))
  expect_true(all(ae_rows$MDV == 0))
})


test_that("add_ae_observation handles multiple AE events per subject", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 2       , 1      ,
    "SUBJ-001", "Headache" , "2023-01-02T12:00", 3       , 2      ,
    "SUBJ-002", "Headache" , "2023-01-01T09:00", 1       , 1
  )
  fx <- create_ae_fixture(ae = ae)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  ae_rows <- result |> dplyr::filter(.data$ANALYTE == "AE_Headache")
  expect_equal(nrow(ae_rows), 3)
  expect_equal(sum(ae_rows$USUBJID == "SUBJ-001"), 2)
})


test_that("add_ae_observation adds no AE rows when term does not match", {
  fx <- create_ae_fixture()
  n_before <- nrow(fx$nif)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Nonexistent",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  expect_equal(sum(result$ANALYTE == "AE_Nonexistent"), 0)
  expect_equal(nrow(result), n_before)
})


test_that("add_ae_observation validates nif input", {
  fx <- create_ae_fixture()

  expect_error(
    add_ae_observation(data.frame(), fx$sdtm, "Headache"),
    "nif"
  )
  expect_error(
    add_ae_observation(NULL, fx$sdtm, "Headache"),
    "nif"
  )
})


test_that("add_ae_observation validates sdtm input", {
  fx <- create_ae_fixture()

  expect_error(
    add_ae_observation(fx$nif, list(), "Headache"),
    "sdtm"
  )

  sdtm_no_ae <- sdtm(list(dm = fx$dm, vs = fx$vs, ex = fx$ex))
  expect_error(
    add_ae_observation(fx$nif, sdtm_no_ae, "Headache"),
    "ae"
  )
})


test_that("add_ae_observation validates scalar and typed arguments", {
  fx <- create_ae_fixture()

  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, c("Headache", "Nausea")),
    "ae_term"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, 1),
    "ae_term"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", ae_field = 1),
    "ae_field"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", analyte = 1),
    "analyte"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", parent = 1),
    "parent"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", cmt = "2"),
    "cmt"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", subject_filter = 1),
    "subject_filter"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", observation_filter = 1),
    "observation_filter"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", coding_table = "x"),
    "coding_table"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", keep = 1),
    "keep"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", debug = "yes"),
    "debug"
  )
  expect_error(
    add_ae_observation(fx$nif, fx$sdtm, "Headache", silent = "yes"),
    "silent"
  )
})


test_that("add_ae_observation arranges by USUBJID and DTC", {
  ae <- tibble::tribble(
    ~USUBJID  , ~AEDECOD   , ~AESTDTC         , ~AETOXGR , ~AESEQ ,
    "SUBJ-002", "Headache" , "2023-01-01T12:00", 1       , 1      ,
    "SUBJ-001", "Headache" , "2023-01-02T08:00", 2       , 1      ,
    "SUBJ-001", "Headache" , "2023-01-01T08:00", 3       , 2
  )
  fx <- create_ae_fixture(ae = ae)

  result <- add_ae_observation(
    fx$nif,
    fx$sdtm,
    "Headache",
    parent = "DRUG",
    cmt = 2,
    silent = TRUE
  )

  expect_true(all(diff(as.integer(as.factor(result$USUBJID))) >= 0))

  within_subj <- result |>
    dplyr::filter(.data$USUBJID == "SUBJ-001") |>
    dplyr::pull(.data$DTC)
  expect_true(all(diff(as.numeric(within_subj)) >= 0))
})
