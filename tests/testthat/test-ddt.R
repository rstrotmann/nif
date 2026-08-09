# Test file for ddt function

test_that("ddt returns a data frame with required columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c("name", "definition", "type", "description", "unit", "source")
  )
  expect_gt(nrow(result), 0)
})


test_that("ddt includes only fields present in the nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~STUDYID,
    1,   0,     100,  1,    1,     NA,  "STUDY1"
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  expect_true(all(result$name %in% names(test_data)))
  expect_false("USUBJID" %in% result$name)
  expect_true("STUDYID" %in% result$name)
})


test_that("ddt returns unique field names", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~AGE, ~SEX,
    1,   0,     100,  1,    1,     NA,  30,   0,
    1,   1,     0,    2,    0,     10,  30,   0
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  expect_equal(nrow(result), length(unique(result$name)))
})


test_that("ddt preserves standard metadata for known fields", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~STUDYID, ~USUBJID, ~AGE, ~SEX,
    1,   0,     100,  1,    1,     NA,  "STUDY1", "SUBJ-001", 40,  1
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  studyid_row <- result[result$name == "STUDYID", ]
  expect_equal(studyid_row$definition, "Study")
  expect_equal(studyid_row$type, "character")
  expect_equal(studyid_row$source, "DM: STUDYID")

  age_row <- result[result$name == "AGE", ]
  expect_equal(age_row$definition, "Age")
  expect_equal(age_row$unit, "years")

  sex_row <- result[result$name == "SEX", ]
  expect_equal(sex_row$type, "0, 1")
  expect_equal(sex_row$description, "0 = Male, 1 = Female")
})


test_that("ddt derives CMT type and description from analytes and EVID", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",
    1,   1,     0,    1,    0,     10,  "DRUG",
    1,   2,     0,    2,    0,     5,   "MET"
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  cmt_row <- result[result$name == "CMT", ]

  expect_equal(nrow(cmt_row), 1)
  expect_equal(cmt_row$type, "1, 1, 2")
  expect_equal(
    cmt_row$description,
    "1 = DRUG administration, 1 = DRUG observation, 2 = MET observation"
  )
  expect_equal(cmt_row$source, "Produced or assigned")
})


test_that("ddt builds CMT description when ANALYTE is missing", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA,
    1,   1,     0,    2,    0,     10
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  cmt_row <- result[result$name == "CMT", ]

  expect_equal(nrow(cmt_row), 1)
  expect_match(cmt_row$description, "administration")
  expect_match(cmt_row$description, "observation")
  expect_false(identical(cmt_row$description, "PK/PD compartment"))
})


test_that("ddt does not error when CMT contains NA", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",
    1,   1,     0,    NA,   0,     10,  "MET",
    1,   2,     0,    1,    0,     5,   "DRUG"
  ) |> nif()

  expect_no_error(result <- ddt(test_data, silent = TRUE))

  cmt_row <- result[result$name == "CMT", ]
  expect_equal(nrow(cmt_row), 1)
})


test_that("ddt does not error when EVID contains NA", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~ANALYTE,
    1,   0,     100,  1,    1,     NA,  "DRUG",
    1,   1,     0,    2,    NA,    10,  "MET",
    1,   2,     0,    1,    0,     5,   "DRUG"
  ) |> nif()

  expect_no_error(ddt(test_data, silent = TRUE))
})


test_that("ddt preserves standard CMT metadata for empty nif", {
  result <- ddt(nif(), silent = TRUE)
  cmt_row <- result[result$name == "CMT", ]

  expect_equal(nrow(cmt_row), 1)
  expect_equal(cmt_row$type, "numeric")
  expect_equal(cmt_row$description, "PK/PD compartment")
})


test_that("ddt handles empty nif object", {
  result <- ddt(nif(), silent = TRUE)

  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c("name", "definition", "type", "description", "unit", "source")
  )
  expect_true(all(result$name %in% names(nif())))
  expect_equal(nrow(result), length(names(nif())))
})


test_that("ddt annotates numeric RACE with coded labels", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  0,
    2,   0,     100,  1,    1,     NA,  1,
    3,   0,     100,  1,    1,     NA,  2
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  race_row <- result[result$name == "RACE", ]

  expect_equal(nrow(race_row), 1)
  expect_equal(race_row$type, "numeric")
  expect_equal(
    race_row$description,
    "0 = WHITE, 1 = ASIAN, 2 = BLACK OR AFRICAN AMERICAN"
  )
})


test_that("ddt does not annotate integer RACE codes", {
  # inherits(integer, "numeric") is FALSE, so integer RACE keeps catalog defaults
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  0L,
    2,   0,     100,  1,    1,     NA,  1L
  ) |> nif()

  expect_true(is.integer(test_data$RACE))

  result <- ddt(test_data, silent = TRUE)
  race_row <- result[result$name == "RACE", ]

  expect_equal(race_row$type, "character")
  expect_equal(race_row$description, "Race")
})

test_that("ddt handles unmapped numeric RACE codes", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  99,
    2,   0,     100,  1,    1,     NA,  0
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  race_row <- result[result$name == "RACE", ]

  expect_equal(nrow(race_row), 1)
  expect_equal(race_row$type, "numeric")
  expect_equal(race_row$description, "0 = WHITE, 99 = NA")
})


test_that("ddt annotates all mapped numeric RACE codes", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  0,
    2,   0,     100,  1,    1,     NA,  1,
    3,   0,     100,  1,    1,     NA,  2,
    4,   0,     100,  1,    1,     NA,  3,
    5,   0,     100,  1,    1,     NA,  4,
    6,   0,     100,  1,    1,     NA,  5,
    7,   0,     100,  1,    1,     NA,  6,
    8,   0,     100,  1,    1,     NA,  7
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  race_row <- result[result$name == "RACE", ]

  expect_equal(race_row$type, "numeric")
  expect_equal(
    race_row$description,
    paste(
      "0 = WHITE",
      "1 = ASIAN",
      "2 = BLACK OR AFRICAN AMERICAN",
      "3 = AMERICAN INDIAN OR ALASKA NATIVE",
      "4 = NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "5 = NOT REPORTED",
      "6 = UNKNOWN",
      "7 = OTHER",
      sep = ", "
    )
  )
})


test_that("ddt leaves character RACE on standard metadata", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~RACE,
    1,   0,     100,  1,    1,     NA,  "WHITE",
    2,   0,     100,  1,    1,     NA,  "ASIAN"
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  race_row <- result[result$name == "RACE", ]

  expect_equal(nrow(race_row), 1)
  expect_equal(race_row$type, "character")
  expect_equal(race_row$description, "Race")
  expect_equal(race_row$source, "DM: RACE")
})


test_that("ddt adds further fields with detected type and empty metadata", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1",
    1,   1,     0,    2,    0,     10,  "value2"
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  custom_row <- result[result$name == "CUSTOM_FIELD", ]

  expect_equal(nrow(custom_row), 1)
  expect_equal(custom_row$type, "character")
  expect_true(is.na(custom_row$definition))
  expect_true(is.na(custom_row$description))
  expect_true(is.na(custom_row$unit))
  expect_true(is.na(custom_row$source))
})


test_that("ddt detects types for multiple further fields", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~FIELD1, ~FIELD2, ~FIELD3,
    1,   0,     100,  1,    1,     NA,  10,      "A",     TRUE,
    1,   1,     0,    2,    0,     10,  20,      "B",     FALSE
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  expect_equal(result$type[result$name == "FIELD1"], "numeric")
  expect_equal(result$type[result$name == "FIELD2"], "character")
  expect_equal(result$type[result$name == "FIELD3"], "logical")
})


test_that("ddt reports first class for multi-class further fields", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_DTC,
    1,   0,     100,  1,    1,     NA,  as.POSIXct("2024-01-01 08:00:00"),
    1,   1,     0,    2,    0,     10,  as.POSIXct("2024-01-01 12:00:00")
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  custom_row <- result[result$name == "CUSTOM_DTC", ]

  expect_equal(custom_row$type, "POSIXct")
  expect_false(grepl("c\\(", custom_row$type))
})


test_that("ddt keeps standard type for known datetime field DTC", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~DTC,
    1,   0,     100,  1,    1,     NA,  as.POSIXct("2024-01-01 08:00:00"),
    1,   1,     0,    2,    0,     10,  as.POSIXct("2024-01-01 12:00:00")
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)
  dtc_row <- result[result$name == "DTC", ]

  expect_equal(nrow(dtc_row), 1)
  expect_equal(dtc_row$type, "datetime")
  expect_equal(dtc_row$definition, "Datetime")
})


test_that("ddt warns about further fields when silent is FALSE", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1"
  ) |> nif()

  old_silent <- nif_option_value("silent")
  nif_option("silent" = FALSE)
  on.exit(nif_option("silent" = old_silent), add = TRUE)

  expect_message(
    ddt(test_data, silent = FALSE),
    "Some data definition fields need completion"
  )
})


test_that("ddt lists further field names in the completion message", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~FIELD_A, ~FIELD_B,
    1,   0,     100,  1,    1,     NA,  1,        "x"
  ) |> nif()

  old_silent <- nif_option_value("silent")
  nif_option("silent" = FALSE)
  on.exit(nif_option("silent" = old_silent), add = TRUE)

  expect_message(
    ddt(test_data, silent = FALSE),
    "FIELD_A"
  )
  expect_message(
    ddt(test_data, silent = FALSE),
    "FIELD_B"
  )
})


test_that("ddt does not warn when there are no further fields", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA
  ) |> nif()

  old_silent <- nif_option_value("silent")
  nif_option("silent" = FALSE)
  on.exit(nif_option("silent" = old_silent), add = TRUE)

  expect_no_message(ddt(test_data, silent = FALSE))
})


test_that("ddt suppresses completion message when silent is TRUE", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1"
  ) |> nif()

  expect_no_message(ddt(test_data, silent = TRUE))
})


test_that("ddt uses nif_option silent setting when silent is NULL", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM_FIELD,
    1,   0,     100,  1,    1,     NA,  "value1"
  ) |> nif()

  old_silent <- nif_option_value("silent")
  on.exit(nif_option("silent" = old_silent), add = TRUE)

  nif_option("silent" = TRUE)
  expect_no_message(ddt(test_data, silent = NULL))

  nif_option("silent" = FALSE)
  expect_message(
    ddt(test_data, silent = NULL),
    "Some data definition fields need completion"
  )
})


test_that("ddt validates that input is a nif object", {
  test_data <- data.frame(
    ID = 1,
    TIME = 0,
    AMT = 100,
    CMT = 1,
    EVID = 1,
    DV = NA_real_
  )

  expect_error(
    ddt(test_data, silent = TRUE),
    "Input must be a nif object"
  )
})


test_that("ddt validates silent argument", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   0,     100,  1,    1,     NA
  ) |> nif()

  expect_error(
    ddt(test_data, silent = "yes"),
    "silent must be a logical value"
  )
})


test_that("ddt includes standard baseline and derived fields when present", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~HEIGHT, ~WEIGHT, ~BMI, ~DOSE, ~DL,
    1,   0,     100,  1,    1,     NA,  170,     70,      24.2, 100,   100,
    1,   1,     0,    2,    0,     10,  170,     70,      24.2, 100,   100
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  expect_equal(result$unit[result$name == "HEIGHT"], "cm")
  expect_equal(result$unit[result$name == "WEIGHT"], "kg")
  expect_equal(result$unit[result$name == "BMI"], "kg/m^2")
  expect_equal(result$definition[result$name == "DOSE"], "Dose")
  expect_equal(result$definition[result$name == "DL"], "Dose level")
})


test_that("ddt includes renal baseline fields when present", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~BL_CRCL, ~BL_RENAL,
    1,   0,     100,  1,    1,     NA,  80,        90,       "normal"
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  expect_equal(result$unit[result$name == "BL_CREAT"], "umol/l")
  expect_equal(result$unit[result$name == "BL_CRCL"], "ml/min")
  expect_equal(
    result$type[result$name == "BL_RENAL"],
    "normal, mild, moderate, severe"
  )
})


test_that("ddt works on a complex multi-field nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~STUDYID, ~USUBJID,   ~ANALYTE,     ~RACE, ~AGE, ~SEX,
    1,   0,     100,  1,    1,     NA,  "STUDY1", "SUBJ-001", "DRUG",       0,     30,   0,
    1,   1,     0,    2,    0,     10,  "STUDY1", "SUBJ-001", "METABOLITE", 0,     30,   0,
    2,   0,     150,  1,    1,     NA,  "STUDY1", "SUBJ-002", "DRUG",       1,     25,   1,
    2,   1,     0,    2,    0,     15,  "STUDY1", "SUBJ-002", "METABOLITE", 1,     25,   1
  ) |> nif()

  result <- ddt(test_data, silent = TRUE)

  expect_true(all(
    c("ID", "TIME", "AMT", "CMT", "EVID", "DV", "STUDYID",
      "USUBJID", "ANALYTE", "RACE", "AGE", "SEX") %in% result$name
  ))
  expect_equal(nrow(result), length(unique(result$name)))
  expect_equal(
    result$description[result$name == "RACE"],
    "0 = WHITE, 1 = ASIAN"
  )
})
