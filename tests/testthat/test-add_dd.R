# Test file for add_dd function

test_that("add_dd updates an existing field without duplicating", {
  ddt_table <- tibble::tribble(
    ~name,    ~definition,          ~type,       ~description,        ~unit, ~source,
    "ID",     "Subject identifier", "numeric",   "Unique subject ID", NA,    "Produced",
    "CUSTOM", NA_character_,        "character", NA_character_,       NA,    NA_character_
  )

  result <- add_dd(
    ddt_table,
    name = "CUSTOM",
    definition = "Custom field",
    type = "character",
    description = "A custom covariate",
    unit = NA_character_,
    source = "manual"
  )

  custom_row <- result[result$name == "CUSTOM", ]

  expect_equal(nrow(result), 2)
  expect_equal(nrow(custom_row), 1)
  expect_equal(custom_row$definition, "Custom field")
  expect_equal(custom_row$type, "character")
  expect_equal(custom_row$description, "A custom covariate")
  expect_true(is.na(custom_row$unit))
  expect_equal(custom_row$source, "manual")
})


test_that("add_dd overwrites all metadata columns for an existing field", {
  ddt_table <- tibble::tribble(
    ~name, ~definition, ~type,     ~description, ~unit, ~source,
    "AGE", "Age",       "numeric", "Age years",  "y",   "DM"
  )

  result <- add_dd(
    ddt_table,
    name = "AGE",
    definition = "Subject age",
    type = "integer",
    description = "Age at screening",
    unit = "years",
    source = "DM: AGE"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$name, "AGE")
  expect_equal(result$definition, "Subject age")
  expect_equal(result$type, "integer")
  expect_equal(result$description, "Age at screening")
  expect_equal(result$unit, "years")
  expect_equal(result$source, "DM: AGE")
})


test_that("add_dd appends a new field when name is absent", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  result <- add_dd(
    ddt_table,
    name = "CUSTOM",
    definition = "Custom field",
    type = "character",
    description = "A custom covariate",
    unit = "mg",
    source = "manual"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("ID", "CUSTOM"))

  custom_row <- result[result$name == "CUSTOM", ]
  expect_equal(custom_row$definition, "Custom field")
  expect_equal(custom_row$type, "character")
  expect_equal(custom_row$description, "A custom covariate")
  expect_equal(custom_row$unit, "mg")
  expect_equal(custom_row$source, "manual")
})


test_that("add_dd uses default unit and source when omitted", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  result <- add_dd(
    ddt_table,
    name = "CUSTOM",
    definition = "Custom field",
    type = "character",
    description = "A custom covariate"
  )

  custom_row <- result[result$name == "CUSTOM", ]

  expect_true(is.na(custom_row$unit))
  expect_equal(custom_row$source, "")
})


test_that("add_dd allows empty source", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  result <- add_dd(
    ddt_table,
    name = "CUSTOM",
    definition = "Custom field",
    type = "character",
    description = "A custom covariate",
    source = ""
  )

  expect_equal(result$source[result$name == "CUSTOM"], "")
})


test_that("add_dd allows NA unit", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  result <- add_dd(
    ddt_table,
    name = "CUSTOM",
    definition = "Custom field",
    type = "character",
    description = "A custom covariate",
    unit = NA_character_,
    source = "manual"
  )

  expect_true(is.na(result$unit[result$name == "CUSTOM"]))
})


test_that("add_dd preserves unrelated rows when updating", {
  ddt_table <- tibble::tribble(
    ~name,    ~definition,          ~type,       ~description,        ~unit, ~source,
    "ID",     "Subject identifier", "numeric",   "Unique subject ID", NA,    "Produced",
    "CUSTOM", NA_character_,        "character", NA_character_,       NA,    NA_character_,
    "SCORE",  "Score",              "numeric",   "Clinical score",    NA,    "QS"
  )

  result <- add_dd(
    ddt_table,
    name = "CUSTOM",
    definition = "Custom field",
    type = "character",
    description = "A custom covariate",
    source = "manual"
  )

  expect_equal(result$definition[result$name == "ID"], "Subject identifier")
  expect_equal(result$definition[result$name == "SCORE"], "Score")
  expect_equal(result$source[result$name == "SCORE"], "QS")
})


test_that("add_dd can complete a stub produced by ddt", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~CUSTOM,
    1,   0,     100,  1,    1,     NA,  "a",
    1,   1,     0,    2,    0,     10,  "b"
  ) |> nif()

  result <- ddt(test_data, silent = TRUE) |>
    add_dd(
      name = "CUSTOM",
      definition = "Custom field",
      type = "character",
      description = "A custom covariate",
      source = "manual"
    )

  custom_row <- result[result$name == "CUSTOM", ]

  expect_equal(nrow(custom_row), 1)
  expect_equal(custom_row$definition, "Custom field")
  expect_equal(custom_row$description, "A custom covariate")
  expect_equal(custom_row$source, "manual")
})


test_that("add_dd returns a data frame with required columns", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  result <- add_dd(
    ddt_table,
    name = "CUSTOM",
    definition = "Custom field",
    type = "character",
    description = "A custom covariate"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("name", "definition", "type", "description", "unit", "source") %in% names(result)
  ))
})


test_that("add_dd rejects non-data-frame input", {
  expect_error(
    add_dd(
      list(name = "ID"),
      name = "CUSTOM",
      definition = "Custom field",
      type = "character",
      description = "A custom covariate"
    ),
    "must be a data.frame"
  )
})


test_that("add_dd rejects data frames missing required columns", {
  bad_table <- tibble::tribble(
    ~name, ~definition,
    "ID",  "Subject identifier"
  )

  expect_error(
    add_dd(
      bad_table,
      name = "CUSTOM",
      definition = "Custom field",
      type = "character",
      description = "A custom covariate"
    ),
    "Missing columns in obj"
  )
})


test_that("add_dd validates name is character", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  expect_error(
    add_dd(
      ddt_table,
      name = 1,
      definition = "Custom field",
      type = "character",
      description = "A custom covariate"
    ),
    "name must be a character value"
  )
})


test_that("add_dd validates definition is character", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  expect_error(
    add_dd(
      ddt_table,
      name = "CUSTOM",
      definition = 1,
      type = "character",
      description = "A custom covariate"
    ),
    "definition must be a character value"
  )
})


test_that("add_dd validates type is character", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  expect_error(
    add_dd(
      ddt_table,
      name = "CUSTOM",
      definition = "Custom field",
      type = 1,
      description = "A custom covariate"
    ),
    "type must be a character value"
  )
})


test_that("add_dd validates description is character", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  expect_error(
    add_dd(
      ddt_table,
      name = "CUSTOM",
      definition = "Custom field",
      type = "character",
      description = 1
    ),
    "description must be a character value"
  )
})


test_that("add_dd validates unit is character", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  expect_error(
    add_dd(
      ddt_table,
      name = "CUSTOM",
      definition = "Custom field",
      type = "character",
      description = "A custom covariate",
      unit = 1
    ),
    "unit must be a character value"
  )
})


test_that("add_dd validates source is character", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  expect_error(
    add_dd(
      ddt_table,
      name = "CUSTOM",
      definition = "Custom field",
      type = "character",
      description = "A custom covariate",
      source = 1
    ),
    "source must be a character value"
  )
})


test_that("add_dd rejects NA source", {
  ddt_table <- tibble::tribble(
    ~name, ~definition,          ~type,     ~description,       ~unit, ~source,
    "ID",  "Subject identifier", "numeric", "Unique subject ID", NA,    "Produced"
  )

  expect_error(
    add_dd(
      ddt_table,
      name = "CUSTOM",
      definition = "Custom field",
      type = "character",
      description = "A custom covariate",
      source = NA_character_
    ),
    "source must not contain NA"
  )
})
