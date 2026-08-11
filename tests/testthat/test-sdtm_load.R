make_sdtm_dir <- function() {
  test_dir <- tempfile("sdtm_test")
  dir.create(test_dir)
  test_dir
}


write_domain_csv <- function(test_dir, domain, data, delim = ",") {
  path <- file.path(test_dir, paste0(domain, ".csv"))
  if (identical(delim, ";")) {
    write.csv2(data, path, row.names = FALSE)
  } else {
    write.csv(data, path, row.names = FALSE)
  }
  path
}


dm_test_data <- function() {
  tibble::tribble(
    ~USUBJID, ~SEX,
    "001",    "M",
    "002",    "F"
  )
}


case_sensitive_file_exists <- function(x) {
  vapply(
    as.character(x),
    function(path) basename(path) %in% list.files(dirname(path)),
    logical(1),
    USE.NAMES = FALSE
  )
}


test_that("read_sdtm validates data_path", {
  expect_error(
    read_sdtm("nonexistent/path"),
    "data_path does not exist"
  )
  expect_error(
    read_sdtm(NULL),
    "data_path must not be NULL"
  )
  expect_error(
    read_sdtm(123),
    "data_path must be a character value"
  )
})


test_that("read_sdtm validates format", {
  expect_error(
    read_sdtm(tempdir(), format = "invalid"),
    "format must be sas, xpt or csv!"
  )
  expect_error(
    read_sdtm(tempdir(), format = "xlsx"),
    "format must be sas, xpt or csv!"
  )
  expect_error(
    read_sdtm(tempdir(), format = NULL),
    "format must not be NULL"
  )
})


test_that("read_sdtm validates domain argument", {
  expect_error(
    read_sdtm(tempdir(), domain = 123),
    "domain must be a character value"
  )
  expect_error(
    read_sdtm(tempdir(), domain = ""),
    "domain must be a non-empty string"
  )
})


test_that("read_sdtm errors when auto-discovery finds no domain files", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_error(
    read_sdtm(test_dir, format = "csv"),
    "no domain data found"
  )
  expect_error(
    read_sdtm(test_dir, domain = character(0), format = "csv"),
    "no domain data found"
  )
})


test_that("read_sdtm errors with clear message when named domain file is missing", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_error(
    read_sdtm(test_dir, domain = "dm", format = "csv"),
    "The following files do not exist:\ndm.csv"
  )
  expect_error(
    read_sdtm(test_dir, domain = c("dm", "vs"), format = "csv"),
    "dm.csv"
  )
  expect_error(
    read_sdtm(test_dir, domain = c("dm", "vs"), format = "csv"),
    "vs.csv"
  )
})


test_that("read_sdtm reads sas, xpt, and csv formats", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  test_data <- dm_test_data()

  suppressWarnings(
    haven::write_sas(test_data, file.path(test_dir, "dm.sas7bdat"))
  )
  result_sas <- read_sdtm(test_dir, domain = "dm", format = "sas")
  expect_s3_class(result_sas, "sdtm")
  expect_equal(names(result_sas$domains), "dm")
  expect_equal(nrow(result_sas$domains$dm), 2)
  expect_equal(result_sas$domains$dm$USUBJID, c("001", "002"))

  haven::write_xpt(test_data, file.path(test_dir, "dm.xpt"))
  result_xpt <- read_sdtm(test_dir, domain = "dm", format = "xpt")
  expect_s3_class(result_xpt, "sdtm")
  expect_equal(nrow(result_xpt$domains$dm), 2)

  write_domain_csv(test_dir, "dm", test_data)
  result_csv <- read_sdtm(test_dir, domain = "dm", format = "csv")
  expect_s3_class(result_csv, "sdtm")
  expect_equal(nrow(result_csv$domains$dm), 2)
  expect_equal(result_csv$domains$dm$SEX, c("M", "F"))
})


test_that("read_sdtm reads multiple explicitly requested domains", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  dm_data <- dm_test_data()
  vs_data <- tibble::tribble(
    ~USUBJID, ~VSTEST,
    "001",    "BP",
    "002",    "HR"
  )

  suppressWarnings({
    haven::write_sas(dm_data, file.path(test_dir, "dm.sas7bdat"))
    haven::write_sas(vs_data, file.path(test_dir, "vs.sas7bdat"))
  })

  result <- read_sdtm(test_dir, domain = c("dm", "vs"), format = "sas")
  expect_s3_class(result, "sdtm")
  expect_equal(names(result$domains), c("dm", "vs"))
  expect_equal(nrow(result$domains$dm), 2)
  expect_equal(nrow(result$domains$vs), 2)
})


test_that("read_sdtm auto-discovers domains and lowercases names", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "DM", dm_test_data())
  write_domain_csv(
    test_dir,
    "vs",
    tibble::tribble(
      ~USUBJID, ~VSTEST,
      "001",    "BP"
    )
  )

  result <- read_sdtm(test_dir, format = "csv")
  expect_s3_class(result, "sdtm")
  expect_setequal(names(result$domains), c("dm", "vs"))
  expect_equal(nrow(domain(result, "dm")), 2)
  expect_equal(nrow(domain(result, "vs")), 1)
})


test_that("read_sdtm omits underscore-prefixed files during auto-discovery", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())
  write_domain_csv(test_dir, "_meta", dm_test_data())

  result <- read_sdtm(test_dir, format = "csv")
  expect_equal(names(result$domains), "dm")
  expect_false("_meta" %in% names(result$domains))
})


test_that("read_sdtm handles custom CSV delimiters", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data(), delim = ";")

  result <- read_sdtm(test_dir, domain = "dm", format = "csv", delim = ";")
  expect_s3_class(result, "sdtm")
  expect_equal(nrow(result$domains$dm), 2)
  expect_equal(result$domains$dm$USUBJID, c("001", "002"))
})


test_that("read_sdtm forwards additional arguments to the reader", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  result <- read_sdtm(
    test_dir,
    domain = "dm",
    format = "csv",
    locale = readr::locale(encoding = "UTF-8")
  )
  expect_s3_class(result, "sdtm")
  expect_equal(nrow(result$domains$dm), 2)

  expect_error(
    read_sdtm(
      test_dir,
      domain = "dm",
      format = "csv",
      totally_fake_arg = TRUE
    ),
    "unused argument"
  )
})


test_that("read_sdtm finds uppercase domain files when domain is lowercase", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write.csv(dm_test_data(), file.path(test_dir, "DM.csv"), row.names = FALSE)

  local_mocked_bindings(
    file.exists = case_sensitive_file_exists,
    .package = "base"
  )

  result <- read_sdtm(test_dir, domain = "dm", format = "csv")
  expect_s3_class(result, "sdtm")
  expect_equal(names(result$domains), "dm")
  expect_equal(nrow(result$domains$dm), 2)
})


test_that("read_sdtm warns once when multiple case-variant domain files match", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write.csv(dm_test_data(), file.path(test_dir, "DM.csv"), row.names = FALSE)

  list_files_orig <- base::list.files
  test_dir_norm <- normalizePath(test_dir, winslash = "/", mustWork = TRUE)

  local_mocked_bindings(
    list.files = function(
      path = ".",
      pattern = NULL,
      all.files = FALSE,
      full.names = FALSE,
      recursive = FALSE,
      ignore.case = FALSE,
      include.dirs = FALSE,
      no.. = FALSE
    ) {
      path_norm <- normalizePath(
        as.character(path)[[1]],
        winslash = "/",
        mustWork = FALSE
      )
      if (identical(path_norm, test_dir_norm)) {
        return(c("DM.csv", "dm.csv"))
      }
      list_files_orig(
        path = path,
        pattern = pattern,
        all.files = all.files,
        full.names = full.names,
        recursive = recursive,
        ignore.case = ignore.case,
        include.dirs = include.dirs,
        no.. = no..
      )
    },
    file.exists = function(x) {
      vapply(
        as.character(x),
        function(path) basename(path) %in% list_files_orig(dirname(path)),
        logical(1),
        USE.NAMES = FALSE
      )
    },
    .package = "base"
  )

  warnings <- capture_warnings(
    result <- read_sdtm(test_dir, domain = "dm", format = "csv")
  )

  expect_equal(length(warnings), 1)
  expect_match(warnings, "Multiple hits for dm")
  expect_match(warnings, "DM.csv")
  expect_match(warnings, "dm.csv")
  expect_match(warnings, "Selected DM.csv")

  expect_s3_class(result, "sdtm")
  expect_equal(names(result$domains), "dm")
  expect_equal(nrow(result$domains$dm), 2)
})


test_that("read_sdtm stores explicitly uppercase domain names as lowercase", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write.csv(dm_test_data(), file.path(test_dir, "DM.csv"), row.names = FALSE)

  result <- read_sdtm(test_dir, domain = "DM", format = "csv")
  expect_equal(names(result$domains), "dm")
  expect_equal(nrow(domain(result, "dm")), 2)
})


test_that("read_sdtm initializes empty mapping tables on the sdtm object", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  result <- read_sdtm(test_dir, domain = "dm", format = "csv")
  expect_true(is.data.frame(result$analyte_mapping))
  expect_true(is.data.frame(result$metabolite_mapping))
  expect_true(is.data.frame(result$parent_mapping))
  expect_true(is.data.frame(result$time_mapping))
  expect_equal(nrow(result$analyte_mapping), 0)
})
