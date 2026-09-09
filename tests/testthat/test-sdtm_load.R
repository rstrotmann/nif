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


vs_test_data <- function() {
  tibble::tribble(
    ~USUBJID, ~VSTEST,
    "001",    "BP",
    "002",    "HR"
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


with_mocked_progress <- function(code) {
  state <- new.env(parent = emptyenv())
  state$bar <- NULL
  state$updates <- list()
  state$done <- 0L

  local_mocked_bindings(
    cli_progress_bar = function(...) {
      state$bar <- list(...)
      1L
    },
    cli_progress_update = function(...) {
      state$updates <- c(state$updates, list(list(...)))
      invisible(NULL)
    },
    cli_progress_done = function(...) {
      state$done <- state$done + 1L
      invisible(NULL)
    },
    .package = "cli"
  )

  result <- force(code)
  list(
    result = result,
    bar = state$bar,
    updates = state$updates,
    done = state$done
  )
}


progress_incs <- function(updates) {
  vapply(
    updates,
    function(update) {
      inc <- update$inc
      if (is.null(inc)) NA_real_ else as.numeric(inc)
    },
    numeric(1)
  )
}


progress_extras <- function(updates) {
  Filter(function(update) !is.null(update$extra), updates)
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
  expect_error(
    read_sdtm(""),
    "data_path must be a non-empty string"
  )
  expect_error(
    read_sdtm(c(tempdir(), tempdir())),
    "data_path must be a single value"
  )

  path_is_file <- tempfile("sdtm_file")
  writeLines("not a directory", path_is_file)
  on.exit(unlink(path_is_file), add = TRUE)
  expect_error(
    read_sdtm(path_is_file),
    "data_path does not exist"
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
  expect_error(
    read_sdtm(tempdir(), format = ""),
    "format must be a non-empty string"
  )
  expect_error(
    read_sdtm(tempdir(), format = c("csv", "sas")),
    "format must be a single value"
  )
  expect_error(
    read_sdtm(tempdir(), format = NA_character_),
    "format must not contain NA"
  )
  expect_error(
    read_sdtm(tempdir(), format = "SAS"),
    "format must be sas, xpt or csv!"
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
  expect_error(
    read_sdtm(tempdir(), domain = NA_character_),
    "domain must not contain NA"
  )
  expect_error(
    read_sdtm(tempdir(), domain = c("dm", "")),
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


test_that("read_sdtm is silent when silent is TRUE", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  expect_silent(
    result <- read_sdtm(
      test_dir,
      domain = "dm",
      format = "csv",
      silent = TRUE
    )
  )
  expect_s3_class(result, "sdtm")
  expect_equal(names(result$domains), "dm")
})


test_that("read_sdtm validates silent", {
  expect_error(
    read_sdtm(tempdir(), silent = "yes"),
    "silent must be a logical value"
  )
  expect_error(
    read_sdtm(tempdir(), silent = c(TRUE, FALSE)),
    "silent must be a single value"
  )
  expect_error(
    read_sdtm(tempdir(), silent = NA),
    "silent must not contain NA"
  )
  expect_error(
    read_sdtm(tempdir(), silent = 1),
    "silent must be a logical value"
  )
})


test_that("read_sdtm errors when only underscore-prefixed files are present", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "_meta", dm_test_data())

  expect_error(
    read_sdtm(test_dir, format = "csv"),
    "no domain data found"
  )
})


test_that("read_sdtm does not auto-discover files with other extensions", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())
  writeLines("not sdtm", file.path(test_dir, "ex.txt"))
  writeLines("not csv", file.path(test_dir, "vs.sas7bdat"))
  writeLines("notes", file.path(test_dir, "README.md"))

  result <- read_sdtm(test_dir, format = "csv", silent = TRUE)
  expect_equal(names(result$domains), "dm")
})


test_that("read_sdtm does not auto-discover domain files in subdirectories", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())
  subdir <- file.path(test_dir, "nested")
  dir.create(subdir)
  write_domain_csv(subdir, "vs", vs_test_data())

  result <- read_sdtm(test_dir, format = "csv", silent = TRUE)
  expect_equal(names(result$domains), "dm")
  expect_false("vs" %in% names(result$domains))
})


test_that("read_sdtm auto-discovers xpt files", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  haven::write_xpt(dm_test_data(), file.path(test_dir, "dm.xpt"))
  haven::write_xpt(vs_test_data(), file.path(test_dir, "vs.xpt"))
  write_domain_csv(test_dir, "ex", dm_test_data())

  result <- read_sdtm(test_dir, format = "xpt", silent = TRUE)
  expect_setequal(names(result$domains), c("dm", "vs"))
  expect_equal(nrow(result$domains$dm), 2)
  expect_equal(nrow(result$domains$vs), 2)
})


test_that("read_sdtm loads only the requested domains", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())
  write_domain_csv(test_dir, "vs", vs_test_data())
  write_domain_csv(test_dir, "ex", dm_test_data())

  result <- read_sdtm(
    test_dir,
    domain = c("ex", "dm"),
    format = "csv",
    silent = TRUE
  )
  expect_equal(names(result$domains), c("ex", "dm"))
  expect_false("vs" %in% names(result$domains))
})


test_that("read_sdtm errors for missing files using the requested extension", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_error(
    read_sdtm(test_dir, domain = "dm", format = "sas"),
    "The following files do not exist:\ndm.sas7bdat"
  )
  expect_error(
    read_sdtm(test_dir, domain = "dm", format = "xpt"),
    "The following files do not exist:\ndm.xpt"
  )
})


test_that("read_sdtm errors when some requested domain files are missing", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  expect_error(
    read_sdtm(test_dir, domain = c("dm", "vs"), format = "csv"),
    "The following files do not exist:\nvs.csv"
  )
})


test_that("read_sdtm reads an empty CSV domain", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  writeLines("USUBJID,SEX", file.path(test_dir, "dm.csv"))

  result <- read_sdtm(
    test_dir,
    domain = "dm",
    format = "csv",
    silent = TRUE
  )
  expect_s3_class(result, "sdtm")
  expect_equal(nrow(result$domains$dm), 0)
  expect_equal(names(result$domains$dm), c("USUBJID", "SEX"))
})


test_that("read_sdtm preserves mixed-case explicit domain as lowercase", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  result <- read_sdtm(
    test_dir,
    domain = "Dm",
    format = "csv",
    silent = TRUE
  )
  expect_equal(names(result$domains), "dm")
  expect_equal(nrow(domain(result, "dm")), 2)
})


test_that("read_sdtm returns a sdtm object with data-frame domains", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  result <- read_sdtm(
    test_dir,
    domain = "dm",
    format = "csv",
    silent = TRUE
  )
  expect_s3_class(result, "sdtm")
  expect_true(inherits(result, "list"))
  expect_true(is.data.frame(result$domains$dm))
  expect_equal(result$domains$dm$USUBJID, c("001", "002"))
  expect_equal(result$domains$dm$SEX, c("M", "F"))
})


test_that("read_sdtm does not create a progress bar when silent is TRUE", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  captured <- with_mocked_progress({
    read_sdtm(test_dir, domain = "dm", format = "csv", silent = TRUE)
  })

  expect_null(captured$bar)
  expect_equal(length(captured$updates), 0)
  expect_equal(captured$done, 0)
  expect_s3_class(captured$result, "sdtm")
})


test_that("read_sdtm uses a size-weighted progress bar", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  dm_path <- write_domain_csv(test_dir, "dm", dm_test_data())
  vs_path <- write_domain_csv(test_dir, "vs", vs_test_data())
  dm_size <- file.info(dm_path)$size
  vs_size <- file.info(vs_path)$size

  captured <- with_mocked_progress({
    read_sdtm(
      test_dir,
      domain = c("dm", "vs"),
      format = "csv",
      silent = FALSE
    )
  })

  expect_equal(captured$bar$total, dm_size + vs_size)
  expect_match(captured$bar$format, "pb_extra\\$domain")
  expect_match(captured$bar$format, "pb_extra\\$size")
  expect_true(captured$done >= 1)

  extras <- progress_extras(captured$updates)
  expect_equal(length(extras), 2)
  expect_equal(extras[[1]]$extra$domain, "DM")
  expect_equal(extras[[1]]$extra$size, format_bytes(dm_size))
  expect_equal(extras[[2]]$extra$domain, "VS")
  expect_equal(extras[[2]]$extra$size, format_bytes(vs_size))

  expect_equal(
    progress_incs(captured$updates),
    c(0, dm_size, 0, vs_size)
  )
  expect_equal(names(captured$result$domains), c("dm", "vs"))
})


test_that("read_sdtm follows the package silent option when silent is NULL", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  local_mocked_bindings(
    nif_option_value = function(option) {
      identical(option, "silent")
    },
    .package = "nif"
  )

  captured <- with_mocked_progress({
    read_sdtm(test_dir, domain = "dm", format = "csv", silent = NULL)
  })

  expect_null(captured$bar)
  expect_s3_class(captured$result, "sdtm")
})


test_that("read_sdtm silent argument overrides the package silent option", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  local_mocked_bindings(
    nif_option_value = function(option) TRUE,
    .package = "nif"
  )

  captured <- with_mocked_progress({
    read_sdtm(test_dir, domain = "dm", format = "csv", silent = FALSE)
  })

  expect_false(is.null(captured$bar))
  expect_true(captured$done >= 1)
  expect_s3_class(captured$result, "sdtm")
})


test_that("read_sdtm falls back to a count-based bar when file sizes are zero", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())
  write_domain_csv(test_dir, "vs", vs_test_data())

  local_mocked_bindings(
    file.info = function(path, extra_cols = TRUE) {
      data.frame(
        size = 0,
        isdir = FALSE,
        mode = 0,
        mtime = Sys.time(),
        ctime = Sys.time(),
        atime = Sys.time(),
        uid = 0,
        gid = 0,
        uname = "",
        grname = "",
        exe = "no",
        row.names = as.character(path)
      )
    },
    .package = "base"
  )

  captured <- with_mocked_progress({
    read_sdtm(
      test_dir,
      domain = c("dm", "vs"),
      format = "csv",
      silent = FALSE
    )
  })

  expect_equal(captured$bar$total, 2)
  extras <- progress_extras(captured$updates)
  expect_equal(extras[[1]]$extra$size, format_bytes(0))
  expect_equal(extras[[2]]$extra$size, format_bytes(0))
  expect_equal(progress_incs(captured$updates), c(0, 1, 0, 1))
})


test_that("read_sdtm treats missing file.info sizes as zero", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  local_mocked_bindings(
    file.info = function(path, extra_cols = TRUE) {
      data.frame(
        size = NA_real_,
        isdir = FALSE,
        mode = 0,
        mtime = Sys.time(),
        ctime = Sys.time(),
        atime = Sys.time(),
        uid = 0,
        gid = 0,
        uname = "",
        grname = "",
        exe = "no",
        row.names = as.character(path)
      )
    },
    .package = "base"
  )

  captured <- with_mocked_progress({
    read_sdtm(test_dir, domain = "dm", format = "csv", silent = FALSE)
  })

  expect_equal(captured$bar$total, 1)
  extras <- progress_extras(captured$updates)
  expect_equal(extras[[1]]$extra$size, "0 B")
  expect_equal(progress_incs(captured$updates), c(0, 1))
})


test_that("read_sdtm completes the progress bar if reading fails", {
  test_dir <- make_sdtm_dir()
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  write_domain_csv(test_dir, "dm", dm_test_data())

  local_mocked_bindings(
    read_delim = function(...) stop("read failed"),
    .package = "readr"
  )

  captured <- with_mocked_progress({
    expect_error(
      read_sdtm(test_dir, domain = "dm", format = "csv", silent = FALSE),
      "read failed"
    )
  })

  expect_false(is.null(captured$bar))
  expect_true(captured$done >= 1)
})
