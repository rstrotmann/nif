read_adam <- function(
  data_path,
  dataset = NULL,
  format = "sas", delim = ",", ...
) {
  # validate input
  validate_char_param(data_path, "data_path")
  validate_char_param(dataset, "dataset",
    allow_null = TRUE,
    allow_multiple = TRUE
  )
  validate_char_param(format, "format")

  # Validate data_path
  if (!dir.exists(data_path)) {
    stop("data_path does not exist: ", data_path, call. = FALSE)
  }

  # Validate format
  valid_formats <- c("sas", "xpt", "csv")
  if (!format %in% valid_formats) {
    stop("format must be one of: ", paste(valid_formats, collapse = ", "),
      call. = FALSE
    )
  }

  # Get file extension based on format
  file_ext <- switch(format,
    "sas" = ".sas7bdat",
    "xpt" = ".xpt",
    "csv" = ".csv"
  )

  available_file_name <- list.files(file.path(data_path), pattern = paste0(".*\\", file_ext))
  available_file_stem <- gsub(paste0("^(.*)\\", file_ext), "\\1", available_file_name)

  # set data sets
  if (is.null(dataset)) {
    dataset <- available_file_stem
    # dataset names starting with an underscore are omitted
    dataset <- dataset[substring(dataset, 1, 1) != "_"]
  }

  missing_files <- dataset[!(toupper(dataset) %in% available_file_stem)]
  if (length(missing_files) > 0)
    conditional_cli(
      cli_alert_warning(paste0(
        "Missing files: ", nice_enumeration(missing_files)
      )), silent = FALSE
    )

  active_file_name <- available_file_name[toupper(available_file_stem) %in% toupper(dataset)]

  out <- list()

  for (x in active_file_name) {
    dataset_name <- tolower(gsub(paste0("^(.*)\\", file_ext), "\\1", x))

    if (format == "sas")
      out[[dataset_name]] <- as.data.frame(haven::read_sas(
        file.path(data_path, x)))

    if (format == "xpt")
      out[[dataset_name]] <- as.data.frame(haven::read_xpt(
        file.path(data_path, x)))

    if (format == "csv")
      out[[dataset_name]] <- as.data.frame(readr::read_delim(
        file.path(data_path, x), delim = delim, show_col_types = FALSE))
  }

  if (length(out) == 0) {
    stop("no dataset found")
  }
  adam(out)
}
