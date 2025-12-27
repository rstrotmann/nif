#' Read SDTM data
#'
#' This function reads SDTM-formatted data as SAS or XPT files from a folder
#' location.
#'
#' @param data_path The file system path to the source folder as character.
#' @param domain The domain name(s) as character, defaults to
#'   all domains found in the folder.
#' @param format The format of the source files as character, either 'sas'
#'   (default), 'xpt', 'xlsx', or 'csv'.
#' @param ... Further parameters, refer to readr::read_csv and
#'   readxl::read_xlsx.
#' @param delim Deliminator.
#' @return A `sdtm` object.
#' @import readr
#' @import haven
#' @import readxl
#' @export
read_sdtm <- function(data_path,
                      domain = NULL,
                      format = "sas", delim = ",", ...) {
  # validate input
  validate_char_param(data_path, "data_path")
  validate_char_param(domain, "domain", allow_null = TRUE, allow_multiple = TRUE)
  validate_char_param(format, "format")

  # Validate data_path
  if (!dir.exists(data_path)) {
    stop("data_path does not exist: ", data_path, call. = FALSE)
  }

  # Validate format
  valid_formats <- c("sas", "xpt", "csv", "xlsx")
  if (!format %in% valid_formats) {
    stop("format must be one of: ", paste(valid_formats, collapse = ", "), call. = FALSE)
  }

  # Validate domain
  # if (!is.character(domain) || length(domain) == 0) {
  #   stop("domain must be a non-empty character vector", call. = FALSE)
  # }

  # Get file extension based on format
  file_ext <- switch(format,
    "sas" = ".sas7bdat",
    "xpt" = ".xpt",
    "csv" = ".csv",
    "xlsx" = ".xlsx"
  )

  # set domains
  if (is.null(domain)) {
    temp <- list.files(file.path(data_path), pattern = paste0(".*\\", file_ext))
    domain <- gsub(paste0("^(.*)\\", file_ext), "\\1", temp)
    # domain names starting with an underscore are omitted
    domain <- domain[substring(domain, 1, 1) != "_"]
  }

  # Check if all required files exist
  missing_files <- character()
  for (x in domain) {
    file_path <- file.path(data_path, paste0(x, file_ext))
    if (!file.exists(file_path)) {
      missing_files <- c(missing_files, file_path)
    }
  }

  if (length(missing_files) > 0) {
    stop("The following files do not exist:\n",
      paste(missing_files, collapse = "\n"),
      call. = FALSE
    )
  }

  out <- list()
  if (format == "sas") {
    for (x in domain) {
      out[[x]] <- as.data.frame(haven::read_sas(
        file.path(data_path, paste0(x, ".sas7bdat"))
      ))
    }
  }
  if (format == "xpt") {
    for (x in domain) {
      out[[x]] <- as.data.frame(haven::read_xpt(
        file.path(data_path, paste0(x, ".xpt"))
      ))
    }
  }
  if (format == "csv") {
    for (x in domain) {
      out[[x]] <- as.data.frame(
        # readr::read_delim(
        readr::read_delim(
          file.path(data_path, paste0(x, ".csv")),
          delim = delim,
          show_col_types = FALSE
        )
      )
    }
  }
  if (format == "xlsx") {
    for (x in domain) {
      out[[x]] <- as.data.frame(
        readxl::read_xlsx(
          file.path(data_path, paste0(x, ".xlsx")),
          ...
        )
      )
    }
  }

  if (length(out) == 0) {
    stop("no domain data found")
  }
  sdtm(out)
}
