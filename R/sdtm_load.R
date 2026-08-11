#' Read SDTM data
#'
#' This function reads SDTM-formatted data as SAS or XPT files from a folder
#' location.
#'
#' @param data_path The file system path to the source folder as character.
#' @param domain The domain name(s) as character, defaults to
#'   all domains found in the folder.
#' @param format The format of the source files as character, either 'sas'
#'   (default), 'xpt', or 'csv'.
#' @param ... Further parameters, refer to readr::read_csv
#' @param delim Deliminator.
#' @return A `sdtm` object.
#' @import readr
#' @import haven
#' @export
read_sdtm <- function(
    data_path,
    domain = NULL,
    format = "sas",
    delim = ",",
    ...
) {
  # validate input
  validate_argument(data_path, "character")
  validate_argument(domain, "character", allow_null = TRUE, allow_multiple = TRUE)
  validate_argument(format, "character", values = c("sas", "xpt", "csv"))

  # Validate data_path
  if (!dir.exists(data_path)) {
    stop("data_path does not exist: ", data_path, call. = FALSE)
  }

  # Get file extension based on format
  file_ext <- switch(format,
    "sas" = ".sas7bdat",
    "xpt" = ".xpt",
    "csv" = ".csv"
  )

  # domain auto discovery
  if (is.null(domain)) {
    temp <- list.files(file.path(data_path), pattern = paste0(".*\\", file_ext))
    domain <- gsub(paste0("^(.*)\\", file_ext), "\\1", temp)
    # domain names starting with an underscore are omitted
    domain <- domain[substring(domain, 1, 1) != "_"]
  }

  resolve_domain_file <- function(data_path, domain, file_ext) {
    wanted <- paste0(tolower(domain), tolower(file_ext))
    files <- list.files(data_path)
    hit <- files[tolower(files) == wanted]
    if (length(hit) == 0) {
      return(NA_character_)
    }
    if (length(hit) > 1) {
      warning(paste0(
        "Multiple hits for ", domain, " (", nice_enumeration(hit), "), ",
        "Selected ", hit[1]
      ), call. = FALSE)
      hit <- hit[[1]]
    }
    file.path(data_path, hit)
  }

  # Resolve each domain file once (avoids repeated multi-hit warnings)
  domain_files <- setNames(
    vapply(
      domain,
      function(x) resolve_domain_file(data_path, x, file_ext),
      character(1)
    ),
    domain
  )

  missing_domains <- names(domain_files)[
    is.na(domain_files) | !file.exists(domain_files)
  ]
  if (length(missing_domains) > 0) {
    stop(
      "The following files do not exist:\n",
      paste0(missing_domains, file_ext, collapse = "\n"),
      call. = FALSE
    )
  }

  out <- list()
  if (format == "sas") {
    for (x in domain) {
      out[[tolower(x)]] <- as.data.frame(
        haven::read_sas(domain_files[[x]], ...)
      )
    }
  }
  if (format == "xpt") {
    for (x in domain) {
      out[[tolower(x)]] <- as.data.frame(
        haven::read_xpt(domain_files[[x]], ...)
      )
    }
  }
  if (format == "csv") {
    for (x in domain) {
      out[[tolower(x)]] <- as.data.frame(
        readr::read_delim(
          domain_files[[x]],
          delim = delim,
          show_col_types = FALSE,
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
