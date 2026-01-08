# Create package environment for global variables

.nif_env <- new.env(parent = emptyenv())
assign("silent", FALSE, envir = .nif_env)
assign("show_hash", FALSE, envir = .nif_env)
assign("abbreviation_threshold", 15, envir = .nif_env)
assign("abbreviation_maxlines", 10, envir = .nif_env)
assign("version", packageVersion("nif"), envir = .nif_env)
assign("debug", TRUE, envir = .nif_env)


#' Set or get global options
#'
#' @description
#' Set global behavior options for all `nif` package functions. Currently
#' supported:
#'
#' * `silent` as logical: Suppress messages.
#' * `watermark` as character: Watermark text on all figures.
#' * `pinboard` as character: Pinboard path for sharing of nif/sdtm objects.
#' * `debug` as logical: Print debug information.
#' * `show_hash` as logical: Include dataset hash in figures.
#' * `abbreviation_threshold` as numeric: Allowed line count in message output
#' without abbreviation. Set to Inf to always show all lines.
#' * `abbrevation_maxlines` as numeric: Line number to that lists in message
#' output will be abbreviated if the line count threshold is exceeded.
#'
#' @param ... Options as named values, or nothing.
#'
#' @return The global options as list, if ... is empty, or nothing.
#' @export
#'
#' @examples
#' nif_option(silent = TRUE)
nif_option <- function(...) {
  args <- list(...)
  allowed_options <- tribble(
    ~name,        ~type_test,
    "pinboard",   is.character,
    "silent",     is.logical,
    "watermark",  is.character,
    "debug",      is.logical,
    "show_hash",  is.logical,
    "test",       is.character,
    # "abbreviate", is.logical,
    "abbreviation_threshold", is.numeric,
    "abbreviation_maxlines", is.numeric
    # "disclaimer", is.character
  )

  if (length(args) == 0) {
    as.list(.nif_env)
  } else {
    # for (i in 1:length(args)) {
    for (i in seq_along(args)) {
      option_name <- names(args)[[i]]
      option_value <- args[[i]]
      temp <- filter(allowed_options, .data$name == option_name)
      if (nrow(temp) == 0) {
        message(paste0("Unknown option '", option_name, "'!"))
      } else if (nrow(temp) == 1) {
        if (!temp$type_test[[1]](option_value)) {
          message(paste0("option '", option_name, "' has the wrong type!"))
        } else {
          assign(option_name, option_value, envir = .nif_env)
        }
      }
    }
  }
}


#' Get the value of global options
#'
#' @param option The option as character.
#'
#' @return The option value, if existent, or NA.
#' @keywords internal
#' @noRd
nif_option_value <- function(option) {
  if (!exists(".nif_env")) {
    return(NA)
  }
  if (!exists(option, envir = .nif_env)) {
    NA
  } else {
    get(option, .nif_env)
  }
}


#' Disclaimer statement
#'
#' @param disclaimer_text Additional disclaimer text.
#'
#' @return Character.
#' @export
#'
#' @examples
#' nif_disclaimer()
nif_disclaimer <- function(disclaimer_text = NA) {
  temp <- paste0("Data set created with `nif`, version ", packageVersion("nif"))
  disc <- disclaimer_text
  if (!is.na(disc)) {
    temp <- paste0(temp, "\n", disc)
  }
  temp
}
