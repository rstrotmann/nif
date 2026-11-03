#' Horizontal line using unicode characters
#'
#' @param length Line length.
#'
#' @returns A character string.
#' @noRd
hline <- function(length = 8) {
  paste0(rep("\U2500", length), collapse = "")
}


#' Create command line message from components
#'
#' If any component is NULL, the message will be empty.
#'
#' @param ... Message components as character or data frame arguments.
#' @param condition Output condition as logical.
#' @param indent The indentation level as numeric.
#' @param color Print color as logical.
#'
#' @returns A list of character strings.
#' @noRd
compose_message <- function(
    ...,
    condition = TRUE,
    indent = 2,
    color = FALSE
) {
  args <- list(...)
  if (any(sapply(args, is.null)))
    return(NULL)

  if (length(args) > 0 & isTRUE(condition)) {
    trimws(paste(lapply(
      args,
      function(x) {
        ifelse(
          inherits(x, "data.frame"),
          df_to_string(x, color = color, indent = indent),
          as.character(x)
        )
      }
    ), collapse = "\n"))
  }
}


#' Output message components
#'
#' @param message A list of character strings.
#'
#' @returns Nothing.
#' @noRd
cat_message <- function(message) {
  dummy <- lapply(
    message,
    function(x) {
      cat(ifelse(x == "", "", paste0(x, "\n\n")))
    })

  invisible(message)
}


#' Convert indent level to padding string of spaces
#'
#' @param indent The indent level as numeric.
#'
#' @return A character string
#' @keywords internal
#' @noRd
indent_string <- function(indent = 0) {
  paste(replicate(positive_or_zero(indent), " "), collapse = "")
}


#' Nice enumeration of multiple strings
#'
#' @param items Items to enumerate as character.
#' @param conjunction The conjunction between the last and penultimate items.
#'
#' @return Enumeration as character.
#' @noRd
#'
#' @examples
#' nice_enumeration("A")
#' nice_enumeration(c("A", "B"))
#' nice_enumeration(c("A", "B", "C"))
#' nice_enumeration(c("A", "B", "C"), conjunction = "or")
nice_enumeration <- function(items, conjunction = "and") {
  if (length(items) == 1) {
    return(items[[1]])
  }
  if (length(items) > 1) {
    paste(
      paste(items[seq_along(items[-1])], collapse = ", "),
      conjunction,
      items[length(items)]
    )
  }
}


#' Return singular or plural form of word
#'
#' @param word Source word in singular form, as character.
#' @param plural Return plural form, as character.
#'
#' @return Character.
#' @noRd
#' @examples
#'
#' plural("subject", FALSE)
#' plural("subject", TRUE)
#' plural("study", FALSE)
#' plural("study", TRUE)
plural <- function(word, plural) {
  exceptions <- tribble(
    ~singular, ~plural,
    "study", "studies",
    "Study", "Studies",
    "was", "were",
    "is", "are"
  )
  if (plural) {
    if (word %in% exceptions$singular) {
      as.character(exceptions[exceptions$singular == word, "plural"])
    } else {
      paste0(word, "s")
    }
  } else {
    word
  }
}


#' Render data frame object to string
#'
#' This function renders a data.frame into a character vector, similar to its
#' representation when printed without line numbers
#'
#' @param df The data frame to be rendered.
#' @param indent Indentation level, as numeric.
#' @param header Boolean to indicate whether the header row is to be included.
#' @param color Print headers in grey as logical.
#' @param n The number of lines to be included, or all if NULL.
#' @param show_none Show empty data frame as 'none', as logical.
#' @param header_sep Show separation line after header, as logical.
#' @param na_string String to use for NA values. Defaults to "NA".
#' @param abbr_lines The row number to which long data frames are abbreviated
#' in the ouput if the threshold is exceeded. Defaults to nif_option settings if
#' NULL.
#' @param abbr_threshold The row number threshold beyond which long data frames
#' are abbreviated. Defaults to nif_option settings if NULL.
#'
#' @returns Character vector.
df_preprint <- function(
    df,
    indent = 0,
    n = NULL,
    header = TRUE,
    header_sep = FALSE,
    color = FALSE,
    show_none = FALSE,
    na_string = "NA",
    abbr_lines = NULL,
    abbr_threshold = NULL
) {

  # Input validation
  if (is.null(df)) {
    return("")
  }

  if (!inherits(df, "data.frame")) {
    stop("Input must be a data frame")
  }

  if (!is.numeric(indent) || indent < 0) {
    stop("Indent must be a non-negative number")
  }

  validate_argument(n, "numeric", allow_null = TRUE)
  validate_argument(header, "logical")
  validate_argument(header_sep, "logical")
  validate_argument(color, "logical")
  validate_argument(show_none, "logical")
  validate_argument(na_string, "character")
  validate_argument(abbr_lines, "numeric", allow_null = TRUE)
  validate_argument(abbr_threshold, "numeric", allow_null = TRUE)

  # business logic

  # Handle empty data frame early
  if (nrow(df) == 0) {
    if (show_none) {
      return(paste0(indent_string(indent), "none\n"))
    }
    return("")
  }

  if (!is.null(n)) {
    df <- utils::head(df, n = n)
  }

  # formatted output
  footer <- ""

  max_widths <- as.numeric(lapply(
    rbind(mutate(df, across(everything(), as.character)), names(df)),
    function(x) max(nchar(x), na.rm = TRUE)
  ))

  # Create the padding function
  pad_element <- function(element, width) {
    sprintf(paste0("%-", width, "s  "), element)
  }

  # abbreviation handler
  if (is.null(abbr_lines))
    abbr_lines <- nif_option_value("abbreviation_maxlines")

  if (is.null(abbr_threshold))
    abbr_threshold <- nif_option_value("abbreviation_threshold")

  nr <- nrow(df)
  if (nr > abbr_threshold) {
    if (!is.null(abbr_lines)) {
      df <- utils::head(df, abbr_lines)
      if (nr - abbr_lines > 0) {
        footer <- paste0(
          indent_string(indent), "(", nr - abbr_lines, " more rows)"
        )
      }
    }
  }

  # Convert all columns to character, handling NA values
  df <- as.data.frame(df) |>
    mutate(across(everything(), ~ ifelse(is.na(.), na_string, as.character(.))))

  # Create line renderer
  render_line <- function(line) {
    paste0(
      indent_string(indent),
      paste0(
        mapply(
          pad_element,
          element = as.character(line),
          width = max_widths
        ),
        collapse = ""
      )
    )
  }

  # Build output starting with header if requested
  output_parts <- character(0)

  if (header) {
    header_line <- render_line(names(df))
    if (color) {
      header_line <- paste0("\u001b[38;5;248m", header_line, "\u001b[0m")
    }
    output_parts <- c(output_parts, header_line)

    if (header_sep) {
      separator <- paste0(
        indent_string(indent),
        paste(
          mapply(
            function(w) paste(rep("-", w), collapse = ""),
            max_widths
          ),
          collapse = "   "
        )
      )
      if (color) {
        separator <- paste0("\u001b[38;5;248m", separator, "\u001b[0m")
      }
      output_parts <- c(output_parts, separator)
    }
  }

  # Add data rows
  c(output_parts, apply(df, 1, render_line), footer)
}


#' Render data frame object to string
#'
#' This function renders a data.frame into a string similar to its
#' representation when printed without line numbers
#'
#' @inheritParams df_preprint
#'
#' @returns nothing
#' @noRd
df_to_string <- function(
    df,
    indent = 0,
    n = NULL,
    header = TRUE,
    header_sep = FALSE,
    color = FALSE,
    show_none = FALSE,
    na_string = "NA",
    abbr_lines = NULL,
    abbr_threshold = NULL
) {
  temp <- df_preprint(
    df, indent, n, header, header_sep, color, show_none, na_string,
    abbr_lines, abbr_threshold)
  paste(temp, collapse = "\n")
}


#' Print data frame as cli object
#'
#' @inheritParams df_preprint
#' @return nothing
#' @noRd
df_to_cli <- function(
    df,
    indent = 0,
    n = NULL,
    header = TRUE,
    header_sep = FALSE,
    color = FALSE,
    show_none = FALSE,
    na_string = "NA",
    abbr_lines = NULL,
    abbr_threshold = NULL
) {
  temp <- df_preprint(
    df, indent, n, header, header_sep, color, show_none, na_string, abbr_lines,
    abbr_threshold)
  dummy <- lapply(temp, function(x) cli_verbatim(x))
}


#' Issue cli message based on silent flag
#'
#' @param cli_expression Message components as cli calls.
#' @param silent Logical flag to suppress messages. If NULL, will check .nif_env
#'              environment (defaults to FALSE if not found).
#' @return Nothing.
#' @import cli
#' @keywords internal
#' @noRd
conditional_cli <- function(cli_expression, silent = NULL) {
  # Get silent flag from parameter or environment
  if (is.null(silent)) {
    silent <- tryCatch(
      {
        get("silent", envir = .nif_env)
      },
      error = function(e) {
        FALSE
      }
    )
  }

  # issue cli message if not silent
  if (!isTRUE(silent)) {
    cli::cli(cli_expression)
  }

  invisible(NULL)
}


conditional_message <- function(..., silent = NULL) {
  # Get silent flag from parameter or environment
  if (is.null(silent)) {
    silent <- tryCatch(
      {
        get("silent", envir = .nif_env)
      },
      error = function(e) {
        FALSE
      }
    )
  }

  # Safely convert arguments to character
  args <- tryCatch(
    {
      lapply(list(...), as.character)
    },
    error = function(e) {
      warning("Failed to convert some arguments to character")
      list(...)
    }
  )

  # Print message if not silent
  if (!isTRUE(silent)) {
    message(paste(args, collapse = ""))
  }

  invisible(NULL)
}


#' Debug output
#'
#' Print an object only if the global option 'debug' is `TRUE`.
#'
#' @param obj An object.
#'
#' @return Nothing
#' @noRd
#' @seealso [nif::nif_option()]
print_debug <- function(obj) {
  if (rlang::is_true(nif_option_value("debug"))) print(obj)
}
