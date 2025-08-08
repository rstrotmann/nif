#' Read SDTM data
#'
#' This function reads SDTM-formatted data as SAS or XPT files from a folder
#' location.
#'
#' @param data_path The file system path to the source folder as character.
#' @param domain The domain name(s) as character, defaults to
#'   `c("dm", "vs", "ex", "pc")`.
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
                      domain = c("dm", "vs", "ex", "pc"),
                      format = "sas", delim = ",", ...) {
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
  if (!is.character(domain) || length(domain) == 0) {
    stop("domain must be a non-empty character vector", call. = FALSE)
  }

  # Get file extension based on format
  file_ext <- switch(format,
    "sas" = ".sas7bdat",
    "xpt" = ".xpt",
    "csv" = ".csv",
    "xlsx" = ".xlsx"
  )

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
         paste(missing_files, collapse = "\n"), call. = FALSE)
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
  new_sdtm(out)
}


#' Generic function
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj The object to pin.
#' @param board Path to pin board, as character, defaults to respective
#'   nif_option setting if NULL.
#' @param name Name for the pin object, as character. Defaults to
#'   'xx_sdtm' or 'xx_nif' with xx the study name.
#' @param title Title for the pin object, as character. Defaults to
#'   the study name(s).
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#'
#' @returns Nothing.
#' @export
pin_write <- function(
    obj, name = NULL, board = NULL, title = NULL, silent = NULL) {
  UseMethod("pin_write")
}


#' Write to pin board
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj sdtm object.
#' @param board Path to pin board, as character, defaults to respective
#'   nif_option setting if NULL.
#' @param name Name for the sdtm pin object, as character. Defaults to
#'   'xx_sdtm' with xx the study name.
#' @param title Title for the sdtm pin object, as character. Defaults to
#'   the study name.
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#'
#' @returns Nothing.
#' @importFrom pins pin_write board_folder
#' @importFrom utils capture.output
#' @export
pin_write.sdtm <- function(
    obj, name = NULL, board = NULL, title = NULL, silent = NULL) {
  # input validation
  validate_sdtm(obj)
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(title, "title", allow_null = TRUE)

  if(is.null(board))
    board <- nif_option_value("board")

  if(is.null(board) || is.na(board))
    stop("No pin board provided")

  board_obj <- pins::board_folder(board)

  if(!dir.exists(board))
    stop(paste0(
      "Board ", board, " does not exist"))

  if(is.null(name))
    name <- paste0(summary(obj)$study, "_sdtm")

  if(is.null(title))
    title <- paste0("SDTM data from study ", summary(obj)$study)

  description <- utils::capture.output(
    print(summary(obj))
  )

  msg <- utils::capture.output(
    pins::pin_write(
      board_obj, obj, name = name, title = title, type = "rds",
      description = description,
      metadata = list(type = "sdtm")),
    type = "message")
  conditional_message(
    paste(msg, collapse = "\n"), silent = silent)
}


#' Read sdtm object from pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param name The pin name, as character.
#' @param board The board folder, defaults to the respective nif_option setting.
#'
#' @returns A sdtm object.
#' @importFrom pins board_folder pin_read
#' @export
pin_read_sdtm <- function(name, board = NULL) {
  # input validation
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name")

  # get board
  if(is.null(board))
    board <- nif_option_value("board")

  if(is.null(board) || is.na(board))
    stop("No pin board provided")

  board_obj <- pins::board_folder(board)

  if(!dir.exists(board))
    stop(paste0(
      "Board ", board, " does not exist"))

  # read pin
  out <- pin_read(board_obj, name)

  validate_sdtm(out)
  return(out)
}



#' List contents of pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param board The board path as character.
#' @param object_type The object to filter for (sdtm or nif)
#'
#' @returns The board folder, defaults to the respective nif_option setting.
#' @importFrom pins board_folder pin_search
pin_list_object <- function(board = NULL, object_type) {
  # input validation
  validate_char_param(board, "board", allow_null = TRUE)

  # get board
  if(is.null(board))
    board <- nif_option_value("board")

  if(is.null(board) || is.na(board))
    stop("No pin board provided")

  board_obj <- pins::board_folder(board)

  if(!dir.exists(board))
    stop(paste0(
      "Board ", board, " does not exist"))

  temp <- pin_search(board_obj)

  sel = as.logical(lapply(
    temp$meta,
    function(x) {x$user[["type"]] == object_type}
  ))
  sel[is.na(sel)] <- FALSE

  temp[sel, ] %>%
    select("name", "title", "created") %>%
    as.data.frame()
}


#' List sdtm objects in pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param board A pinboard path, as character.
#'
#' @returns A data frame.
#' @export
pin_list_sdtm <- function(board = NULL) {
  pin_list_object(board, "sdtm")
}


#' List nif objects in pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param board A pinboard path, as character.
#'
#' @returns A data frame.
#' @export
pin_list_nif <- function(board = NULL) {
  pin_list_object(board, "nif")
}
