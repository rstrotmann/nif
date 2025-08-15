#' Get pinboard object
#'
#' @param board The path to the board object. Defaults to the 'pinboard' value
#' of nif_option, or the value of the 'NIF_PINBOARD' key in the .Renviron
#' setting if the 'nif_option' is not set.
#'
#' @returns A pinboard object.
#' @importFrom pins board_folder
get_pinboard <- function(board = NULL) {
  if(is.null(board)) {
    board <- nif_option_value("pinboard")
    if(is.na(board) | is.null(board) | board == "") {
      board = Sys.getenv("NIF_PINBOARD")
      if(board == "")
        stop("No pinboard found")
    }
  }

  if(!dir.exists(board))
    stop(paste0(
      "Pinboard ", board, " does not exist"))

  board_obj <- pins::board_folder(board)
  return(board_obj)
}


#' Get or set pinboard path
#'
#' This function returns the currently active pinboard path, i.e.,
#' either the respective nif_option setting or the folder set in the .Renviron
#' variable NIF_PINBOARD. If the 'path' argument is provided, the nif_option
#' 'pinboard' will be set to that folder. Use an empty string ("") to reset the
#' pinboard path.
#'
#' Overall, the following pinboard paths are used for all pinboard functions (in
#' the order of their priority):
#'
#' * The 'board' parameter in the individual function call (highest)
#' * The 'pinboard' setting in nif_options
#' * The .Renviron variable NIF_PINBOARD (lowest)
#'
#' You may use the convenience function [usethis::edit_r_environ()] to edit the
#' .Renviron file and include a user-level setting for the default pinboard path
#' (e.g., `NIF_PINBOARD=\path\to\pinboard\folder`).
#'
#' @param path Pinboard path as character.
#'
#' @returns Currently active pinboard path as character.
#' @seealso [usethis::edit_r_environ()]
#' @seealso [nif_option()]
#' @export
nif_pinboard <- function(path = NULL) {
  # validate_char_param(folder, "folder", allow_null = TRUE)

  if(!is.null(path)) {
    if(!path == "" & !dir.exists(path))
      stop(paste0(
        "Pinboard ", path, " does not exist"))
    nif_option(pinboard = path)
  }

  temp <- get_pinboard()
  temp$path
}


#' Generic pin_write function
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated, please use pb_write() instead!
#'
#' @param obj The object to pin.
#' @inheritParams get_pinboard
#' @param name Name for the pin object, as character. Defaults to
#'   'xx_sdtm' or 'xx_nif' with xx the study name.
#' @param title Title for the pin object, as character. Defaults to
#'   the study name(s).
#' @param dco Data cut off as character.
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#' @param force Force-write, as logical.
#'
#' @returns Nothing.
#' @export
pin_write <- function(
    obj, name = NULL, board = NULL, title = NULL, dco = NULL, force = FALSE,
    silent = NULL) {
  lifecycle::deprecate_warn("0.56.3", "pin_write()", "pb_write()")
  UseMethod("pb_write")
}


#' Generic pin_write function
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj The object to pin.
#' @inheritParams get_pinboard
#' @param name Name for the pin object, as character. Defaults to
#'   'xx_sdtm' or 'xx_nif' with xx the study name.
#' @param title Title for the pin object, as character. Defaults to
#'   the study name(s).
#' @param dco Data cut off as character.
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#' @param force Force-write, as logical.
#'
#' @returns Nothing.
#' @export
pb_write <- function(
    obj, name = NULL, board = NULL, title = NULL, dco = NULL, force = FALSE,
    silent = NULL) {
  UseMethod("pb_write")
}


#' Write to pin board
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj sdtm object.
#' @inheritParams get_pinboard
#' @param name Name for the sdtm pin object, as character. Defaults to
#'   'xx_sdtm' with xx the study name.
#' @param title Title for the sdtm pin object, as character. Defaults to
#'   the study name.
#' @param dco Data cut off as character.
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#' @param force Force-write, as logical.
#'
#' @returns Nothing.
#' @importFrom pins pin_write board_folder
#' @importFrom utils capture.output
#' @export
pb_write.sdtm <- function(
    obj, name = NULL, board = NULL, title = NULL, dco = NULL, force = FALSE,
    silent = NULL) {
  # input validation
  validate_sdtm(obj)
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(title, "title", allow_null = TRUE)
  validate_char_param(dco, "dco", allow_null = TRUE)
  validate_logical_param(force, "force")

  board_obj <- get_pinboard(board)

  if(is.null(name))
    name <- paste0(summary(obj)$study, "_sdtm")

  if(is.null(title))
    title <- paste0("SDTM data from study ", summary(obj)$study)

  if(is.null(dco)) {
    dco <- ""
  }

  description <- utils::capture.output(
    print(summary(obj))
  )

  msg <- utils::capture.output(
    pins::pin_write(
      board_obj, obj, name = name, title = title, type = "rds",
      description = description, force_identical_write = force,
      metadata = list(type = "sdtm", dco = dco)),
    type = "message")
  conditional_message(
    paste(msg, collapse = "\n"), silent = silent)
}


#' Write to pin board
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj nif object.
#' @inheritParams get_pinboard
#' @param name Name for the nif pin object, as character. Defaults to
#'   'xx_nif' with xx the studies included.
#' @param title Title for the nif pin object, as character. Defaults to
#'   the studies included.
#' @param dco Data cut off as character.
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#' @param force Force-write, as logical.
#'
#' @returns Nothing.
#' @importFrom pins pin_write board_folder
#' @importFrom utils capture.output
#' @export
pb_write.nif <- function(
    obj, name = NULL, board = NULL, title = NULL, dco = NULL, force = FALSE,
    silent = NULL) {
  # input validation
  validate_nif(obj)
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(title, "title", allow_null = TRUE)
  validate_char_param(dco, "dco", allow_null = TRUE)

  board_obj <- get_pinboard(board)

  if(is.null(name))
    name <- paste0(paste(studies(obj), collapse = "_"), "_nif")

  if(is.null(title))
    title <- paste(studies(obj), collapse = "_")

  if(is.null(dco))
    dco <- ""

  msg <- utils::capture.output(
    pins::pin_write(
      board_obj, obj, name = name, title = title, type = "rds",
      force_identical_write = force,
      metadata = list(type = "nif", dco = dco)),
    type = "message")
  conditional_message(
    paste(msg, collapse = "\n"), silent = silent)
}


#' Read sdtm object from pinboard
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param name The pin name, as character.
#' @inheritParams get_pinboard
#'
#' @returns A sdtm object.
#' @importFrom pins board_folder pin_read
#' @export
pin_read_sdtm <- function(name, board = NULL) {
  lifecycle::deprecate_warn("0.56.3", "pin_read_sdtm()", "pb_read_sdtm()")
  pb_read_sdtm(name, board = board)
}


#' Read sdtm object from pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param name The pin name, as character.
#' @inheritParams get_pinboard
#'
#' @returns A sdtm object.
#' @importFrom pins board_folder pin_read
#' @export
pb_read_sdtm <- function(name, board = NULL) {
  # input validation
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name")

  board_obj <- get_pinboard(board)

  # read pin
  out <- pin_read(board_obj, name)

  validate_sdtm(out)
  return(out)
}


#' Read nif object from pinboard
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use pb_read_nif() instead!
#'
#' @param name The pin name, as character.
#' @inheritParams get_pinboard
#'
#' @returns A nif object.
#' @export
pin_read_nif <- function(name, board = NULL) {
  lifecycle::deprecate_warn("0.56.3", "pin_read_nif()", "pb_read_nif()")

  pb_read_nif(name, board = board)
}


#' Read nif object from pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param name The pin name, as character.
#' @inheritParams get_pinboard
#'
#' @returns A nif object.
#' @export
pb_read_nif <- function(name, board = NULL) {
  # input validation
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name")

  board_obj <- get_pinboard(board)

  # read pin
  out <- pin_read(board_obj, name)

  validate_nif(out)
  return(out)
}


#' List contents of pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_pinboard
#' @param object_type The object to filter for (sdtm or nif)
#'
#' @returns The board folder, defaults to the respective nif_option setting.
#' @importFrom pins board_folder pin_search
pb_list_object <- function(board = NULL, object_type) {
  # input validation
  validate_char_param(board, "board", allow_null = TRUE)

  board_obj <- get_pinboard(board)
  temp <- pin_search(board_obj)

  temp$dco <- as.character(lapply(temp$meta, function(x) x$user$dco))
  temp$type <- as.character(lapply(temp$meta, function(x) x$user$type))

  temp %>%
    filter(.data$type == object_type) %>%
    select("name", "title", "created", "dco") %>%
    as.data.frame()
}


#' List sdtm objects in pinboard
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams get_pinboard
#'
#' @returns A data frame.
#' @export
pin_list_sdtm <- function(board = NULL) {
  lifecycle::deprecate_warn("0.56.3", "pin_list_sdtm()", "pb_list_sdtm()")
  pb_list_object(board, "sdtm")
}


#' List sdtm objects in pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_pinboard
#'
#' @returns A data frame.
#' @export
pb_list_sdtm <- function(board = NULL) {
  pb_list_object(board, "sdtm")
}



#' List nif objects in pinboard
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams get_pinboard
#'
#' @returns A data frame.
#' @export
pin_list_nif <- function(board = NULL) {
  lifecycle::deprecate_warn("0.56.3", "pin_list_nif()", "pb_list_nif()")
  pb_list_object(board, "nif")
}


#' List nif objects in pinboard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_pinboard
#'
#' @returns A data frame.
#' @export
pb_list_nif <- function(board = NULL) {
  pb_list_object(board, "nif")
}



