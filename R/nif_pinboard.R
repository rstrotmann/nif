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
    if(is.na(board)) {
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

#' Get currently active pinboard path
#'
#' @returns A character string.
#' @export
nif_pinboard <- function() {
  temp <- get_pinboard()
  temp$path
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
#'
#' @returns Nothing.
#' @export
pin_write <- function(
    obj, name = NULL, board = NULL, title = NULL, dco = NULL, silent = NULL) {
  UseMethod("pin_write")
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
#'
#' @returns Nothing.
#' @importFrom pins pin_write board_folder
#' @importFrom utils capture.output
#' @export
pin_write.sdtm <- function(
    obj, name = NULL, board = NULL, title = NULL, dco = NULL, silent = NULL) {
  # input validation
  validate_sdtm(obj)
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(title, "title", allow_null = TRUE)
  validate_char_param(dco, "dco", allow_null = TRUE)

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
      description = description,
      metadata = list(type = "sdtm", dco = dco)),
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
#' @inheritParams get_pinboard
#'
#' @returns A sdtm object.
#' @importFrom pins board_folder pin_read
#' @export
pin_read_sdtm <- function(name, board = NULL) {
  # input validation
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name")

  board_obj <- get_pinboard(board)

  # read pin
  out <- pin_read(board_obj, name)

  validate_sdtm(out)
  return(out)
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
#'
#' @returns Nothing.
#' @importFrom pins pin_write board_folder
#' @importFrom utils capture.output
#' @export
pin_write.nif <- function(
    obj, name = NULL, board = NULL, title = NULL, dco = NULL, silent = NULL) {
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
      metadata = list(type = "nif", dco = dco)),
    type = "message")
  conditional_message(
    paste(msg, collapse = "\n"), silent = silent)
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
pin_read_nif <- function(name, board = NULL) {
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
pin_list_object <- function(board = NULL, object_type) {
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
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams get_pinboard
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
#' @inheritParams get_pinboard
#'
#' @returns A data frame.
#' @export
pin_list_nif <- function(board = NULL) {
  pin_list_object(board, "nif")
}
