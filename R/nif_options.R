# Create package environment for global variables

.nif_env <- new.env(parent = emptyenv())
assign("silent", FALSE, envir = .nif_env)


#' Set or get global options
#'
#' @description
#' Set global behavior options for all `nif` package functions. Currently
#' supported:
#'
#' * `silent` as logical: Suppress messages.
#' * `watermark` as character: Watermark text on all figures.
#' * `debug` as logical: Print debug information.
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
    ~name,       ~type_test,
    "silent",    is.logical,
    "watermark", is.character,
    "debug",     is.logical,
    "test",      is.character,
  )

  if(length(args) == 0) {
    as.list(.nif_env)
  } else {
    for(i in 1:length(args)) {
      option_name <- names(args)[[i]]
      option_value <- args[[i]]
      temp <- filter(allowed_options, .data$name == option_name)
      if(nrow(temp) == 0){
        message(paste0("Unknown option '", option_name, "'!"))
      } else if(nrow(temp) == 1) {
        if(!temp$type_test[[1]](option_value)) {
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
nif_option_value <- function(option) {
  if(!exists(".nif_env")) return(NA)
  if(!exists(option, envir = .nif_env)){
    return(NA)
  } else {
    return(get(option, .nif_env))
  }
}
