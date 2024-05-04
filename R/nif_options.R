# Create package environment for global variables

.nif_env <- new.env(parent = emptyenv())
assign("silent", FALSE, envir = .nif_env)


#' Set global options
#'
#' @description
#' Set global behavior options for all `nif` package functions. Currently
#' supported:
#'
#' * `silent` as logical: Suppress messages.
#' * `watermark` as character: Watermark text on all figures.
#'
#' @param ... Options as named values.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' nif_option(silent = TRUE)
nif_option <- function(...) {
  args <- list(...)
  allowed_options <- tribble(
    ~name,       ~type_test,   ~length,
    "silent",    is.logical,   NA,
    "watermark", is.character, 30,
    "test",      is.character, NA
  )

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
  # for(i in names(temp)) {
  #   assign(i, temp[[i]], envir = .nif_env)
  # }
}
