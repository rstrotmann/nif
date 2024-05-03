# Create package environment for global variables

.nif_env <- new.env(parent = emptyenv())
assign("silent", FALSE, envir = .nif_env)


#' Set global options
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
  temp <- args[names(args) %in% c("silent", "test")]
  for(i in names(args)) {
    assign(i, args[[i]], envir = .nif_env)
  }
}
