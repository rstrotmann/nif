#' Get the name of a function
#'
#' Returns the name of a function that is supplied as an argument.
#'
#' @param fun A function.
#' @return The name of the function as a character string.
#' @noRd
#' @examples
#'
#' function_name(mean)
#' function_name(sum)
function_name <- function(fun) {
  deparse(substitute(fun))
}


#' Format a file size in bytes for display
#'
#' @param n Size in bytes as a single numeric value.
#'
#' @return A character string such as `"512 B"`, `"1.5 KB"`, `"2.0 MB"`.
#' @noRd
format_bytes <- function(n) {
  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 0) {
    return("0 B")
  }

  if (n < 1024) {
    return(paste(round(n), "B"))
  }
  if (n < 1024^2) {
    return(sprintf("%.1f KB", n / 1024))
  }
  if (n < 1024^3) {
    return(sprintf("%.1f MB", n / 1024^2))
  }
  sprintf("%.1f GB", n / 1024^3)
}


#' Coalescing join
#'
#' Source:
#' https://www.r-bloggers.com/2023/05/
#' replace-missing-value-from-other-columns-using-coalesce-join-in-dplyr/
#'
#' @param x Left, as data frame.
#' @param y Right, as data frame.
#' @param by The 'by' argument of the join functions.
#' @param keep 'left' means keep value from left table if values exist in both
#'  tables.
#' @param suffix Same as the suffix argument in dplyr joins.
#' @param join Choose a join type from the list. The default is full_join.
#'
#' @return A data frame.
#' @noRd
coalesce_join <- function(
    x, y, by = NULL,
    keep = c("left", "right"),
    suffix = c(".x", ".y"),
    join = c("full_join", "left_join", "right_join", "inner_join")
) {
  keep <- match.arg(keep)

  # Confirm the join argument is in the list and matches the string to the
  # function
  join <- match.arg(join)
  join <- match.fun(join)

  # Depends on the keep argument, overwrite the duplicate value
  # If keep = "left", the value from the left table will be kept, vice versa.
  if (keep == "left") suffix_ <- suffix else suffix_ <- rev(suffix)

  join(x, y, by = by, suffix = suffix) |>
    mutate(
      across( # Apply the coalesce function to all overlapped columns
        # Select columns ended with .x if keep = "left"; or .y if keep = "right"
        ends_with(suffix_[1]),
        # Replace .x in var.x with .y to generate var.y, if keep = "left"; or
        # vice versa
        ~ coalesce(., get(str_replace(cur_column(), suffix_[1], suffix_[2]))),
        # Remove the suffix from the combined columns
        .names = "{str_remove(.col, suffix_[1])}"
      ),
      # Remove the temporary columns ended with suffix
      .keep = "unused"
    )
}



