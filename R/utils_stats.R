#' Positive value or zero if negative
#'
#' @param x Numeric.
#' @return Numeric.
#' @noRd
#' @examples
#' positive_or_zero(2)
#' positive_or_zero(-2)
#' positive_or_zero(c(2, 1, 0, -1, -2))
positive_or_zero <- function(x) {
  x[which(x < 0 | is.na(x))] <- 0
  x
}


#' Mean derivative that works with vectors containing NaN
#'
#' @param x The input as numeric.
#'
#' @return The mean as numeric.
#' @noRd
safe_mean <- function(x) {
  temp <- x[!is.nan(x) & !is.na(x)]
  if (length(temp) == 0) {
    return(NA)
  }
  out <- mean(temp, na.rm = TRUE)
  attributes(out)$N <- length(temp[!is.na(temp)])
  out
}


#' SD derivative that works with vectors containing NaN
#'
#' @param x The input as numeric.
#'
#' @return The mean as numeric.
#' @noRd
safe_sd <- function(x) {
  temp <- x[!is.nan(x) & !is.na(x)]
  if (length(temp) == 0) {
    return(NA)
  }
  out <- stats::sd(temp, na.rm = TRUE)
  attributes(out)$N <- length(temp[!is.na(temp)])
  out
}


#' Min derivative that works with vectors containing NaN
#'
#' @param x The input as numeric.
#'
#' @return The mean as numeric.
#' @noRd
safe_min <- function(x) {
  temp <- x[!is.nan(x) & !is.na(x)]
  if (length(temp) == 0) {
    return(NA)
  }
  out <- min(temp, na.rm = TRUE)
  out
}


numeric_or_na <- function(x) {
  all(is.na(x) | is.numeric(x))
}

#' Difference, or NA, if difference is negative
#'
#' @param a A as numeric.
#' @param b B as numeric.
#'
#' @return Numeric.
#' @noRd
pos_diff <- function(a, b) {
  data.frame(
    a = a,
    b = b
  ) |>
    mutate(diff = case_when(.data$a - .data$b < 0 ~ NA,
                            .default = .data$a - .data$b)) |>
    pull(.data$diff)
}


lower_ci <- function(mean, sd, n, conf_level = 0.9){
  se <- sd / sqrt(n)
  mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

upper_ci <- function(mean, sd, n, conf_level = 0.9){
  se <- sd / sqrt(n)
  mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}


#' Set all NA values in DV to zero
#'
#' @param obj A nif object.
#'
#' @returns A nif object.
#' @noRd
dv_na_to_zero <- function(obj) {
  validate_nif_argument(obj)

  mutate(obj, DV = case_when(is.na(.data$DV) ~ 0, .default = .data$DV))
}


#' Add percent column to data frame, based on the n column.
#'
#' @param df A data frame.
#'
#' @returns The data frame with the 'percent' field added.
#' @noRd
add_percent <- function(df) {
  mutate(df, percent = round(.data$n/sum(.data$n) * 100, 1))
}


#' Geometric mean
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Drop missing values before calculation.
#'   Defaults to `FALSE`.
#'
#' @return A numeric scalar. Zeros yield `0`. Negatives yield `NaN`.
#'   An empty vector, or all-`NA` input with `na.rm = TRUE`, returns `NA_real_`.
#' @export
geomean <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }
  if (!length(x)) {
    return(NA_real_)
  }
  exp(mean(log(x)))
}


#' Geometric coefficient of variation, in percent
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Drop missing values before calculation.
#'   Defaults to `FALSE`.
#'
#' @return A numeric scalar (percent). Fewer than two non-missing values
#'   returns `NA_real_`. Zeros and negatives yield `NaN`.
#' @export
geocv <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }
  if (length(x) < 2L) {
    return(NA_real_)
  }
  sqrt(exp(stats::sd(log(x))^2) - 1) * 100
}
