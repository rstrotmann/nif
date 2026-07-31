# source: https://ggplot2-book.org/extensions#sec-modifying-geom-defaults


#' @title GeomAdmin ggproto object
#' @description A ggplot2 geom for treatment administration lines that extends
#'   GeomVline.
#' @details This geom creates vertical lines at administration times with
#'   customizable appearance. It inherits from GeomVline but provides specific
#'   defaults for administration lines.
#' @format NULL
#' @usage NULL
GeomAdmin <- ggplot2::ggproto(
  "GeomAdmin", ggplot2::GeomVline,
  default_aes = ggplot2::aes(
    colour = "grey",
    fill = NA,
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  )
)


#' @title stat_admin_proto ggproto object
#' @description ggplot2 stat that draws administration times for a selected
#'   analyte from a nif data set.
#' @details Requires aesthetics `x` and `admin`, where `admin` is a character
#'   analyte name. Uses columns `EVID` and `ANALYTE` from the nif data.
#' @format NULL
#' @usage NULL
#' @noRd
stat_admin_proto <- ggplot2::ggproto(
  "stat_admin_proto",
  ggplot2::Stat,
  required_aes = c("x", "admin"),
  optional_aes = c("EVID", "ANALYTE"),
  dropped_aes = c("x", "y", "admin", "EVID", "ANALYTE"),
  default_aes = ggplot2::aes(
    xintercept = after_stat(xintercept)
  ),
  compute_group = function(data, scales) {
    if (!is.data.frame(data)) {
      stop("data must be a data frame")
    }

    required_cols <- c("x", "admin", "EVID", "ANALYTE")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(
        "Missing required columns: ", paste(missing_cols, collapse = ", "),
        ". geom_admin() requires nif data with EVID and ANALYTE."
      )
    }

    if (!is.character(data$admin) && !is.factor(data$admin)) {
      stop("'admin' must be a character analyte name")
    }

    analyte <- as.character(unique(data$admin))
    if (length(analyte) != 1 || is.na(analyte) || analyte == "") {
      stop("'admin' must specify a single non-missing analyte name")
    }

    admin_rows <- data$EVID == 1 & data$ANALYTE == analyte
    admin_rows[is.na(admin_rows)] <- FALSE
    xintercept <- unique(data$x[admin_rows])
    xintercept <- xintercept[!is.na(xintercept)]

    data.frame(xintercept = xintercept)
  }
)


#' Administration lines for a selected analyte
#'
#' Draws vertical lines at administration times (`EVID == 1`) for the analyte
#' named in the `admin` aesthetic. The plot or layer data must be a nif object
#' (with `EVID` and `ANALYTE` columns).
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. Must
#'   include `admin`, a character string naming the analyte whose administrations
#'   should be plotted (e.g. `aes(admin = "RS2023")`). `x` is typically inherited
#'   from the plot (e.g. `TIME`).
#' @param data A nif object. If `NULL`, the default, the plot data is used and
#'   must be a nif object.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#' @param color The color of the vertical lines. Defaults to `"grey"`.
#' @param linewidth The width of the lines. Defaults to `0.5`.
#' @param linetype The type of the lines. Defaults to `1` (solid).
#' @param alpha The transparency of the lines. Defaults to `NA` (opaque).
#' @param ... Additional parameters passed to the layer.
#'
#' @return A ggplot layer object.
#'
#' @import ggplot2
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' examplinib_sad_nif |>
#'   filter(ID == 1) |>
#'   ggplot(aes(x = TIME, y = DV)) +
#'   geom_admin(aes(admin = "RS2023")) +
#'   geom_point(na.rm = TRUE)
geom_admin <- function(
  mapping = NULL,
  data = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  color = "grey",
  linewidth = 0.5,
  linetype = 1,
  alpha = NA,
  ...
) {
  if (!is.null(mapping) && !inherits(mapping, "uneval")) {
    stop("mapping must be created using aes()")
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("na.rm must be a single logical value")
  }

  if (!is.logical(show.legend) || length(show.legend) != 1) {
    stop("show.legend must be a single logical value")
  }

  if (!is.logical(inherit.aes) || length(inherit.aes) != 1) {
    stop("inherit.aes must be a single logical value")
  }

  if (!is.character(color) || length(color) != 1) {
    stop("color must be a single character value")
  }

  if (!is.numeric(linewidth) || length(linewidth) != 1 || linewidth < 0) {
    stop("linewidth must be a non-negative numeric value")
  }

  if ((!is.numeric(linetype) && !is.character(linetype)) ||
        length(linetype) != 1) {
    stop("linetype must be a single numeric or character value")
  }

  if (length(alpha) != 1 ||
        (!is.na(alpha) &&
           (!is.numeric(alpha) || alpha < 0 || alpha > 1))) {
    stop("alpha must be NA or a numeric value between 0 and 1")
  }

  # Validate nif now, or when plot data is inherited
  if (is.null(data)) {
    data <- function(x) {
      validate_nif(x)
      x
    }
  } else {
    validate_nif(data)
  }

  # Ensure EVID and ANALYTE reach the stat (nif columns, not plot aesthetics)
  nif_mapping <- ggplot2::aes(EVID = .data$EVID, ANALYTE = .data$ANALYTE)
  mapping <- if (is.null(mapping)) {
    nif_mapping
  } else {
    utils::modifyList(nif_mapping, mapping)
  }

  if (is.null(mapping$admin)) {
    stop("geom_admin() requires an 'admin' aesthetic with an analyte name")
  }

  ggplot2::layer(
    geom = GeomAdmin,
    data = data,
    mapping = mapping,
    stat = stat_admin_proto,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      color = color,
      linewidth = linewidth,
      linetype = linetype,
      alpha = alpha,
      ...
    )
  )
}


#' Add a watermark annotation layer for a ggplot2 object
#' @name watermark
#'
#' @param watermark_text The watermark as character. If NULL, uses the value
#'   from nif_option("watermark").
#' @param cex The text size multiplier as numeric. Must be positive.
#' @param fontface Font face ("bold" by default). Must be one of "plain",
#'   "bold", "italic", "bold.italic".
#' @param color The color of the watermark text (default: "lightgrey").
#' @param alpha The transparency of the watermark (default: 0.1).
#' @param x The x position of the watermark (default: 0.5, centered).
#' @param y The y position of the watermark (default: 1, top).
#' @param rotation The rotation angle in degrees (default: 0).
#'
#' @return A ggplot2 annotation layer with the watermark, or NULL if
#'   watermark_text is empty.
#' @importFrom grid textGrob gpar
#' @importFrom stringr str_length
#' @importFrom ggplot2 annotation_custom
#' @export
watermark <- function(
  watermark_text = NULL,
  cex = 1.5,
  fontface = "bold",
  color = "lightgrey",
  alpha = 0.1,
  x = 0.5,
  y = 1,
  rotation = 0
) {
  # Input validation
  if (!is.null(cex) && (!is.numeric(cex) || length(cex) != 1 || cex <= 0)) {
    stop("cex must be a positive numeric value")
  }

  if (!is.character(fontface) || length(fontface) != 1 ||
        !fontface %in% c("plain", "bold", "italic", "bold.italic")) {
    stop("fontface must be one of: 'plain', 'bold', 'italic', 'bold.italic'")
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric value between 0 and 1")
  }

  if (!is.numeric(x) || length(x) != 1 || x < 0 || x > 1) {
    stop("x must be a numeric value between 0 and 1")
  }

  if (!is.numeric(y) || length(y) != 1 || y < 0 || y > 1) {
    stop("y must be a numeric value between 0 and 1")
  }

  if (!is.numeric(rotation) || length(rotation) != 1) {
    stop("rotation must be a numeric value")
  }

  # Get watermark text from options if not provided
  if (is.null(watermark_text)) {
    watermark_text <- tryCatch(
      nif_option_value("watermark"),
      error = function(e) NA_character_
    )
  }

  # Handle empty or NA watermark text
  if (is.na(watermark_text) || watermark_text == "") {
    return(NULL)
  }

  # Ensure watermark_text is character
  watermark_text <- as.character(watermark_text)

  # Adjust text size based on length
  l <- str_length(watermark_text)
  if (l > 20) {
    cex <- cex * 20 / l
  }

  # Create watermark grob
  watermark_grob <- tryCatch(
    grid::textGrob(
      watermark_text,
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      vjust = 1.5,
      gp = grid::gpar(
        color = color,
        alpha = alpha,
        fontface = fontface,
        cex = cex
      ),
      rot = rotation
    ),
    error = function(e) {
      warning("Failed to create watermark: ", e$message)
      NULL
    }
  )

  if (is.null(watermark_grob)) {
    return(NULL)
  }

  # Return annotation layer
  ggplot2::annotation_custom(grob = watermark_grob)
}
