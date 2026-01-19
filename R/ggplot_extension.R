# source: https://ggplot2-book.org/extensions#sec-modifying-geom-defaults


#' @title stat_admin_proto ggproto object
#' @description A ggplot2 stat for treatment administrations that creates
#'   vertical lines at administration times.
#' @details This stat requires a data frame with columns 'x' and 'admin', where
#'   'admin' should be a logical or numeric column indicating administration
#'   events (1 or TRUE for administrations, 0 or FALSE otherwise).
#' @import ggplot2
#' @format NULL
#' @usage NULL
#' @noRd
stat_admin_proto <- ggplot2::ggproto(
  "stat_admin_proto",
  ggplot2::Stat,
  required_aes = c("x", "admin"),
  default_aes = ggplot2::aes(
    xintercept = after_stat(xintercept)
  ),
  compute_group = function(data, scales) {
    # Input validation
    if (!is.data.frame(data)) {
      stop("data must be a data frame")
    }

    required_cols <- c("x", "admin")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Convert admin to logical if numeric
    if (is.numeric(data$admin)) {
      data$admin <- as.logical(data$admin)
    }

    # Validate admin values
    if (!is.logical(data$admin)) {
      stop("admin column must be logical or numeric (0/1)")
    }

    # Handle NA values
    if (any(is.na(data$admin))) {
      warning("NA values found in admin column, these will be treated as FALSE")
      data$admin[is.na(data$admin)] <- FALSE
    }

    # Get administration points
    tryCatch(
      {
        admin_data <- data[data$admin, ]

        # Return empty data frame if no administrations
        if (nrow(admin_data) == 0) {
          return(data.frame(xintercept = numeric(0)))
        }

        # Create result
        result <- data.frame(
          xintercept = admin_data$x
        )
        return(result)
      },
      error = function(e) {
        stop("Error in compute_group: ", e$message)
      }
    )
  }
)


#' ggplot stat for treatment administrations
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. If
#'   specified and `inherit.aes = TRUE` (the default), it is combined with the
#'   default mapping at the top level of the plot. You must supply `mapping` if
#'   there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'   If `NULL`, the default, the data is inherited from the plot data as
#'   specified in the call to [ggplot2::ggplot()]. A `data.frame`, or other
#'   object, will override the plot data. A `function` will be called with a
#'   single argument, the plot data. The return value must be a `data.frame`,
#'   and will be used as the layer data.
#' @param geom The geometric object to use to display the data. Defaults to
#'   "vline".
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function. Defaults to "identity".
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped. `FALSE` never
#'   includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than
#'   combining with them. This is most useful for helper functions that define
#'   both data and aesthetics and shouldn't inherit behavior from the default
#'   plot specification.
#' @param color The color of the vertical lines. Defaults to "grey".
#' @param ... Additional parameters passed to the layer.
#'
#' @return A ggplot layer object.
#'
#' @import ggplot2
#' @export
stat_admin <- function(
  mapping = NULL,
  data = NULL,
  geom = "vline",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  color = "grey",
  ...
) {
  # Validate parameters
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

  # Create layer
  layer(
    stat = stat_admin_proto,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color = color, ...)
  )
}


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


#' Administration geom layer for ggplot
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. If
#'   specified and `inherit.aes = TRUE` (the default), it is combined with the
#'   default mapping at the top level of the plot. You must supply `mapping` if
#'   there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'   If `NULL`, the default, the data is inherited from the plot data as
#'   specified in the call to [ggplot2::ggplot()]. A `data.frame`, or other
#'   object, will override the plot data. A `function` will be called with a
#'   single argument, the plot data. The return value must be a `data.frame`,
#'   and will be used as the layer data.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped. `FALSE` never
#'   includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than
#'   combining with them. This is most useful for helper functions that define
#'   both data and aesthetics and shouldn't inherit behavior from the default
#'   plot specification.
#' @param color The color of the vertical lines. Defaults to "grey".
#' @param linewidth The width of the lines. Defaults to 0.5.
#' @param linetype The type of the lines. Defaults to 1 (solid).
#' @param alpha The transparency of the lines. Defaults to NA (fully opaque).
#' @param ... Additional parameters passed to the layer.
#'
#' @return A ggplot layer object.
#'
#' @import ggplot2
#' @export
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
  # Validate parameters
  if (!is.null(mapping) && !inherits(mapping, "uneval"))
    stop("mapping must be created using aes()")

  if (!is.logical(na.rm) || length(na.rm) != 1)
    stop("na.rm must be a single logical value")

  if (!is.logical(show.legend) || length(show.legend) != 1)
    stop("show.legend must be a single logical value")

  if (!is.logical(inherit.aes) || length(inherit.aes) != 1)
    stop("inherit.aes must be a single logical value")

  if (!is.character(color) || length(color) != 1)
    stop("color must be a single character value")

  if (!is.numeric(linewidth) || length(linewidth) != 1 || linewidth < 0)
    stop("linewidth must be a non-negative numeric value")

  if (!is.numeric(linetype) && !is.character(linetype) ||
        length(linetype) != 1)
    stop("linetype must be a single numeric or character value")

  if (!is.na(alpha) &&
        (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1))
    stop("alpha must be NA or a numeric value between 0 and 1")

  # Create layer
  layer(
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
