# source: https://ggplot2-book.org/extensions#sec-modifying-geom-defaults


StatAdmin <- ggplot2::ggproto("StatAdmin", ggplot2::Stat,
  compute_group = function(data, scales) {
    temp <- data[which(data$admin == 1), ]
    temp$xintercept <- temp$x
    return(temp)
  },
  required_aes = c("x", "admin")
)


#' ggplot stat for treatment administrations
#'
#' @param mapping The mapping.
#' @param data The data as data frame.
#' @param geom The geom.
#' @param position The position.
#' @param na.rm Remove NA, as logical.
#' @param show.legend Show legend as logical.
#' @param inherit.aes Inherit aesthetics as logical.
#' @param xintercept The x intercept.
#' @param ... Further parameters.
#'
#' @return A ggplot layer object.
#' @importFrom ggplot2 layer
#' @export
stat_admin <- function(mapping = NULL, data = NULL,
                       geom = "vline", position = "identity",
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, xintercept = NULL, ...) {
  layer(
    stat = StatAdmin,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color = "gray",...)
  )
}


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
#' @inheritParams ggplot2::geom_vline
#' @return a ggplot2 layer.
#' @importFrom ggplot2 layer
#' @export
geom_admin <- function(mapping = NULL, data = NULL,
                       # stat = StatAdmin,
                       # position = "identity",
                       na.rm = FALSE,
                       show.legend = NA,
                       # inherit.aes = TRUE,
                       ...) {
  ggplot2::layer(
    geom = GeomAdmin,
    data = data,
    mapping = mapping,
    stat = StatAdmin,
    position = "identity",
    show.legend = show.legend,
    # inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color = "gray", ...)
  )
}


#' Add a watermark annotation layer for a ggplot2 object
#' @name watermark
#'
#' @param watermark_text The watermark as character. If NULL, uses the value from nif_option("watermark").
#' @param cex The text size multiplier as numeric. Must be positive.
#' @param fontface Font face ("bold" by default). Must be one of "plain", "bold", "italic", "bold.italic".
#' @param color The color of the watermark text (default: "lightgrey").
#' @param alpha The transparency of the watermark (default: 0.1).
#' @param x The x position of the watermark (default: 0.5, centered).
#' @param y The y position of the watermark (default: 1, top).
#' @param rotation The rotation angle in degrees (default: 0).
#'
#' @return A ggplot2 annotation layer with the watermark, or NULL if watermark_text is empty.
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
    cex <- cex * 20/l
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
      return(NULL)
    }
  )
  
  if (is.null(watermark_grob)) {
    return(NULL)
  }
  
  # Return annotation layer
  ggplot2::annotation_custom(grob = watermark_grob)
}

