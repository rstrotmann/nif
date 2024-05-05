# source: https://ggplot2-book.org/extensions#sec-modifying-geom-defaults

StatAdmin <- ggproto("StatAdmin", Stat,
  compute_group = function(data, scales) {
    temp <- data[which(data$admin == 1), ]
    temp$xintercept <- temp$x
    return(temp)
  },
  required_aes = c("x", "admin")
)

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

GeomAdmin <- ggproto("GeomAdmin", GeomVline,
                             default_aes = aes(
                               colour = "grey",
                               fill = NA,
                               linewidth = 0.5,
                               linetype = 1,
                               alpha = NA
                             )
)

#' administration geom layer for ggplot
#'
#' @inheritParams ggplot2::geom_vline
#' @return a ggplot2 layer.
#' @import ggplot2
#' @export
geom_admin <- function(mapping = NULL, data = NULL,
                       # stat = StatAdmin,
                       # position = "identity",
                       na.rm = FALSE,
                       show.legend = NA,
                       # inherit.aes = TRUE,
                       ...) {
  layer(
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


# geom_watermark <- function(watermark = NULL, ...) {
#   # if(is.null(watermark)) watermark = nif_option_value("watermark")
#
#   layer(
#     geom = GeomText,
#     data = NULL,
#     mapping = NULL,
#     stat = "identity",
#     position = "identity",
#     params = list(color = "gray",
#                   label = watermark,
#                   x = 0, y = 0,
#                   vjust = 0.5,
#                   hjust = 0.5, ...)
#   )
# }


#' Add a watermark annotation layer for a ggplot2 object
#' @name watermark
#' @param watermark String to be added as watermark
#' @param fontsize Font size
#' @param colour Font colour
#' @param alpha Alpha (transparency; lower number = more transparent)
#' @param fontface Font face ("bold" by default)
#' @param angle Angle of the watermark
#' @import grid
#' @export
watermark <- function(watermark_text = NULL,
                      cex = 2,
                      fontface = "bold"
                      ) {

  if(is_null(watermark_text)) watermark_text = nif_option_value("watermark")
  if(is.na(watermark_text)) watermark_text <- ""

  l <- str_length(watermark_text)
  if(l > 20) cex <- cex * 20/l

  watermark_grob <- grid::textGrob(
    watermark_text,
    x = unit(0.5, "npc"), y = unit(1, "npc"),
    vjust = 1.5,
    gp = gpar(
      # hadjust = 1,
      # vjust = 1,
      # hjust = 0,
      color = "lightgrey",
      alpha = .1,
      fontface = fontface,
      cex = cex
      ),
    rot = 0
  )

  if(watermark_text != "") annotation_custom(grob = watermark_grob)
}

