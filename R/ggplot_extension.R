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
#' @param watermark_text The watermark as character.
#' @param cex The cex as numeric.
#' @param fontface Font face ("bold" by default)
#'
#' @return A grob.
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @export
watermark <- function(watermark_text = NULL,
                      cex = 1.5,
                      fontface = "bold"
                      ) {

  if(is_null(watermark_text)) watermark_text = nif_option_value("watermark")
  if(is.na(watermark_text)) watermark_text <- ""

  l <- str_length(watermark_text)
  if(l > 20) cex <- cex * 20/l

  watermark_grob <- grid::textGrob(
    watermark_text,
    x = grid::unit(0.5, "npc"),
    y = grid::unit(1, "npc"),
    vjust = 1.5,
    gp = grid::gpar(
      color = "lightgrey",
      alpha = .1,
      fontface = fontface,
      cex = cex
      ),
    rot = 0
  )

  if(watermark_text != "") ggplot2::annotation_custom(grob = watermark_grob)
}

