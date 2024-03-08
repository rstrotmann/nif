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

###########################

# nif.001 %>%
#   as.data.frame() %>%
#   filter(ANALYTE == "M1774") %>%
#   filter(ID == 10) %>%
#   ggplot(aes(x = TIME, y = DV, group = ID, color = ANALYTE, admin = EVID)) +
#   geom_admin() +
#   geom_point() +
#   theme_bw()








