#' @title Geom to plot a time line with points for each earthquake
#'
#'Geom function geom_timeline()
#'
#'@description a geom for ggplot2 called geom_timeline() for plotting a time line of
#'earthquakes ranging from xmin to xmax dates with a point for each earthquake.
#'Aesthetics include colour, size, and alpha (for transparency).
#'
#'@details dataframe obtained after running functions eq_clean_data()
#'and eq_location_clean(). Initial raw data should be downloaded from NOAA website.
#'The data must be cleaned using the functions eq_clean_data and eq_location_clean,
#'that are included in the package. Aesthetics can be specified in geom_timeline or
#'in ggplot
#'
#'
#'@return function produce a time line plot of selected earthquakes
#'
#' @inheritParams ggplot2::layer
#' @param mapping Set of aesthetic mappings created
#' @param data  The data to be displayed
#' @param stat  The statistical transformation to use on the data for this layer,
#' as a string.
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#' a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them.
#' @param ... Other arguments passed on to the layer. it can be used to set an
#' aesthetic to a fixed value.
#'
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#'earthquake_track_clean %>%
#'dplyr::filter(COUNTRY %in% c('JAPAN', 'IRAN',YEAR > 2010)) %>%
#'
#'    #option to filter date:
#'#dplyr::filter(DATE > '2010-01-01', DATE < '2015-01-01') %>%
#'ggplot() +
#'geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) +
#'scale_size_continuous(name = 'Richter scale value') +
#'scale_color_continuous(name = '# of Deaths')+
#'theme(legend.position="bottom", legend.box="horizontal")
#' }
#'
#'
#'@export
geom_timeline <- function(
mapping = NULL, data = NULL, position = "identity", stat = "identity", na.rm = FALSE, show.legend = NA,
inherit.aes = TRUE, ...) {

  ggplot2::layer(geom = GeomTimeline, data = data, mapping = mapping, stat = stat,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...)
  )
}

#'Geom function GeomTimeline that contains code
#'
#' @importFrom ggplot2 ggproto aes draw_key_point
#' @importFrom grid gList pointsGrob gpar segmentsGrob gpar
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,required_aes = "x",

default_aes = ggplot2::aes(y=0, colour="black", shape=19, size=1,
                           stroke = 0.5, alpha = 0.5, fill = NA),

draw_key = ggplot2::draw_key_point,

draw_panel = function(data, panel_params, coord) {
  coords <- coord$transform(data, panel_params)
  grid::gList(grid::pointsGrob(coords$x, coords$y,pch = coords$shape,
             gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
             fill = alpha(coords$fill, coords$alpha))),

              grid::segmentsGrob(x0 = min(coords$x), y0 = coords$y,
                                 x1 = max(coords$x), y1 = coords$y,
                                 gp = grid::gpar(col = "darkgrey", lwd = 1)))
}
)
