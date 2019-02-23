#' @title Geom to add labels to the
#'
#'Geom function geom_timeline_label
#'
#'@description a geom for ggplot2 called geom_timeline_label() to add annotations to
#'the earthquake data. This geom adds a vertical line to each data point
#'with a text annotation (e.g. the location of the earthquake) attached
#'to each line. There is an option to subset to n_max number of
#'earthquakes, where we take the n_max largest (by magnitude) earthquakes.
#'Aesthetics are x, which is the date of the earthquake and label which
#'takes the column name from which annotations will be obtained.
#'
#'@details dataframe obtained after running functions eq_clean_data()
#'and eq_location_clean(). Initial raw data should be downloaded from
#'NOAA website. The data must be cleaned using the functions eq_clean_data and eq_location_clean,
#'that are included in the package. Aesthetics can be specified in geom_timeline or
#'in ggplot
#'
#'@return function produces a plot with the annotations of the earthquake data
#'
#' @inheritParams ggplot2::layer
#' @param mapping Set of aesthetic mappings created
#' @param data  The data to be displayed
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param stat  The statistical transformation to use on the data for this layer,
#' as a string.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#' a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them
#' @param ... Other arguments passed on to the layer. it can be used to set an
#' aesthetic to a fixed value.
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#'earthquake_tracking_clean %>%
#'dplyr::filter(COUNTRY %in% c("JAPAN","IRAN","PERU") & lubridate::year(DATE) >= 2016) %>%
#'ggplot() +
#'geom_timeline(aes(x = DATE,y = COUNTRY,color = TOTAL_DEATHS,size = EQ_PRIMARY)) +
#'geom_timeline_label(aes(label=LOCATION_NAME),n_max=10)+
#'scale_size_continuous(name = 'Richter scale value') +
#'scale_color_continuous(name = '# of Deaths')+
#'theme(legend.position="bottom", legend.box="horizontal")
#'}
#'
#'@export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelinelabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#'Geom function GeomTimelinelabel that contains code
#'
#' @importFrom ggplot2 ggproto aes draw_key_point
#' @importFrom dplyr arrange group_by slice
#' @importFrom grid polylineGrob gList
#'
#'
#' @export
GeomTimelinelabel <- ggplot2::ggproto("GeomTimelinelabel", ggplot2::Geom,
                      required_aes = c("x","label"),
                      default_aes = ggplot2::aes(y=0, n_max=0),
                      draw_key = ggplot2::draw_key_point,

                      draw_panel = function(data, panel_params, coord) {

                                    data<- data %>%
                                    dplyr::arrange(desc(size)) %>%
                                    dplyr::group_by(y) %>%
                                    dplyr::slice(1:data$n_max[1])

                      coords <- coord$transform(data, panel_params)

                                    lines <- grid::polylineGrob(
                                    x = unit(c(coords$x, coords$x), "npc"),
                                    y = unit(c(coords$y, coords$y + 0.08), "npc"),
                                    id = rep(1:dim(coords)[1], 2),
                                    gp = grid::gpar(col = "grey"))

                                    names <- grid::textGrob(
                                    label = coords$label,
                                    x = unit(coords$x, "npc"),
                                    y = unit(coords$y + 0.08, "npc"),
                                    just = c("left", "bottom"),
                                    rot = 45,
                                    gp=gpar(fontsize=8, col = "black"))

                                    grid::gList(lines, names)

                                      }
)
