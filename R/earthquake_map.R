#' @title Function to map NOAA earthquake data
#'
#'Function eq_map()
#'
#'@description This function takes an argument data containing
#'the filtered data frame with earthquakes to visualize. The function maps
#'the epicentres (LATITUDE/LONGITUDE) and annotates each point with in pop
#'up window containing annotation data stored in a column of the data frame.
#'The user can choose which column is used for the annotation
#'in the pop-up with a function argument named annot_col. Each earthquake
#'is shown with a circle, and the radius of the circle should be
#'proportional to the earthquake's magnitude
#'
#'@importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @param earthquake_track_clean, a dataframe obtained after runing functions eq_clean_data()
#' and eq_location_clean(). Initial raw data should be downloaded from NOAA website
#' @param annot_col name of column in the dataframe, that should be used
#' for annotation
#'
#' @return This function returns a map with earthquake epicentres and annotations
#' within pop-up window
#'
#' @examples
#' \dontrun{
#' earthquake_track_clean %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  eq_map(annot_col = "DATE")
#' }
#'
#'
#' @export
eq_map <- function(earthquake_track_clean, annot_col) {
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(data = earthquake_track_clean, lng = earthquake_track_clean$LONGITUDE, lat = earthquake_track_clean$LATITUDE,
                              radius = as.numeric(earthquake_track_clean$EQ_PRIMARY), popup = earthquake_track_clean[[annot_col]],
                              stroke = FALSE, weight=1, fillOpacity = 0.3)
}

#'Function eq_create_label()
#'
#'@description This function Create a function takes the dataset as an argument
#'and creates an HTML label that can be used as the annotation text in the leaflet map.
#'This function puts together a character string for each earthquake that shows
#'the cleaned location, the magnitude (EQ_PRIMARY), and the total number of deaths
#'(TOTAL_DEATHS), with #boldface labels for each ("Location", "Total deaths", and
#'"Magnitude"). If an earthquake is missing values for any of these, both the label
#'and the value should be skipped for that element of the tag.
#'
#' @importFrom dplyr if_else
#'
#' @param clean_data, a dataframe obtained after runing functions eq_clean_data()
#'and eq_location_clean(). Initial raw data should be downloaded from NOAA website

#'
#' @return This function creates HTML label with annotations
#'
#' @examples
#' \dontrun{
#' clean_data %>%
#' dplyr::filter(COUNTRY == "IRAN" & lubridate::year(DATE) >= 2016) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#'
#'
#' @export
eq_create_label<- function(clean_data) {

  paste0(
    dplyr::if_else(is.na(clean_data$LOCATION_NAME),"", paste("<b>Location: </b>",clean_data$LOCATION_NAME,"<br/>")),
    dplyr::if_else(is.na(clean_data$EQ_PRIMARY),"", paste("<b>Magnitude: </b>",clean_data$EQ_PRIMARY,"<br/>")),
    dplyr::if_else(is.na(clean_data$TOTAL_DEATHS),"", paste("<b>Total deaths: </b>",clean_data$TOTAL_DEATHS,"<br/>")))

}
