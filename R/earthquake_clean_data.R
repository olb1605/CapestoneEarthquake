####
if(getRversion()>='2.15.1')
  utils::globalVariables(c("LOCATION_NAME","I_D","YEAR","MONTH","DAY","LATITUDE","LONGITUDE",
                           "EQ_PRIMARY","COUNTRY","STATE","TOTAL_DEATHS","DATE","YEAR4"))
#' @title Function to cleans NOAA earthquake and localisation data
#'
#'
#'Function eq_clean_data()
#'
#'@description This function returns a date frame having the following:
#' 1.A date column created by uniting the year, month, day and converting it
#' to the Date class
#' 2. LATITUDE and LONGITUDE columns converted to numeric class
#' 3. converting data to numeric class
#'
#' @importFrom readr read_delim
#' @importFrom dplyr select mutate
#' @importFrom magrittr "%>%"
#'
#' @param earthquake_track raw dataframe obtained after reading NOAA Significant Earthquake
#' Database tab-separated file
#'
#'
#' @return This function returns a data frame with clean data ready to make a plot
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(magrittr)
#' library(readr)
#'
#' earthquake_tracka_clean<-eq_clean_data(earthquake_track)
#' }
#'
#'
#' @export

eq_clean_data <- function(earthquake_track) {

    #1.A date column created by uniting the year, month, day and converting it
  #to the Date class

   earthquake_track_clean <- earthquake_track %>%

    dplyr::mutate(MONTH = ifelse(is.na(MONTH), "01", MONTH))%>%
    dplyr::mutate(DAY = ifelse(is.na(DAY), "01", DAY))%>%
    dplyr::mutate(DATE = as.Date(julian(as.numeric(MONTH), as.numeric(DAY), as.numeric(YEAR)),
                                 origin = "1970-01-01"))%>%

    #2. LATITUDE and LONGITUDE columns converted to numeric class
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%

    # other data cleaning
    dplyr::mutate(EQ_PRIMARY = as.numeric(EQ_PRIMARY))%>%
    dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS))

    as.data.frame(earthquake_track_clean)

}

#'Function eq_location_clean()
#'
#'@description This function cleans the LOCATION_NAME column by stripping out
#'the country name (including the colon) and converts names to title case
#'(as opposed to all caps)
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_replace str_trim str_to_title
#'
#' @param earthquake_track_clean obtained after runnign eq_clean_data() function on
#' NOAA Significant Earthquake Database tab-separated file
#'
#' @return This function returns a data frame with clean location names
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(stringr)
#' library(magrittr)
#'
#' earthquake_track_clean<-eq_location_clean(earthquake_track_clean)
#' }
#'
#'
#' @export

eq_location_clean <- function(earthquake_track_clean) {

  earthquake_track_clean<-earthquake_track_clean %>%

    dplyr::mutate(LOCATION_NAME = LOCATION_NAME %>%
                    stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                    stringr::str_trim("both") %>%
                    stringr::str_to_title())
  as.data.frame(earthquake_track_clean)

}
