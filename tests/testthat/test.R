context("CapestoneEarthquake")
fileName<-system.file("extdata","signif.txt",package="CapestoneEarthquake")
earthquake_track <- readr::read_tsv(fileName, col_names = TRUE)
as.data.frame(earthquake_track)

#Test eq_clean_data
test_that('eq_clean_data: Checking the earthquake_track_clean is a data frame', {
  earthquake_track_clean <- CapestoneEarthquake::eq_clean_data(earthquake_track)
  expect_that(earthquake_track_clean,testthat::is_a("data.frame"))
})


test_that('eq_clean_data: Checking the earthquake_track_clean data contains DATE column', {
  earthquake_track_clean <- eq_clean_data(earthquake_track)
  expect_match(names(earthquake_track_clean), 'DATE', all=F)
})

test_that('eq_clean_data: Checking that Latitude is numeric', {
  earthquake_track_clean <- eq_clean_data(earthquake_track)
  expect_that(earthquake_track_clean$LATITUDE,testthat::is_a("numeric"))
})


test_that('eq_clean_data: Checking that Longitude is numeric', {
  earthquake_track_clean <- eq_clean_data(earthquake_track)
  expect_that(earthquake_track_clean$LONGITUDE,testthat::is_a("numeric"))
})



#Test eq_location_clean function
test_that('eq_location_clean: Checking the earthquake_track_clean is a data frame', {
  earthquake_track_clean <- eq_location_clean(eq_clean_data(earthquake_track))
  expect_that(earthquake_track_clean,testthat::is_a("data.frame"))
})



test_that('eq_location_clean: Location name changed', {
    earthquake_track_clean <- eq_location_clean(earthquake_track)
  differences <- earthquake_track$LOCATION_NAME != earthquake_track_clean$LOCATION_NAME
  expect_equal(sum(!is.na(earthquake_track_clean$LOCATION_NAME)), sum(differences, na.rm = T))
})

test_that('eq_location_clean: Checking the LOCATION is in title case', {
    earthquake_track_clean <- eq_location_clean(earthquake_track)
  expect_equal(earthquake_track_clean$LOCATION_NAME, stringr::str_to_title(earthquake_track_clean$LOCATION_NAME))
})

#geom_timeline
timeline_plot<-
  ggplot2::ggplot() +
  geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY))
expect_that(timeline_plot, is_a("ggplot"))


#geom_timeline_label
timeline_label_plot<-
  ggplot2::ggplot() +
  geom_timeline(aes(x = DATE,y = COUNTRY,color = TOTAL_DEATHS,size = EQ_PRIMARY)) +
  geom_timeline_label(aes(label=LOCATION_NAME),n_max=10)
expect_that(timeline_label_plot, is_a("ggplot"))


#Test eq_map function
test_that('eq_map: the result is a leaflet', {
  earthquake_track_clean <- eq_location_clean(eq_clean_data(earthquake_track))
  expect_that(earthquake_track_clean %>%
                dplyr::filter(COUNTRY == 'MEXICO' & lubridate::year(DATE) >= 2000) %>%
                eq_map(annot_col = 'DATE'), testthat::is_a('leaflet'))
})

#Test eq_create_label function

test_that('eq_create_label: the result is a leaflet', {
  earthquake_track_clean <- eq_location_clean(eq_clean_data(earthquake_track))
  expect_that(earthquake_track_clean %>%
                dplyr::filter(COUNTRY == 'MEXICO' & lubridate::year(DATE) >= 2000) %>%
                dplyr::mutate(popup_text = eq_create_label(.)) %>%
                eq_map(annot_col = 'popup_text'), testthat::is_a('leaflet'))
})


