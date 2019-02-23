CapestoneEarthquake
-------------------

This package contains functions to visualise earthquakes from a dataset obtained from the U.S. National Oceanographic and Atmospheric Administration (NOAA). The functions are: - data cleaning functions: eq\_clean\_data() and eq\_location\_clean - geoms to visualise earthquakes magnitude and number of death by selected countries on the timeline: geom\_timeline() and geom\_timeline\_label() - functions mapping the earthquakes and label them eq\_map() and eq\_create\_label()

Obtain data
-----------

Download data from NOAA website, save it and load:

``` r
library(readr)

NOAA_raw_data<-system.file("extdata","signif.txt",package="CapestoneEarthquake")
earthquake_track <- readr::read_tsv(NOAA_raw_data, col_names = TRUE)
as.data.frame(earthquake_track)
```

This function returns a dataframe "earthquake\_track"

Data cleaning
-------------

Before running visualisations clean the data using function eq\_data\_clean(). This function creats the data column by uniting the year, month, day and converting it to the Date class. LATITUDE and LONGITUDE columns are converted to numeric class and some other data (number of death, magnitude) are converted to numeric.

``` r
library(dplyr)
library(magrittr)
 
earthquake_track_clean<-eq_clean_data(earthquake_track)
```

Next cleaning function eq\_location\_clean strips out the country name (including the colon) from the LOCATION\_NAME column and converts names to title case (as opposed to all caps)

``` r
library(dplyr)
library(stringr)
library(magrittr)
 
earthquake_track_clean<-eq_location_clean(earthquake_track_clean)
```

Visualisation on the plot
-------------------------

Make a plot to visualize the times, the magnitudes and the number of deaths associated to earthquakes within certain countries, use the geom\_timeline with the `ggplot` function:

``` r
library(ggplot2)
library(grid)

earthquake_track_clean %>%
  dplyr::filter(COUNTRY %in% c('JAPAN', 'IRAN',YEAR > 2010)) %>%
  ggplot() +
  geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
                    size = EQ_PRIMARY)) +
  scale_size_continuous(name = 'Richter scale value') +
  scale_color_continuous(name = '# of Deaths')+
  theme(legend.position="bottom", legend.box="horizontal")
```

In order to add labels to the plot use the geom\_timelinelabel. It adds annotations to the earthquake data and an option to subset the n\_max largest earthquakes by magnitude:

``` r
library(ggplot2)
library(grid)
library(dplyr)

earthquake_track_clean%>%
  dplyr::filter(COUNTRY %in% c("JAPAN","IRAN","PERU") & lubridate::year(DATE) >= 2016) %>%
  ggplot() +
  geom_timeline(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
  geom_timeline_label(aes(label=LOCATION_NAME),n_max=3)+
  scale_size_continuous(name = 'Richter scale value') +
  scale_color_continuous(name = '# of Deaths')+
  theme(legend.position="bottom", legend.box="horizontal")
```

Visualisation on the map
------------------------

eq\_map function helps to visualize earthquake epicentres of the certain region for the selected period. The column from the data can be chosen for annotation that will be in pop-up windows:

``` r
eq_clean_data(earthquake_track) %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
  eq_map(annot_col = "DATE")
```

To add more information to annotations use eq\_create\_lable function together with eq\_map. This function adds the Location, Total deaths, and Magnitude of the earthquakes:

``` r
eq_clean_data(earthquake_track) %>%
  dplyr::filter(COUNTRY == "IRAN" & lubridate::year(DATE) >= 2016) %>% 
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")
```
