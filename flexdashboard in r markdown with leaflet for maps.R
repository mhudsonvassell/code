---
  title: "Bike Shares Daily"
output: 
  flexdashboard::flex_dashboard:
  orientation: columns
vertical_layout: fill
---
  
  ```{r setup, include=FALSE}
library(flexdashboard)
library(readr)
library(tidyverse)
library(leaflet)

stations_df <- read_csv('https://assets.datacamp.com/production/course_6355/datasets/stations_data.csv')
```



Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Stations Map
  
  ```{r}
leaflet(stations_df) %>%
  addTiles() %>%
  addMarkers()
```