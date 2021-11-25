## libraries ----
library(dplyr)
library(ggplot2)
library(ggthemr)
library(leaflet)
library(leaflet.extras)

Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.setlocale("LC_MEASUREMENT", 'en_US.UTF-8')
Sys.setlocale("LC_PAPER", 'en_US.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")

## grapical settings ----
ggthemr("fresh")

swatch_pal <- function() {
  function(n) {
    if(n > length(swatch()) - 1) warning(paste("Swatch only has", length(swatch())-1), " colours")
    color_list <- as.character(swatch()[2:length(swatch())])
    return(color_list[1:n])
  }
}

scale_colour_ggthemr_d <- function(...) {
  ggplot2::discrete_scale(
    "colour", "ggthemr_swatch_color",
    swatch_pal(),
    ...
  )
}

scale_color_ggthemr_d <- scale_colour_ggthemr_d

## data -----

weeks <- readRDS("sources/mortality-bills/cleaned/weeks.rds")
burials <- readRDS("sources/mortality-bills/cleaned/burials.rds")
parishes <- readRDS("sources/mortality-bills/cleaned/parishes.rds")
cods <- readRDS("sources/mortality-bills/cleaned/cods.rds")


source("scripts/excess_deaths.R")
source("scripts/clusters.R")

parishes_to_plot <- 
  joined_parishes %>% 
  left_join(relative_burials_summary, by = "parcode") %>% 
  select(parcode, parish = parish.x, max_relative_burials, when)

individual_plots <- purrr::map(parishes_to_plot$parcode, plot_one_parish)
numpal <- colorNumeric("Spectral", parishes_to_plot$max_relative_burials, reverse = TRUE)

parishes_to_plot %>%
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 8)) %>% 
  setView(lng = 6.5, lat = 4, zoom = 6) %>% 
  addPolygons(weight = 2.2,
              color = "grey",
              fillColor = ~numpal(max_relative_burials),
              label = ~paste(parish),
              popup = leafpop::popupGraph(individual_plots),
              opacity = 1, fillOpacity = 0.7) %>% 
  setMapWidgetStyle(list(background= "white")) %>%
  htmlwidgets::saveWidget('interactive_london.en.html')
