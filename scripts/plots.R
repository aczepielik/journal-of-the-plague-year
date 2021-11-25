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

## basic plots ----

plot1 <- burials %>% 
  filter(counttype == "buried") %>%
  right_join(weeks %>% select(weekID, year, begindate2), by = "weekID") %>% 
  filter(year < 1665) %>% 
  group_by(begindate2) %>% 
  summarise(countn = sum(countn)) %>% 
  ggplot(aes(begindate2, countn)) + 
  geom_line(size = 0.5) +
  scale_y_continuous(expand = expansion(add = c(0, 200))) +
  labs(y = "Number of burials",
       title = "Weekly burials in London: XII 1643 - XII 1664",
       caption = "Based on: Smith, Richard and Davenport, Romola and Newton, Gill (2020).
       London weekly bills of mortality, 1644-1849. [Data Collection].
       Colchester, Essex: UK Data Service. 10.5255/UKDA-SN-854104") +
  theme(axis.title.x = element_blank(), plot.caption = element_text(size = 8))

plot1

plot2 <- burials %>% 
  filter(counttype == "buried") %>%
  right_join(weeks %>% select(weekID, year, begindate2), by = "weekID") %>% 
  filter(year <= 1665) %>% 
  group_by(begindate2) %>% 
  summarise(countn = sum(countn)) %>% 
  ggplot(aes(begindate2, countn)) + 
  geom_line(size = 1) +
  scale_y_continuous(expand = expansion(add = 0)) +
  labs(y = "Number of burials",
       title = "Weekly burials in London: XII 1643 - XII 1665",
       caption = "Based on: Smith et al. (2020) DOI:10.5255/UKDA-SN-854104") +
  theme(axis.title.x = element_blank(), plot.caption = element_text(size = 8))

plot2

plot3 <- burials %>% 
  filter(counttype == "buried") %>%
  right_join(weeks %>% select(weekID, year, begindate2), by = "weekID") %>% 
  filter(year == 1665) %>% 
  group_by(begindate2) %>% 
  summarise(countn = sum(countn)) %>% 
  ggplot(aes(begindate2, countn)) + 
  geom_line(size = 1) +
  scale_y_continuous(expand = expansion(add = 0)) +
  labs(y = "Number of burials",
       title = "Weekly burials in London in 1665",
       caption = "Based on: Smith et al. (2020) DOI:10.5255/UKDA-SN-854104") +
  theme(axis.title.x = element_blank(), plot.caption = element_text(size = 8))

plot3

plot4 <- burials %>% 
  filter(counttype == "buried" | counttype == "plague") %>%
  right_join(weeks %>% select(weekID, year, begindate2), by = "weekID") %>% 
  filter(year <= 1665) %>% 
  group_by(begindate2, counttype) %>% 
  summarise(countn = sum(countn)) %>% 
  tidyr::pivot_wider(id_cols = begindate2, names_from = counttype, values_from = countn) %>% 
  mutate(countn = buried - if_else(is.na(plague), 0, plague)) %>% 
  ggplot(aes(begindate2, countn)) + 
  geom_line(size = 1) +
  scale_y_continuous(expand = expansion(add = 0)) +
  labs(y = "Number of burials",
       title = "Weekly burials besides those of plague:\nXII 1643 - XII 1665",
       caption = "Based on: Smith et al. (2020) DOI:10.5255/UKDA-SN-854104") +
  theme(axis.title.x = element_blank(), plot.caption = element_text(size = 8))

plot4

## Excess deaths by cause ----

source("scripts/excess_deaths.R")

plot5 <- excesss_death_by_cause %>% 
  ggplot(aes(begindate2, codn)) +
  geom_line() + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(add = 0, mult = 0)) +
  facet_wrap(~codspellcorrex, scales = "free_y", ncol = 3) +
  labs(y = "Weekly number of registered burials",
       title = "Exces deaths by cause",
       subtitle = "In order of average burials in 1665",
       caption = "Based on: Smith et al. (2020) DOI:10.5255/UKDA-SN-854104") +
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(size = 8),
        strip.text = element_text(size = 9))

plot5

## severity and time ----
source("scripts/clusters.R")

parishes_to_plot <- 
joined_parishes %>% 
  left_join(relative_burials_summary, by = "parcode") %>% 
  select(parcode, parish = parish.x, max_relative_burials, when)

plot6 <- parishes_to_plot %>% 
  ggplot(aes(fill = max_relative_burials)) + geom_sf() +
  scale_fill_distiller(
    palette = "Spectral",
    limits = c(1, NA),
    labels = scales::percent,
    name = "") +
  theme_void() + 
  legend_bottom() + 
  labs(title = "The highest number of burials relative to average") +
  guides(fill = guide_colourbar(barwidth = 18))

plot6

plot7 <- parishes_to_plot %>% ggplot(aes(fill = when)) + geom_sf() +
  guides(fill = guide_legend(reverse = FALSE)) +
  scale_fill_distiller(palette = "Spectral",
                       trans = "date",
                       limits = c(as.Date("1665-06-15"), as.Date("1665-10-15")),
                       oob = scales::squish,
                       direction = 1,
                       name = "Time of the highest bills:") +
  theme_void() + legend_bottom()

plot7

plot8 <- parcode_clusters(clusts, 5) %>% 
  full_join(relative_burials, by = "parcode") %>% 
  ggplot(aes(begindate2, relative_burials,
             color = factor(cluster))) + 
  geom_smooth(alpha = 0.15) + 
  scale_colour_manual(values = swatch()[c(2, 3, 4, 9, 5)],
                      na.value = "grey85") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title = "Five types of the plague course",
       y = "Number of burials as % of average") + 
  theme(legend.position = "none", axis.title.x = element_blank())

plot8

plot9 <- joined_parishes %>% 
  left_join(parcode_clusters(clusts, 5)) %>% 
  select(parish = parish.x, cluster) %>% 
  ggplot(aes(fill = factor(cluster))) + geom_sf() +
  scale_fill_manual(values = swatch()[c(2, 3, 4, 9, 5)], na.value = "grey85") +
  theme_void() + 
  theme(legend.position = "none")

plot9

