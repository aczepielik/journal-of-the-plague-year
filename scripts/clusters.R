prev_parish_levels <- burials %>% 
  filter(counttype == "buried") %>% 
  right_join(weeks %>% filter(year > 1660 & year < 1665), by = "weekID") %>% 
  group_by(parcode) %>% 
  summarize(avg_burials = mean(countn))

relative_burials <- burials %>% 
  left_join(parishes %>% select(parcode, parish), by = "parcode") %>% 
  filter(counttype == "buried") %>% 
  right_join(weeks %>% filter(year == 1665, begindate2 > as.Date("1665-05-01")), by = "weekID") %>% 
  select(weekID, begindate2, parcode, parish, countn) %>% 
  inner_join(prev_parish_levels, by = "parcode") %>% 
  mutate(relative_burials = countn/avg_burials)

plot_one_parish <- function(selected_parcode, burials_data = relative_burials) {
  parish_name <- burials_data %>%
    filter(parcode == selected_parcode) %>%
    head(1) %>% pull(parish)
  
  ggplot(burials_data, aes(begindate2, relative_burials, group = parcode)) +
    geom_line(size = 2) + 
    gghighlight::gghighlight(
      parcode == selected_parcode,
      label_key = parish,
      use_group_by = FALSE,
      unhighlighted_params = list(alpha = 0.3, size = 0.7)) +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(parish_name, "in 1665"),
         subtitle = "Weekly number of burials\nas % of average number",
         y = "Relative burials")
}

relative_burials_summary <- 
  relative_burials %>% 
  group_by(parcode) %>% 
  summarise(max_relative_burials = max(relative_burials),
            when = begindate2[which.max(relative_burials)])

relative_burials_wide <- relative_burials %>% 
  tidyr::pivot_wider(id_cols = begindate2,
                     names_from = parcode,
                     values_from = relative_burials,
                     values_fill = 0)

mean_squared_distance <- function(code1, code2, data=relative_burials_wide) {
  vec1 <- data[[code1]]
  vec2 <- data[[code2]]
  
  return(mean((vec1 - vec2)^2, na.rm = TRUE))
}

distance_matrix <- 
  matrix(nrow = ncol(relative_burials_wide) - 1,
         ncol = ncol(relative_burials_wide) - 1) %>% 
  `colnames<-`(colnames(relative_burials_wide)[-1]) %>% 
  `rownames<-`(colnames(relative_burials_wide)[-1])

for (i in 1:nrow(distance_matrix)) {
  for (j in i:ncol(distance_matrix)){
    par1 <- rownames(distance_matrix)[i]
    par2 <- colnames(distance_matrix)[j]
    
    distance_matrix[i,j] <- distance_matrix[j,i] <- mean_squared_distance(par1, par2)
  }
}

clusts <- hclust(as.dist(distance_matrix), method = "ward.D2")

parcode_clusters <- function(clusts, k) {
  clusts %>%
    cutree(k = k) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("parcode") %>% 
    rename_at(".", function(x) "cluster")
}

## parish map
library(xml2)
library(reticulate)

svgpath2mpl <- import("svgpath2mpl")

reverse_y_axis_single <- function(mat) {
  mat[, 2] <- -mat[, 2] + 700
  mat/100
}

reverse_y_axis_many <- function(matrices){
  purrr::map(matrices, reverse_y_axis_single)
}

parishes_svg <- xml2::read_xml("sources/maps/parishes.svg")

parishes_shapes <- 
  parishes_svg %>% 
  xml_child(2) %>% 
  xml_children() %>% 
  purrr::map(function(node) data.frame(id = xml_attr(node, "id"),
                                       path = xml_attr(node, "d"),
                                       class = xml_attr(node, "class"))) %>% 
  bind_rows() %>% 
  filter(class == "Reds") %>% 
  mutate(coords = purrr::map(path,
                             function(p) svgpath2mpl$parse_path(p)$to_polygons() %>% reverse_y_axis_many()),
         shapes = purrr::map(coords, sf::st_polygon)) %>% 
  as_tibble() %>% 
  select(id, shapes) %>% 
  mutate(id = as.numeric(gsub("p_", "", id)))

library(jsonlite)

guardian <- fromJSON("sources/mortality-bills/data_from_guardian.json")

guardian_parishes <- 
  guardian$sheets$BurialsParishWeekly %>% 
  select(parish, mapkey)

library(fuzzyjoin)

guardian_parishes <- 
  guardian_parishes %>% 
  mutate(parish = stringr::str_replace_all(
    parish,
    c("All Hallows Bread Street" = "Allhallows Breadstreet",
      "All Hallows Honey Lane" = "	Allhallows Honylane",
      "All Hallows the Wall" = "Allhallows London Wall",
      "All Hallows Stayning/Staining" = "Allhallows Staining",
      "All Hallows Lumbard Street" = "Allhallows Lombardstreet",
      "Hackney Parish" = "South Hackney", # or west?
      "St Albans Wood Street" = "St Alban Woodstreet",
      "St Austins Parish" = "St Augustin/Austins",
      "St Brides Parish / St Bridget" = "St Bridget",
      "St Butolph/Botolph Billingsgate" = "St Botolph Billingsgate",
      "St Faith under St Pauls under St Pauls" = "St Faith under St Pauls",
      "St Faith" = "St Faith under St Pauls",
      "St Gabriel Fenchurch" = "St Gabriel Fenchurchstreet",
      "St Gabriel Fenchurchstreetstreet" = "St Gabriel Fenchurchstreet",
      "St Hellen/Hellins" = "St Helen near Bishopsgate",
      "St Kath. near the Tower" = "St Katharine near the Tower",
      "St Magnus Parish" = "St Magnus by London Bridge",
      "St Margaret New Fish Street" = "St Margaret Newfishstreet",
      "St Mary Islington" = "St Mary at Islington",
      "St Maudlin Old Fish Street" = "St Maudlin Oldfishstreet",
      "St Stephen Coleman Street" = "St Steven Colemanstreet",
      "St Vedast" = "St Foster",
      "Stepney Parish" = "St Dunstan Stepney",
      "Trinity Parish" = "Trinity Less",
      "Rothorith/Redriff Parish" = "St Mary Rothorithe",
      "St Maudlin Milk Street" = "St Mary Magd. Milkstreet",
      "Christchurch Greyfriars" = "Christchurch"
    ))) %>% 
  filter(mapkey != "") %>% 
  mutate(mapkey = as.numeric(mapkey))


joined_parishes <- 
  parishes_shapes %>% 
  left_join(guardian_parishes, by = c("id" = "mapkey")) %>%
  tidyr::replace_na(list(parish = "")) %>% 
  stringdist_left_join(parishes, by = c(parish = 'parish')) %>%
  filter(!(parish.x == 'Christchurch' & parcode != "CHRC")) %>% 
  sf::st_as_sf()

