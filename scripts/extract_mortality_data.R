library(jsonlite)
library(dplyr)

whole_site <- fromJSON("sources/mortality-bills/data_from_guardian.json")

parishes <- whole_site$sheets$BurialsParishWeekly

parish_meta <- 
  parishes %>% 
  select(matches("^((?!burial).)*$", perl = TRUE)) %>% 
  mutate_at(c("qgiskey", "mapkey", "longitude", "latitude"), as.numeric)

parish_total <- parishes %>% 
  select(mapkey, matches("^burial")) %>% 
  mutate_all(as.numeric) %>% 
  mutate_all(~tidyr::replace_na(., replace = 0))

parish_plague <- parishes %>% 
  select(mapkey, matches("^plague")) %>% 
  mutate_all(as.numeric) %>% 
  mutate_all(~tidyr::replace_na(., replace = 0))

causes <- whole_site$sheets$CausesDeathWeekly %>% 
  select(causeofdeath, starts_with("week")) %>% 
  mutate_at(vars(starts_with("week")), as.numeric) %>% 
  mutate_at(vars(starts_with("week")), ~tidyr::replace_na(., replace = 0))


