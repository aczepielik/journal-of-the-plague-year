base_grid <- expand.grid(codspellcorrex = unique(cods$codspellcorrex),
                         weekID = weeks %>% 
                           filter(weekID >= "1650/01" & weekID < "1665/01") %>% 
                           pull(weekID))
prev <- 
  cods %>% 
  filter(weekID >= "1650/01" & weekID < "1665/01") %>% 
  full_join(base_grid, by = c("codspellcorrex", "weekID")) %>% 
  right_join(weeks %>% select(weekID, year, begindate2), by = "weekID") %>% 
  group_by(codspellcorrex, year) %>% 
  summarize(avg_deaths = mean(codn, na.rm = TRUE), n_weeks = sum(!is.na(codn))) %>% 
  tidyr::nest() %>% 
  ungroup() %>% 
  filter(purrr::map_lgl(data, ~sum(.x$n_weeks > 15) >= 8)) %>% 
  mutate(
    stl = purrr::map(data, ~as_tibble(forecast::mstl(.x$avg_deaths, t.degree = 0))),
    decomposed = purrr::map2(data, stl, ~bind_cols(.x, .y))) %>% 
  select(codspellcorrex, decomposed) %>% 
  tidyr::unnest(decomposed) %>% 
  group_by(codspellcorrex) %>% 
  summarize(level = last(Trend),
            dispersion = sd(Remainder, na.rm = TRUE),
            n = sum(!is.na(avg_deaths)))

plague_year <- 
  cods %>% 
  filter(weekID >= "1665/01" & weekID < "1666/01") %>% 
  group_by(codspellcorrex) %>% 
  summarise(avg = mean(codn, na.rm = TRUE))

excessive_cods <- 
  plague_year %>% 
  full_join(prev, by = "codspellcorrex") %>% 
  filter(level > 1) %>% 
  mutate(pval = pnorm(avg, mean=level, sd=dispersion, lower.tail = FALSE),
         pval_t = pt(sqrt(n) * (avg - level)/dispersion, df = n - 1, lower.tail = FALSE),
         adjusted = p.adjust(pval, "bonferroni"),
         t_adjusted = p.adjust(pval_t, "bonferroni"),
         signif = t_adjusted < 0.05) %>% 
  filter(signif) %>%
  arrange(desc(avg)) %>% 
  pull(codspellcorrex)

filtered_grid <- expand.grid(
  codspellcorrex = excessive_cods,
  weekID = weeks %>%
    filter(weekID >= "1650/01" & weekID < "1666/01") %>%
    pull(weekID))


excesss_death_by_cause <- 
cods %>% 
  filter(weekID >= "1650/01" & weekID < "1666/01" & codspellcorrex %in% excessive_cods) %>% 
  full_join(filtered_grid, by = c("codspellcorrex", "weekID")) %>% 
  right_join(weeks %>% 
               filter(weekID >= "1650/01" & weekID < "1666/01") %>% 
               select(weekID, year, begindate2), by = "weekID") %>% 
  group_by(codspellcorrex, begindate2) %>%
  summarise(codn = sum(codn)) %>% 
  mutate(codspellcorrex = factor(codspellcorrex, levels = excessive_cods, ordered = TRUE))

