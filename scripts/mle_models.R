library(dplyr)
library(deSolve)
library(bbmle)
library(ggplot2)
library(ggthemr)

Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.setlocale("LC_MEASUREMENT", 'en_US.UTF-8')
Sys.setlocale("LC_PAPER", 'en_US.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")

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


# data ----
weeks <- readRDS("sources/mortality-bills/cleaned/weeks.rds")
burials <- readRDS("sources/mortality-bills/cleaned/burials.rds")

plague_burials <- 
  burials %>% 
  filter(counttype == "plague") %>%
  full_join(weeks, by = "weekID") %>% 
  filter(weekID >= "1665/20" & weekID < "1666/01") %>% 
  group_by(begindate2) %>% 
  summarise(countn = sum(countn, na.rm = TRUE)) %>% 
  mutate(int_cum_deaths = cumsum(countn))

interpolated_days_int <- seq(as.integer(first(plague_burials$begindate2)),
                             as.integer(last(plague_burials$begindate2)),
                             by = 1)

int_cum_deaths <- spline(
  plague_burials$begindate2,
  plague_burials$int_cum_deaths,
  xout = interpolated_days_int, method = "hyman")

weekly_plague_burials <- 
  tibble(day = as.integer(plague_burials$begindate2) - as.integer(plague_burials$begindate2[1]) + 1,
         int_cum_deaths = plague_burials$int_cum_deaths)

interpolated_plague_burials <- 
  tibble(day = int_cum_deaths$x - int_cum_deaths$x[1] + 1,
         int_cum_deaths = as.integer(int_cum_deaths$y))

# utils ----
softplus <- function(x) log(1 + exp(-abs(x))) + max(x,0)

# model HE ----

model <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    
    N_h = Sh + Ilow + Ihigh + Rh
    
    new_Ilow = beta * Sh * Il / N_h 
    new_Ihigh_and_R = sigma * Ilow
    new_Ihigh = (1 - g) * new_Ihigh_and_R
    new_recovered = new_Ihigh_and_R - new_Ihigh
    new_dead = gamma_b * Ihigh
    
    dSh = - new_Ilow
    dIlow = new_Ilow - new_Ihigh_and_R
    dIhigh = new_Ihigh - new_dead
    dRh = new_recovered
    dDh = new_dead
    
    lice_K = K*N_h
    lice_births = r * Sl * (1 - (Sl + Il)/lice_K)
    
    
    new_I1_infected_lice = beta_low * Sl * Ilow/N_h
    new_I2_infected_lice = beta_high * Sl * Ihigh/N_h
    
    new_removed_lice = gamma_l * Il
    new_infected_lice = new_I1_infected_lice + new_I2_infected_lice
    
    
    dSl = lice_births - new_infected_lice
    dIl = new_infected_lice - new_removed_lice
    
    der <- c(dSh, dIlow, dIhigh, dRh, dDh, dSl, dIl)
    
    list(der)
  })
}

ode_pred <- function(steps, beta = 0.05, sigma = 1/8, gamma_b = 1/2, gamma_l = 1/3,
                     g = 0.4, r = 0.11, K = 15, beta_low = 0.05, beta_high = 0.5,
                     population = 360000, frac = 0.5, Ilow = 10, Ihigh = 2, Rh = 0,
                     Dh = 1){
  
  inits <- c(Sh = population * frac, Ilow = Ilow, Ihigh = Ihigh, Rh = Rh, Dh = Dh,
             Sl = K * (population*frac + Rh), Il = K * Ilow)
  
  params <- c(beta = beta, sigma = sigma, gamma_b = gamma_b, gamma_l = gamma_l,
              g = g, r = r, K = K, beta_low = beta_low, beta_high = beta_high)
  
  ode(inits, steps, model, params)
  
}

nll <- function(beta = 0.05, sigma = 1/8, gamma_b = 1/2, gamma_l = 1/3,
                g = 0.4, r = 0.11, K = 15, beta_low = 0.05, beta_high = 0.5,
                population = 360000, frac = 0.5, Ilow = 10, Ihigh = 2, Rh = 0,
                Dh = 1) {
  #day and int_cum_deaths come from data environment
  
  fits <- ode_pred(day, beta, sigma, gamma_b, gamma_l,
                   g, r, K, beta_low, beta_high,
                   population, frac, Ilow, Ihigh, Rh,
                   Dh)
  -sum(dpois(int_cum_deaths, fits[, 'Dh'], log = TRUE))
}

fixed <- list(
  beta = 0.05, sigma = 1/8, gamma_b = 1/2, gamma_l = 1/3,
  g = 0.4, r = 0.11, K = 15, population = 360000, Rh = 0, Dh = 1)

fit <- mle2(nll, fixed = fixed, data = interpolated_plague_burials,
            method = "L-BFGS-B", lower = 0)

fit_weekly_blind <- mle2(nll, fixed = fixed, data = weekly_plague_burials,
                         method = "L-BFGS-B", lower = 0)

fit_weekly <- mle2(nll, start = as.list(fit@coef), fixed = fixed, data = weekly_plague_burials,
                   method = "L-BFGS-B", lower = 0)

preds <- do.call(ode_pred, c(list(steps = interpolated_plague_burials$day), as.list(fit@fullcoef)))
preds_weekly <- do.call(ode_pred,c(list(steps = weekly_plague_burials$day), as.list(fit_weekly@fullcoef)))

as.data.frame(preds) %>% 
  bind_cols(interpolated_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = int_cum_deaths)) + geom_line(aes(y = Dh))

as.data.frame(preds) %>% 
  bind_cols(interpolated_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = c(0, diff(int_cum_deaths)))) + geom_line(aes(y = c(0, diff(Dh))))

as.data.frame(preds_weekly) %>% 
  bind_cols(weekly_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = c(0, diff(int_cum_deaths)))) + geom_line(aes(y = c(0, diff(Dh))))

plot12 <- as.data.frame(preds_weekly) %>% 
  bind_cols(plague_burials) %>% 
  mutate(diffD = pmax(0, c(9, diff(Dh)))) %>% 
  select(begindate2, Real = countn, Simulated = diffD) %>%
  tidyr::pivot_longer(cols = c(Real, Simulated)) %>% 
  ggplot(aes(begindate2, value, color = name)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  scale_colour_ggthemr_d() +
  labs(y = "Weekly burials",
       title = "Human - lice model",
       subtitle = "Susceptible fraction = 0.34, \u03b2low = 0.01, \u03b2high = 0.63, I0 low = 23.1, I0 high = 4.6") +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  annotate(geom = "text",
           x = plague_burials$begindate2[30],
           y = 0.9*max(diff(preds_weekly[,'Dh'])),
           label = paste0("BIC: ", round(BIC(fit_weekly), digits = 2)),
           size = 5
  )

plot12

# pneumonic model ----

model_pneumonic <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    
    N = S + I
    
    new_infected = beta * S * I / N 
    new_dead = gamma * I
    
    dS = - new_infected
    dI = new_infected - new_dead
    dD = new_dead
    
    der <- c(dS, dI, dD)
    
    list(der)
  })
}

ode_pred_pneumonic <- function(steps, beta = 0.5, gamma = 0.4,
                               population = 360000, frac = 0.5, I0 = 4, D0 = 1){
  
  inits <- c(S = population * frac, I = I0, D = D0)
  params <- c(beta = beta, gamma = gamma)
  
  ode(inits, steps, model_pneumonic, params)
}

nll_pneumonic <- function(beta = 0.5, gamma = 0.4,
                          population = 360000, frac = 0.5, I0 = 4, D0 = 1) {
  
  #day and int_cum_deaths come from data environment
  fits <- ode_pred_pneumonic(day, beta, gamma, population, frac, I0, D0)
  
  -sum(dpois(int_cum_deaths, fits[, 'D'], log = TRUE))
}

fixed_pneumonic <- list(gamma = 0.4, population = 360000)

fit_pneumonic <- mle2(nll_pneumonic, fixed = fixed_pneumonic,
                      data = interpolated_plague_burials,
                      method = "L-BFGS-B", lower = 0)

fit_pneumonic_weekly_blind <- mle2(nll_pneumonic, fixed = fixed_pneumonic,
                                   data = weekly_plague_burials,
                                   method = "L-BFGS-B", lower = 0)

fit_pneumonic_weekly <- mle2(nll_pneumonic, 
                             start = as.list(fit_pneumonic@coef),
                             fixed = fixed_pneumonic,
                             data = weekly_plague_burials,
                             method = "L-BFGS-B", lower = 0)

preds_pneumonic <- do.call(
  ode_pred_pneumonic,
  c(list(steps = interpolated_plague_burials$day), as.list(fit_pneumonic@fullcoef))
)

preds_pneumonic_weekly_initial <- do.call(
  ode_pred_pneumonic, 
  c(list(steps = weekly_plague_burials$day)))

preds_pneumonic_weekly <- do.call(
  ode_pred_pneumonic, 
  c(list(steps = weekly_plague_burials$day),
    as.list(fit_pneumonic_weekly_blind@fullcoef)))

as.data.frame(preds_pneumonic_weekly_initial) %>% 
  bind_cols(weekly_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = c(0, diff(int_cum_deaths)))) + geom_line(aes(y = c(0, diff(D))))

#official plot
plot10 <- 
  as.data.frame(preds_pneumonic_weekly_initial) %>% 
  bind_cols(plague_burials) %>% 
  mutate(diffD = pmax(0, c(9, diff(D)))) %>% 
  select(begindate2, Real = countn, Simulated = diffD) %>%
  tidyr::pivot_longer(cols = c(Real, Simulated)) %>% 
  ggplot(aes(begindate2, value, color = name)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  scale_colour_ggthemr_d() +
  labs(y = "Weekly burials",
       title = "Pneumonic model",
       subtitle = "Susceptible fraction = 0.5, \u03b2 = 0.5, Infected at the beginning = 4") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

as.data.frame(preds_pneumonic_weekly) %>% 
  bind_cols(weekly_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = c(0, diff(int_cum_deaths)))) + geom_line(aes(y = c(0, diff(D))))

plot11 <- 
  as.data.frame(preds_pneumonic_weekly) %>% 
  bind_cols(plague_burials) %>% 
  mutate(diffD = pmax(0, c(9, diff(D)))) %>% 
  select(begindate2, Real = countn, Simulated = diffD) %>%
  tidyr::pivot_longer(cols = c(Real, Simulated)) %>% 
  ggplot(aes(begindate2, value, color = name)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  scale_colour_ggthemr_d() +
  labs(y = "Weekly burials",
       title = "Pneumonic model",
       subtitle = "Susceptible fraction = 0.18, \u03b2 = 0.45, Infected at the beginning = 11") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) + 
  annotate(geom = "text",
           x = plague_burials$begindate2[30],
           y = 0.9*max(diff(preds_pneumonic_weekly[,'D'])),
           label = paste0("BIC: ", round(BIC(fit_pneumonic_weekly), digits = 2)),
           size = 5
  )

plot11

as.data.frame(preds_pneumonic) %>% 
  bind_cols(interpolated_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = int_cum_deaths)) + geom_line(aes(y = D))

as.data.frame(preds_pneumonic) %>% 
  bind_cols(interpolated_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = c(0, diff(int_cum_deaths)))) + geom_line(aes(y = c(0, diff(D))))

# rats model ----

model_rats <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    
    Nrat <- Srat + Irat + Rrat
    
    flee_to_rats <- Ff * (1 - exp(-a*Nrat))
    flee_to_humans <- Ff - flee_to_rats
    
    new_infected_rats <- beta_rat * Srat * flee_to_rats / Nrat
    new_removed_rats <- gamma_rat * Irat
    new_recovered_rats <- g_rat * new_removed_rats
    new_dead_rats <- new_removed_rats - new_recovered_rats
    
    dSrat <- - new_infected_rats
    dIrat <- new_infected_rats - new_removed_rats
    dRrat <- new_recovered_rats
    dDrat <- new_dead_rats
    
    dH <- r_flee * H * (1 - H/K_flee)
    dFf <- new_dead_rats * H - d_flee * Ff
    
    Nh <- Sh + Ih + Rh
    new_infected_humans <- beta_h * Sh * flee_to_humans / Nh
    new_removed_humans <- gamma_h * Ih
    new_recovered_humans <- g_h * new_removed_humans
    new_dead_humans <- new_removed_humans - new_recovered_humans
    
    dSh <- - new_infected_humans
    dIh <- new_infected_humans - new_removed_humans
    dRh <- new_recovered_humans
    dDh <- new_dead_humans
    
    der <- c(dSrat, dIrat, dRrat, dDrat, dH, dFf, dSh, dIh, dRh, dDh)
    
    list(der)
  }) 
}

ode_pred_rats <- function(steps, beta_rat = 0.5, gamma_rat = 1/5.2, g_rat = 0.1,
                          r_flee = 0.0084, K_flee = 6, d_flee = 0.2, a_nom = 3,
                          beta_h = 0.1, gamma_h = 0.1, g_h = 0.4,
                          Ff0 = 6, rat_mult = 1, Ir0 = 15, Rr0 = 0, Dr0 = 0,
                          population = 360000, frac = 0.5, I0 = 10, R0 = 0, D0 = 1){
  
  Srat0 = population * rat_mult
  a = a_nom/Srat0
  
  I0 = softplus(I0)
  #Ir0 = softplus(Ir0)
  Ff0 = softplus(Ff0)
  
  inits <- c(Srat = Srat0, Irat = Ir0, Drat = Dr0, Rrat = Rr0,
             H = K_flee, Ff = Ff0,
             Sh = population * frac, Ih = I0, Rh = R0, Dh = D0)
  
  params <- c(beta_rat = beta_rat, gamma_rat = gamma_rat, g_rat = g_rat,
              r_flee = r_flee, K_flee = K_flee, d_flee = d_flee, a = a,
              beta_h = beta_h, gamma_h = gamma_h, g_h = g_h)
  
  ode(inits, steps, model_rats, params)
}

nll_rats <- function(beta_rat = 0.5, gamma_rat = 1/5.2, g_rat = 0.1,
                     r_flee = 0.0084, K_flee = 6, d_flee = 0.2, a_nom = 3,
                     beta_h = 0.1, gamma_h = 0.1, g_h = 0.4,
                     Ff0 = -200, rat_mult = 1, Ir0 = 15, Rr0 = 0, Dr0 = 0,
                     population = 360000, frac = 0.5, I0 = 10, R0 = 0, D0 = 1) {
  
  #day and int_cum_deaths come from data environment
  fits <- ode_pred_rats(day, beta_rat, gamma_rat, g_rat,
                        r_flee, K_flee, d_flee, a_nom,
                        beta_h, gamma_h , g_h,
                        Ff0, rat_mult, Ir0, Rr0, Dr0,
                        population, frac, I0, R0, D0)
  
  -sum(dpois(int_cum_deaths, fits[, 'Dh'], log = TRUE))
}

fixed_rats <- list(
  gamma_rat = 1/5.2, g_rat = 0.1,
  r_flee = 0.0084, K_flee = 6, d_flee = 0.2, a_nom = 3,
  gamma_h = 0.1, g_h = 0.4, Rr0 = 0, Dr0 = 0,
  population = 360000, R0 = 0, D0 = 1
)

fit_rats <- mle2(nll_rats, fixed = fixed_rats, data = interpolated_plague_burials)

#fit_rats_weekly_blind <- mle2(nll_rats, fixed = fixed_rats, data = weekly_plague_burials)

fit_rats_weekly <- mle2(nll_rats, start = as.list(fit_rats@coef), 
                        fixed = fixed_rats, data = weekly_plague_burials)


preds_rats <- do.call(
  ode_pred_rats,
  c(list(steps = interpolated_plague_burials$day), as.list(fit_rats@fullcoef))
)

preds_rats_weekly <- do.call(
  ode_pred_rats, 
  c(list(steps = weekly_plague_burials$day),
    as.list(fit_rats_weekly@fullcoef)))

as.data.frame(preds_rats) %>% 
  bind_cols(interpolated_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = int_cum_deaths)) + 
  geom_line(aes(y = Dh))

as.data.frame(preds_rats) %>% 
  bind_cols(interpolated_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  #geom_point(aes(y = int_cum_deaths)) + 
  geom_line(aes(y = Srat))

as.data.frame(preds_rats) %>% 
  bind_cols(interpolated_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = c(0, diff(int_cum_deaths)))) + geom_line(aes(y = c(0, diff(Dh))))

as.data.frame(preds_rats_weekly) %>% 
  bind_cols(weekly_plague_burials) %>% 
  ggplot(aes(x = time)) + 
  geom_point(aes(y = c(0, diff(int_cum_deaths)))) + geom_line(aes(y = c(0, diff(Dh))))


plot13 <- 
  as.data.frame(preds_rats_weekly) %>% 
  bind_cols(plague_burials) %>% 
  mutate(diffD = pmax(0, c(9, diff(Dh)))) %>% 
  select(begindate2, Real = countn, Simulated = diffD) %>%
  tidyr::pivot_longer(cols = c(Real, Simulated)) %>% 
  ggplot(aes(begindate2, value, color = name)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  scale_colour_ggthemr_d() +
  labs(y = "Weekly burials",
       title = "Humans - fleas - rats model",
       subtitle = "Susceptible fraction = 0.77, \u03b2 humans= 0.34, I0 humans = 24\n\u03b2 rats= 0.068, I0 rats = 169, Rats:Humans = 0.93, Infected fleas at t0 = 0") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") + 
    annotate(geom = "text",
             x = plague_burials$begindate2[30],
             y = 0.9*max(diff(preds_rats_weekly[,'Dh'])),
             label = paste0("BIC: ", round(BIC(fit_rats_weekly), digits = 2)),
             size = 5
    )

plot13
