################################################################################
# Primary authors: Jahred Liddie
# Purpose: statistical analyses for Florence
# Date created: 3/2/2026
################################################################################
library(broom)
library(MASS)
library(spdep)
library(tigris)
library(tidyverse)

dat <- read_csv("data/processed_data/florence_analytic_dataset.csv")
dat$zip3 <- as.character(dat$zip3)

source("script/2_EDA_and_analysis/analysis_functions.R")

################################################################################
# getting zcta shapes using tigris package - 2020 is most recent available
zcta_geometry <- zctas(year = 2020, cb = TRUE)

# filter for NC
nc_zcta <- zcta_geometry %>%
  filter(str_starts(GEOID20, "27") | str_starts(GEOID20, "28"))

# aggregate into zip3 
nc_zip3_geom <- nc_zcta %>%
  mutate(zip3 = substr(GEOID20, 1, 3)) %>%
  group_by(zip3) %>%
  summarise(
    geometry = st_union(geometry)
  )

dat_neighbors <- dat %>%
    group_by(zip3) %>%
    slice(1) %>%
    mutate(zip3 = as.character(zip3)) %>%
    ungroup() %>%
    mutate(id = as.factor(row_number()))

dat_neighbors <- left_join(nc_zip3_geom, dat_neighbors, by = "zip3")
dat_neighbors <- st_as_sf(dat_neighbors, coords = geometry, crs = st_crs(nc_zip3_geom))
  
nb <- poly2nb(dat_neighbors, queen = TRUE)

dat_neighbors <- map_dfr(1:20, ~id_neighbors.f(row_numbers = .x))
dat_neighbors <- dat_neighbors %>% 
  dplyr::select(zip3, id, neighbors) %>%
  st_drop_geometry()

dat <- left_join(dat, dat_neighbors)

dat <- dat %>% 
  group_by(week_start) %>% 
  rowwise() %>%
  mutate(neighbor_weight = 1/length(unlist(neighbors))) %>%
  mutate(neighbor_cases_weighted = sum( neighbor_weight * dat$n_events[dat$id %in% unlist(neighbors) &  dat$week_start == week_start]),
         neighbor_cases_unweighted = sum( dat$n_events[dat$id %in% unlist(neighbors) & dat$week_start == week_start])) %>%
  ungroup()
  
###############################################################################
three_weeks <- unique(dat$week_start[dat$hurricane_3week])
five_weeks <- unique(dat$week_start[dat$hurricane_5week])
eight_weeks <- unique(dat$week_start[dat$hurricane_8week])

# primary model:
m_initial <- glm(n_events ~ inundation_exposure*hurricane_3week +
                   inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
                 offset = log(total_population),
                 data = dat, family = "quasipoisson")

initial_three_week_SAC <- map_dfr(three_weeks, 
                                  ~eval_SAC.f(hurricane_week = .x, 
                                              model_dataset = dat, model = m_initial))

# including spatially lagged cases
m1a <- glm(n_events ~ inundation_exposure*hurricane_3week +
             inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
             log(neighbor_cases_weighted + 1), 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

three_week_SAC <- map_dfr(three_weeks, 
                          ~eval_SAC.f(hurricane_week = .x, 
                                      model_dataset = dat, model = m1a))

  # check the residuals by plotting against time:
  # dat$resid <- residuals(m1a, type="deviance")
  # 
  # plot(dat$weeks_since_anchor, dat$resid,
  #      ylim=c(-60,60),pch=19,cex=0.7,col=grey(0.6),
  #      main="Residuals over time",ylab="Deviance residuals",xlab="Date")
  # abline(h=0,lty=2,lwd=2)

# varying hurricane period:
m1b <- glm(n_events ~ inundation_exposure*hurricane_5week +
             inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
             log(neighbor_cases_weighted + 1), 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

  # # check the residuals by plotting against time
  # dat$resid <- residuals(m1b, type="deviance")
  # 
  # plot(dat$weeks_since_anchor, dat$resid,
  #      ylim=c(-60,60),pch=19,cex=0.7,col=grey(0.6),
  #      main="Residuals over time",ylab="Deviance residuals",xlab="Date")
  # abline(h=0,lty=2,lwd=2)

five_week_SAC <- map_dfr(five_weeks, 
                          ~eval_SAC.f(hurricane_week = .x, model_dataset = dat, model = m1b))

m1c <- glm(n_events ~ inundation_exposure*hurricane_8week +
             inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
             log(neighbor_cases_weighted + 1), 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

eight_week_SAC <- map_dfr(eight_weeks, 
                          ~eval_SAC.f(hurricane_week = .x, model_dataset = dat, model = m1c))

  # # check the residuals by plotting against time
  # dat$resid <- residuals(m1c, type="deviance")
  # 
  # plot(dat$weeks_since_anchor, dat$resid,
  #      ylim=c(-60,60),pch=19,cex=0.7,col=grey(0.6),
  #      main="Residuals over time",ylab="Deviance residuals",xlab="Date")
  # abline(h=0,lty=2,lwd=2)

################################################################################
all_models <- ls()[grepl("^m[[:digit:]]", ls())]

all_models

all_final_coefs <- map_dfr(all_models, 
                           ~tidy(eval(as.name(.x)), conf.int = TRUE, exponentiate = TRUE),
                           .id = "model_id")

all_final_coefs <- all_final_coefs %>%
  mutate(model_id = as.numeric(model_id),
         model_type = case_when(model_id == 1 ~ "CITS: Florence model (3-week)",
                                model_id == 2 ~ "CITS: Florence model (5-week)",
                                model_id == 3 ~ "CITS: Florence model (8-week)"),
         model_group = "Florence model")

all_final_fit <- map_dfr(all_models, ~glance(eval(as.name(.x))),
                         .id = "model_id")
         
all_final_fit$model_id <- as.numeric(all_final_fit$model_id)

all_final_summary <- left_join(all_final_coefs, all_final_fit)

all_final_summary <- all_final_summary %>%
  mutate(plot_estimate = paste(format(round(estimate, 2), nsmall = 2), 
                               " [", format(round(conf.low, 2), nsmall = 2), 
                               ", ", format(round(conf.high, 2), nsmall = 2), "]", sep = "")
         )

if (FALSE) {
  write.csv(all_final_summary %>% 
              dplyr::filter(grepl("inundation_exposureTRUE:hurricane", term)),
            "tables/Florence_model_results.csv")
  
}
