###############################################################################
# Primary Authors: Jahred Liddie, Katie O'Brien
# Purpose: statistical analyses
# Date created: 3/2/2026
###############################################################################

library(broom)
library(tidyverse)

dat <- read_csv("data/processed_data/analytic_dataset.csv")

###############################################################################
# primary model:
m1a <- glm(n_events ~ inundation_exposure*hurricane_3week +
            inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
          offset = log(total_population),
          data = dat, family = "quasipoisson")

# no separate pre-trend for exposed/control
m2 <- glm(n_events ~ inundation_exposure*hurricane_3week +
             as.factor(year) + as.factor(month), 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

# season fixed effects
m3 <- glm(n_events ~ inundation_exposure*hurricane_3week +
            inundation_exposure*as.factor(year) + inundation_exposure*season, 
          offset = log(total_population),
          data = dat, family = "quasipoisson")

# varying hurricane period:
m1b <- glm(n_events ~ inundation_exposure*hurricane_5week +
            inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
          offset = log(total_population),
          data = dat, family = "quasipoisson")

m1c <- glm(n_events ~ inundation_exposure*hurricane_8week +
             inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

# TODO: exclude 5-week period after initial 3 weeks, intercept shift, slope shift?

# adding temperature variables:
# incl mean daily temp and humidity
m4a <- glm(n_events ~ inundation_exposure*hurricane_3week +
            inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
            tdmean + humidity, 
          offset = log(total_population),
          data = dat, family = "quasipoisson")

# incl max temp and humidity
m4b <- glm(n_events ~ inundation_exposure*hurricane_3week +
            inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
            tmax + humidity, 
          offset = log(total_population),
          data = dat, family = "quasipoisson")

# incl min temp and humidity
m4c <- glm(n_events ~ inundation_exposure*hurricane_3week +
             inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
             tmin + humidity, 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

# TODO: lagged temp and humidity

# comparing top and bottom quartiles of flooding metric
dat_quartiles <- dat %>%
  filter(quartile_flood_value %in% c(1, 4)) %>%
  mutate(quartile_flood_value = as.factor(quartile_flood_value))

m1_quartiles <- glm(n_events ~ quartile_flood_value*hurricane_3week +
                      quartile_flood_value*as.factor(year) + quartile_flood_value*as.factor(month), 
                    offset = log(total_population),
                    data = dat_quartiles, family = "quasipoisson")

# non-controlled ITS models:
m1_ITS <- glm(n_events ~ hurricane_3week +
            as.factor(year) + as.factor(month), 
            offset = log(total_population),
            data = dat, family = "quasipoisson")

m1_ITS_season <- glm(n_events ~ hurricane_3week +
                       as.factor(year) + as.factor(month), 
                     offset = log(total_population),
                     data = dat, family = "quasipoisson")

################################################################################

all_models <- ls()[grepl("^m[[:digit:]]", ls())]

all_final_coefs <- map_dfr(all_models, ~tidy(eval(as.name(.x))),
                           .id = "model_id")

# TODO: make plots of coefficient estimates
all_final_coefs <- all_final_coefs %>%
  mutate(model_id = as.numeric(model_id),
         # model_type = case_when(...)
  )
