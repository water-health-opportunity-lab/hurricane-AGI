################################################################################
# Primary authors: Jahred Liddie, Katie O'Brien
# Purpose: imputing masked cases by zip3
# Date created: 6/22/2026
################################################################################
library(broom)
library(MASS)
library(lme4)
library(tidyverse)

dat_masked <- read.csv("data/processed_data/dataset_with_added_masked_units_for_imputation.csv")
dat_masked$zip3 <- as.character(dat_masked$zip3)

################################################################################
# drop during/after hurricane
dat_masked_train <- dat_masked %>%
  filter(week_start < 20240927)

m1 <- glm(n_events_disagg ~ zip3 +
              as.factor(year) + as.factor(month) + weeks_since_anchor, 
            offset = log(total_population),
            data = dat_masked_train, 
            family = "poisson")

# m1a <- glmer(n_events_disagg ~ (1|zip3) +
#             as.factor(year) + as.factor(month) + scale(weeks_since_anchor), 
#           offset = log(total_population),
#           data = dat_masked_train, 
#           family = "poisson")

# m1b <- glm.nb(n_events_disagg ~ zip3 +
#             as.factor(year) + as.factor(month) + weeks_since_anchor +
#             offset(log(total_population)),
#           data = dat_masked_train)

m2 <- glm(n_events_disagg ~ zip3 + tmean + ppt_mean + humidity +
              as.factor(year) + as.factor(month) + weeks_since_anchor, 
            offset = log(total_population),
            data = dat_masked_train, 
            family = "poisson")

# m2a <- glmer(n_events_disagg ~ (1|zip3) + tmean + ppt_mean + humidity +
#             as.factor(year) + as.factor(month) + weeks_since_anchor,
#               offset = log(total_population),
#            data = dat_masked_train)

m2b <- glm.nb(n_events_disagg ~ zip3 + tmean + ppt_mean + humidity +
                as.factor(year) + as.factor(month) + weeks_since_anchor +
                offset(log(total_population)),
              data = dat_masked_train)

m3 <- glm(n_events_disagg ~ zip3*tmean + zip3*ppt_mean + zip3*humidity +
              as.factor(year) + as.factor(month) + weeks_since_anchor, 
            offset = log(total_population),
            data = dat_masked_train, 
            family = "poisson")

m4 <- glm(n_events_disagg ~ zip3 + tmean + ppt_mean + humidity +
              zip3*as.factor(year) + zip3*as.factor(month) + zip3*weeks_since_anchor, 
            offset = log(total_population),
            data = dat_masked_train, 
            family = "poisson")

m5 <- glm(n_events_disagg ~ zip3*tmean + zip3*ppt_mean + zip3*humidity +
              zip3*as.factor(year) + zip3*as.factor(month) + zip3*weeks_since_anchor, 
            offset = log(total_population),
            data = dat_masked_train, 
            family = "poisson")

all_poisson_models <- ls()[grepl("^m[[:digit:]]$", ls())]

all_nb_models <- ls()[grepl("^m[[:digit:]]b", ls())]

all_poisson_fit <- map_dfr(all_poisson_models, 
                           ~glance(eval(as.name(.x))),
                           .id = "model_id")

all_nb_fit <- map_dfr(all_nb_models, 
                           ~glance(eval(as.name(.x))),
                           .id = "model_id")

# TODO: predict final model and rerun main analysis
