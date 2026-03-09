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
  # m2 <- glm(n_events ~ inundation_exposure*hurricane_3week +
  #              as.factor(year) + as.factor(month), 
  #            offset = log(total_population),
  #            data = dat, family = "quasipoisson")

# season fixed effects
m2 <- glm(n_events ~ inundation_exposure*hurricane_3week +
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

all_models

all_final_coefs <- map_dfr(all_models, 
                           ~tidy(eval(as.name(.x)), conf.int = TRUE, exponentiate = TRUE),
                           .id = "model_id")

all_final_coefs <- all_final_coefs %>%
  mutate(model_id = as.numeric(model_id),
         model_type = case_when(model_id == 1 ~ "Non-controlled ITS",
                                model_id == 2 ~ "Non-controlled ITS (season FEs)",
                                model_id == 3 ~ "CITS: top and bottom flooding quartiles",
                                model_id == 4 ~ "CITS: main model (3-week)",
                                model_id == 5 ~ "CITS: main model (5-week)",
                                model_id == 6 ~ "CITS: main model (8-week)",
                                model_id == 7 ~ "CITS: season FEs",
                                model_id == 8 ~ "CITS: mean daily temp + humidity",
                                model_id == 9 ~ "CITS: max daily temp + humidity",
                                model_id == 10 ~ "CITS: min daily temp + humidity"),
         model_group = case_when(model_id %in% c(4, 5, 6) ~ "CITS: main model",
                                 model_id %in% c(1, 2) ~ "ITS",
                                 model_id %in% c(7, 8, 9, 10) ~ "Sensitivity analyses")
  )

all_final_fit <- map_dfr(all_models, ~glance(eval(as.name(.x))),
                         .id = "model_id")

all_final_fit$model_id <- as.numeric(all_final_fit$model_id)

all_final_summary <- left_join(all_final_coefs, all_final_fit)

ggplot(all_final_summary %>% 
         filter(model_group == "CITS: main model" &
                grepl("inundation_exposureTRUE:hurricane", term))) +
  geom_errorbar(aes(x = model_type, y = estimate,
                    ymin = conf.low, ymax = conf.high, color = model_group),
                width = 0.5, size = 2, position = position_dodge(width = 0.6)) +
  geom_point(aes(x = model_type, y = estimate), 
             color = "black", size = 4, position = position_dodge(width = 0.6)) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = MetBrewer::met.brewer(name = "Egypt")) +
  geom_hline(yintercept = 1, color = "darkgrey", linetype = "dashed") +
  labs(y = "Incidence rate ratio", x = "") +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title = element_text(size = 12, color = "black"),
        # legend.key.height = unit(0.25, "cm"),
        # legend.key.width = unit(4, "cm"),
        # legend.text.position = "top",
        # legend.text = element_text(size = 10),
        legend.position = "none",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

if (FALSE) {
  ggsave("figures/main_results.png", dpi = 600, height = 3, width = 6)
}

ggplot(all_final_summary %>% 
         filter(model_group == "Sensitivity analyses" &
                  grepl("inundation_exposureTRUE:hurricane", term))) +
  geom_errorbar(aes(x = model_type, y = estimate,
                    ymin = conf.low, ymax = conf.high, color = model_group),
                width = 0.5, size = 2, position = position_dodge(width = 0.6)) +
  geom_point(aes(x = model_type, y = estimate), 
             color = "black", size = 4, position = position_dodge(width = 0.6)) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = MetBrewer::met.brewer(name = "Egypt")) +
  geom_hline(yintercept = 1, color = "darkgrey", linetype = "dashed") +
  labs(y = "Incidence rate ratio", x = "") +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 8, angle = 60, hjust = 1, color = "black", face = "bold"),
        # legend.key.height = unit(0.25, "cm"),
        # legend.key.width = unit(4, "cm"),
        # legend.text.position = "top",
        # legend.text = element_text(size = 10),
        legend.position = "none",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

if (FALSE) {
  ggsave("figures/initial_sensitivity_results.png", dpi = 600, height = 4, width = 6)
}
