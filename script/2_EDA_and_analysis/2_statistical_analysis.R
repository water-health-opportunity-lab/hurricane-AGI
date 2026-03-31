###############################################################################
# Primary Authors: Jahred Liddie, Katie O'Brien
# Purpose: statistical analyses
# Date created: 3/2/2026
###############################################################################

library(broom)
library(MASS)
library(tidyverse)

dat <- read_csv("data/processed_data/analytic_dataset.csv")

dat_masked <- read.csv("data/processed_data/dataset_with_added_masked_units.csv")

state_by_week <- read_csv("data/processed_data/state_by_week_with_added_masked_units.csv")

###############################################################################
# primary model:
m1a <- glm(n_events ~ inundation_exposure*hurricane_3week +
            inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
          offset = log(total_population),
          data = dat, family = "quasipoisson")

  # # Check the residuals by plotting against time
  # res2 <- residuals(m2, type="deviance")
  # pacf(res2)
  # 
  # plot(dat$days_since_anchor,res2,ylim=c(-30,30),pch=19,cex=0.7,col=grey(0.6),
  #      main="Residuals over time",ylab="Deviance residuals",xlab="Date")
  # abline(h=0,lty=2,lwd=2)

# no separate pre-trend for exposed/control
  # m2 <- glm(n_events ~ inundation_exposure*hurricane_3week +
  #              as.factor(year) + as.factor(month), 
  #            offset = log(total_population),
  #            data = dat, family = "quasipoisson")

# varying hurricane period:
m1b <- glm(n_events ~ inundation_exposure*hurricane_5week +
             inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

m1c <- glm(n_events ~ inundation_exposure*hurricane_8week +
             inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
           offset = log(total_population),
           data = dat, family = "quasipoisson")

# season fixed effects
m2 <- glm(n_events ~ inundation_exposure*hurricane_3week +
            inundation_exposure*as.factor(year) + inundation_exposure*season, 
          offset = log(total_population),
          data = dat, family = "quasipoisson")

# negative binomial model
m3a <- glm.nb(n_events ~ inundation_exposure*hurricane_3week +
                inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
                offset(log(total_population)),
              data = dat)

m3b <- glm.nb(n_events ~ inundation_exposure*hurricane_5week +
                inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
                offset(log(total_population)),
              data = dat)

m3c <- glm.nb(n_events ~ inundation_exposure*hurricane_8week +
                inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
                offset(log(total_population)),
              data = dat)

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

# excluding 5 week period after the initial 3 weeks
dat_excl5weeks <- dat %>% filter( !(hurricane_8week & hurricane_3week == F))

m5 <- glm(n_events ~ inundation_exposure*hurricane_3week +
            inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month),
          offset = log(total_population),
          data = dat_excl5weeks, family = "quasipoisson")

# TODO: lagged temp and humidity

# comparing top and bottom quartiles of flooding metric
dat_quartiles <- dat %>%
  filter(quartile_flood_value %in% c(1, 4)) %>%
  mutate(quartile_flood_value = as.factor(quartile_flood_value))

m1a_quartiles <- glm(n_events ~ quartile_flood_value*hurricane_3week +
                      quartile_flood_value*as.factor(year) + quartile_flood_value*as.factor(month), 
                    offset = log(total_population),
                    data = dat_quartiles, family = "quasipoisson")

# non-controlled ITS model at the zip3 level:
m1a_ITS <- glm(n_events ~ hurricane_3week +
            as.factor(year) + as.factor(month), 
            offset = log(total_population),
            data = dat, family = "quasipoisson")

m1b_ITS <- glm(n_events ~ hurricane_5week +
                as.factor(year) + as.factor(month), 
              offset = log(total_population),
              data = dat, family = "quasipoisson")

m1c_ITS <- glm(n_events ~ hurricane_8week +
                as.factor(year) + as.factor(month), 
              offset = log(total_population),
              data = dat, family = "quasipoisson")

################################################################################
# sensitivity analyses below are based on the following: 
  # 1) a dataset in which events that occurred in masked geographic units 
    # were split (according to total population) into zip3 units
  # 2) a dataset at the state by week level that includes events that occurred in
    # masked geographic units

m1a_masked <- glm(n_events ~ inundation_exposure*hurricane_3week +
                    inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
                  offset = log(total_population),
                  data = dat_masked, family = "quasipoisson")

m1b_masked <- glm(n_events ~ inundation_exposure*hurricane_5week +
                   inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
                  offset = log(total_population),
                  data = dat_masked, family = "quasipoisson")

m1c_masked <- glm(n_events ~ inundation_exposure*hurricane_8week +
                   inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month), 
                  offset = log(total_population),
                  data = dat_masked, family = "quasipoisson")

# non-controlled ITS model at the state level:
m1a_ITS_state <- glm(n_events ~ hurricane_3week +
                      as.factor(year) + as.factor(month), 
                    offset = log(total_population),
                    data = state_by_week, family = "quasipoisson")

m1b_ITS_state <- glm(n_events ~ hurricane_5week +
                      as.factor(year) + as.factor(month), 
                    offset = log(total_population),
                    data = state_by_week, family = "quasipoisson")

m1c_ITS_state <- glm(n_events ~ hurricane_8week +
                      as.factor(year) + as.factor(month), 
                    offset = log(total_population),
                    data = state_by_week, family = "quasipoisson")

################################################################################
all_models <- ls()[grepl("^m[[:digit:]]", ls())]

all_models

all_final_coefs <- map_dfr(all_models, 
                           ~tidy(eval(as.name(.x)), conf.int = TRUE, exponentiate = TRUE),
                           .id = "model_id")

all_final_coefs <- all_final_coefs %>%
  mutate(model_id = as.numeric(model_id),
         model_type = case_when(model_id == 1 ~ "CITS: main model (3-week)",
                                model_id == 2 ~ "Non-controlled ITS (3-week)",
                                model_id == 3 ~ "Non-controlled ITS: state-level (3-week)",
                                model_id == 4 ~ "CITS: incl. masked units (3-week)",
                                model_id == 5 ~ "CITS: top and bottom flooding quartiles",
                                model_id == 6 ~ "CITS: main model (5-week)",
                                model_id == 7 ~ "Non-controlled ITS (5-week)",
                                model_id == 8 ~ "Non-controlled ITS: state-level (5-week)",
                                model_id == 9 ~ "CITS: incl. masked units (5-week)",
                                model_id == 10 ~ "CITS: main model (8-week)",
                                model_id == 11 ~ "Non-controlled ITS (8-week)",
                                model_id == 12 ~ "Non-controlled ITS: state-level (8-week)",
                                model_id == 13 ~ "CITS: incl. masked units (8-week)",
                                model_id == 14 ~ "CITS: season FEs",
                                model_id == 15 ~ "CITS: negative binomial (3-week)",
                                model_id == 16 ~ "CITS: negative binomial (5-week)",
                                model_id == 17 ~ "CITS: negative binomial (8-week)",
                                model_id == 18 ~ "CITS: mean daily temp + humidity",
                                model_id == 19 ~ "CITS: max daily temp + humidity",
                                model_id == 20 ~ "CITS: min daily temp + humidity",
                                model_id == 21 ~ "CITS: excl. 5-week period"),
         model_group = case_when(grepl("CITS: main model", model_type) ~ "CITS: main model",
                                 grepl("CITS: incl. masked units", model_type) ~ "CITS: incl. masked events",
                                 grepl("Non-controlled ITS", model_type) ~ "ITS",
                                 TRUE ~ "Sensitivity analyses")
  )

all_final_fit <- map_dfr(all_models[!grepl("^m3", all_models)], ~glance(eval(as.name(.x))),
                         .id = "model_id")

all_final_fit_nb <- map_dfr(all_models[grepl("^m3", all_models)], ~glance(eval(as.name(.x))),
                            .id = "model_id")

all_final_fit_nb <- all_final_fit_nb %>%
  mutate(model_id = 15:17,
         logLik = as.numeric(logLik))
         
all_final_fit <- all_final_fit %>%
  mutate(model_id = ifelse(as.numeric(model_id) >= 15, as.character(as.numeric(model_id) + 3), as.character(model_id)))

all_final_fit <- rbind(all_final_fit, all_final_fit_nb)

all_final_fit$model_id <- as.numeric(all_final_fit$model_id)

all_final_summary <- left_join(all_final_coefs, all_final_fit)

all_final_summary <- all_final_summary %>%
  mutate(plot_estimate = paste(format(round(estimate, 2), nsmall = 2), 
                               " [", format(round(conf.low, 2), nsmall = 2), 
                               ", ", format(round(conf.high, 2), nsmall = 2), "]", sep = "")
         )

ggplot(all_final_summary %>% 
         filter(model_group == "CITS: main model" &
                grepl("inundation_exposureTRUE:hurricane", term))) +
  geom_errorbar(aes(y = model_type, x = estimate,
                    xmin = conf.low, xmax = conf.high, color = model_group),
                width = 0.5, size = 2, position = position_dodge(width = 0.6)) +
  geom_point(aes(y = model_type, x = estimate), 
             color = "black", size = 4, position = position_dodge(width = 0.6)) +
  geom_text(aes(y = model_type, x = estimate, label = plot_estimate), 
            color = "black", size = 4, vjust = -1) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = MetBrewer::met.brewer(name = "Egypt")) +
  geom_vline(xintercept = 1, color = "darkgrey", linetype = "dashed") +
  xlim(0.25, 2) +
  labs(x = "Incidence rate ratio", y = "") +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, color = "black", face = "bold"),
        legend.position = "none",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

if (FALSE) {
  ggsave("figures/main_results.png", dpi = 600, height = 3, width = 6)
}

ggplot(all_final_summary %>% 
         filter(model_group == "ITS" & grepl("hurricane", term))) +
  geom_errorbar(aes(y = model_type, x = estimate,
                    xmin = conf.low, xmax = conf.high, color = model_group),
                width = 0.5, size = 2, position = position_dodge(width = 0.6)) +
  geom_point(aes(y = model_type, x = estimate), 
             color = "black", size = 4, position = position_dodge(width = 0.6)) +
  geom_text(aes(y = model_type, x = estimate, label = plot_estimate), 
            color = "black", size = 4, vjust = -1.75) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = MetBrewer::met.brewer(name = "Egypt")) +
  geom_vline(xintercept = 1, color = "darkgrey", linetype = "dashed") +
  xlim(0.25, 2) +
  labs(x = "Incidence rate ratio", y = "") +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, color = "black", face = "bold"),
        legend.position = "none",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

if (FALSE) {
  ggsave("figures/noncontrolled_ITS_results.png", dpi = 600, height = 4.5, width = 6)
}

ggplot(all_final_summary %>% 
         filter(model_group == "CITS: incl. masked events" &
                grepl("inundation_exposureTRUE:hurricane", term))) +
  geom_errorbar(aes(y = model_type, x = estimate,
                    xmin = conf.low, xmax = conf.high, color = model_group),
                width = 0.5, size = 2, position = position_dodge(width = 0.6)) +
  geom_point(aes(y = model_type, x = estimate), 
             color = "black", size = 4, position = position_dodge(width = 0.6)) +
  geom_text(aes(y = model_type, x = estimate, label = plot_estimate), 
            color = "black", size = 4, vjust = -2.5) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = MetBrewer::met.brewer(name = "Egypt")) +
  geom_vline(xintercept = 1, color = "darkgrey", linetype = "dashed") +
  xlim(0.25, 2) +
  labs(x = "Incidence rate ratio", y = "") +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, color = "black", face = "bold"),
        legend.position = "none",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

if (FALSE) {
  ggsave("figures/CITS_results_with_masked_units.png", dpi = 600, height = 4, width = 6)
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
  ylim(0.25, 3.25) +
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
  ggsave("figures/sensitivity_results.png", dpi = 600, height = 4, width = 6)
}
