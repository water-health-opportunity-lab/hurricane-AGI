###############################################################################
# Primary Authors: Jahred Liddie, Katie O'Brien
# Purpose: EDA of case data and covariates
# Date created: 3/2/2026
###############################################################################

library(tidyverse)

dat <- read_csv("data/processed_data/analytic_dataset.csv")

dat_masked <- read.csv("data/processed_data/dataset_with_added_masked_units.csv")

state_by_week <- read_csv("data/processed_data/state_by_week_with_added_masked_units.csv")

###############################################################################
# 1) exploratory plots:
# a few different versions of aggregated data:
  # 1) week-level only
agg_dat <- dat %>%
  group_by(date, year) %>%
  summarise(sum_events = sum(n_events),
            sum_pop = sum(total_population),
            hurricane_3week = unique(hurricane_3week),
            hurricane_5week = unique(hurricane_5week),
            hurricane_8week = unique(hurricane_8week)) %>%
  ungroup() %>%
  mutate(case_rate_per10k = sum_events/sum_pop*1e4)

agg_dat_masked <- dat_masked %>%
  group_by(date, year) %>%
  summarise(sum_events = sum(n_events),
            sum_pop = sum(total_population),
            hurricane_3week = unique(hurricane_3week),
            hurricane_5week = unique(hurricane_5week),
            hurricane_8week = unique(hurricane_8week)) %>%
  ungroup() %>%
  mutate(case_rate_per10k = sum_events/sum_pop*1e4)

agg_exposure_dat <- dat %>%
  group_by(date, year, inundation_exposure) %>%
  summarise(sum_events = sum(n_events),
            sum_pop = sum(total_population),
            hurricane_3week = unique(hurricane_3week),
            hurricane_5week = unique(hurricane_5week),
            hurricane_8week = unique(hurricane_8week),
            # mean of means for ppt and temp
            ppt_mean = mean(ppt_mean),
            tmean = mean(tmean)) %>%
  ungroup() %>%
  mutate(case_rate_per10k = sum_events/sum_pop*1e4,
         inundation_exposure_plot = ifelse(inundation_exposure, "Flooded", "Non-flooded"))

dat_masked <- dat_masked %>%
  mutate(date = as.Date(date))
  
agg_exposure_dat_masked <- dat_masked %>%
  group_by(date, year, inundation_exposure) %>%
  summarise(sum_events = sum(n_events),
            sum_pop = sum(total_population),
            hurricane_3week = unique(hurricane_3week),
            hurricane_5week = unique(hurricane_5week),
            hurricane_8week = unique(hurricane_8week),
            # mean of means for ppt and temp
            ppt_mean = mean(ppt_mean),
            tmean = mean(tmean)) %>%
  ungroup() %>%
  mutate(case_rate_per10k = sum_events/sum_pop*1e4,
         inundation_exposure_plot = ifelse(inundation_exposure, "Flooded", "Non-flooded"))

agg_exposure_quartiles_dat <- dat %>%
  group_by(date, year, quartile_flood_value) %>%
  summarise(sum_events = sum(n_events),
            sum_pop = sum(total_population),
            hurricane_3week = unique(hurricane_3week),
            hurricane_5week = unique(hurricane_5week),
            hurricane_8week = unique(hurricane_8week),
            # mean of means for ppt and temp
            ppt_mean = mean(ppt_mean),
            tmean = mean(tmean)) %>%
  ungroup() %>%
  mutate(case_rate_per10k = sum_events/sum_pop*1e4)

dat <- dat %>%
  mutate(case_rate_per10k = n_events/total_population*1e4,
         inundation_exposure_plot = ifelse(inundation_exposure, "Flooded", "Non-flooded"))

# evidence of slightly seasonality (higher in winter, lower in spring/summer/fall)
  # and increasing trend by year:

# fully aggregated plot by year
ggplot(agg_dat, aes(x = date, y = sum_events)) +
  geom_point(size = 0.3) +
  geom_line(linewidth = 0.75) +
  labs(x = "Date", y = "Total AGI visits") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75))

ggplot(agg_dat, aes(x = date, y = sum_events)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.25) +
  facet_wrap(~year, scales = "free") +
  labs(x = "Date", y = "Total AGI visits") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75))

  # very similar:
  # ggplot(agg_exposure_dat, aes(x = date, y = sum_events, color = inundation_exposure)) +
  #   geom_point() +
  #   geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  #   facet_wrap(~year, scales = "free_x") +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 0.75))

# pre-post plots for each group (2024 only)
ggplot(agg_dat %>% filter(year == 2024), aes(x = date, y = sum_events)) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-11-15"),
                ymin = -Inf,
                ymax = Inf), fill = 'lightgrey', alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-25"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-11"),
                ymin = -Inf,
                ymax = Inf), fill = 'darkgrey', alpha = 0.05) +
  geom_point(color = "blue") +
  ylim(0, 2500) +
  geom_line(color = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, color = "darkblue") +
  labs(x = "Date", y = "Total AGI visits") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75))

if (FALSE) {
  ggsave("figures/state_level_cases_trend.png", dpi = 600, height = 4, width = 6)
}

ggplot(agg_exposure_dat %>% filter(year == 2024 & month(date) >= 6), 
       aes(x = date, y = sum_events, color = inundation_exposure_plot, group = inundation_exposure_plot)) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-11-15"),
                ymin = -Inf,
                ymax = Inf), fill = 'lightgrey', color = "lightgrey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-25"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color = "grey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-11"),
                ymin = -Inf,
                ymax = Inf), fill = 'darkgrey', color = "darkgrey", alpha = 0.5) +
  geom_point() +
  geom_line() +
  geom_label(x = as.Date('2024-10-04'), y = 1200, label = "3 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-10-18'), y = 1200, label = "5 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-11-05'), y = 1200, label = "8 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  labs(x = "Date", y = "Total AGI visits") +
  scale_color_manual(values = c("Flooded" = "darkblue", "Non-flooded" = "darkred"),
                     name = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom")

if (FALSE) {
  ggsave("figures/treatment_group_level_cases_trend.png", dpi = 600, height = 4, width = 6.5)
}

ggplot(agg_exposure_dat %>% filter(year == 2024 & month(date) >= 6), 
       aes(x = date, y = case_rate_per10k, color = inundation_exposure_plot)) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-11-15"),
                ymin = -Inf,
                ymax = Inf), fill = 'lightgrey', color = "lightgrey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-25"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color = "grey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-11"),
                ymin = -Inf,
                ymax = Inf), fill = 'darkgrey', color = "darkgrey", alpha = 0.5) +
  geom_point() +
  geom_line() +
  geom_label(x = as.Date('2024-10-04'), y = 3.25, label = "3 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-10-18'), y = 3.25, label = "5 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-11-05'), y = 3.25, label = "8 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  labs(x = "Date", y = "AGI visits per 10k") +
  ylim(0.5, 3.3) +
  scale_color_manual(values = c("Flooded" = "darkblue", "Non-flooded" = "darkred"),
                     name = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom")

if (FALSE) {
  ggsave("figures/treatment_group_level_rate_trend.png", dpi = 600, height = 4, width = 6.5)
}

ggplot(agg_exposure_dat_masked %>% filter(year == 2024 & month(date) >= 6), 
       aes(x = date, y = case_rate_per10k, color = inundation_exposure_plot)) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-11-15"),
                ymin = -Inf,
                ymax = Inf), fill = 'lightgrey', color = "lightgrey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-25"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color = "grey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-11"),
                ymin = -Inf,
                ymax = Inf), fill = 'darkgrey', color = "darkgrey", alpha = 0.5) +
  geom_point() +
  geom_line() +
  geom_label(x = as.Date('2024-10-04'), y = 4.5, label = "3 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-10-18'), y = 4.5, label = "5 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-11-05'), y = 4.5, label = "8 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  labs(x = "Date", y = "AGI visits per 10k") +
  ylim(0.5, 4.5) +
  scale_color_manual(values = c("Flooded" = "darkblue", "Non-flooded" = "darkred"),
                     name = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom")

if (FALSE) {
  ggsave("figures/treatment_group_level_rate_trend_v2.png", dpi = 600, height = 4, width = 6.5)
}

ggplot(agg_exposure_quartiles_dat %>% filter(year == 2024 & month(date) >= 6), 
       aes(x = date, y = ppt_mean, color = as.factor(quartile_flood_value))) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-11-15"),
                ymin = -Inf,
                ymax = Inf), fill = 'lightgrey', color = "lightgrey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-25"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color = "grey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-11"),
                ymin = -Inf,
                ymax = Inf), fill = 'darkgrey', color = "darkgrey", alpha = 0.5) +
  geom_point() +
  geom_line() +
  geom_label(x = as.Date('2024-10-04'), y = 15.5, label = "3 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-10-18'), y = 15.5, label = "5 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  geom_label(x = as.Date('2024-11-05'), y = 15.5, label = "8 weeks", size = 2,
             color = "black", fill = NA, fontface = "bold") +
  labs(x = "Date", y = "Mean precipitation [mm]") +
  scale_color_brewer(name = "Flooding quartile") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom")

if (FALSE) {
  ggsave("figures/mean_precip_trend.png", dpi = 600, height = 4, width = 6.5)
}

# plotting several years over one another
agg_dat <- agg_dat %>%
  mutate(yday = lubridate::yday(date))
    
ggplot(agg_dat %>% filter(year %in% c(2021:2024)), 
       aes(x = yday, y = case_rate_per10k, 
           group =  as.factor(year), color = as.factor(year))) +
  geom_point() +
  geom_line() +
  xlim(1, 365) +
  labs(x = "Day of year", y = "AGI visits per 10k") +
  scale_color_manual(values = MetBrewer::met.brewer(name = "Egypt", n = 4),
                     name = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom")

if (FALSE) {
  ggsave("figures/year_comparison.png", dpi = 600, height = 4, width = 6)
}

# plotting several years over one another
agg_dat_masked <- agg_dat_masked %>%
  mutate(yday = lubridate::yday(date))

ggplot(agg_dat_masked %>% filter(year %in% c(2021:2024)), 
       aes(x = yday, y = case_rate_per10k, 
           group =  as.factor(year), color = as.factor(year))) +
  geom_point() +
  geom_line() +
  xlim(1, 365) +
  labs(x = "Day of year", y = "AGI visits per 10k") +
  scale_color_manual(values = MetBrewer::met.brewer(name = "Egypt", n = 4),
                     name = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom")

if (FALSE) {
  ggsave("figures/year_comparison_masked.png", dpi = 600, height = 4, width = 6)
}

ggplot(dat %>% filter(year == 2024 & month(date) >= 6), 
       aes(x = date, y = case_rate_per10k, color = inundation_exposure_plot, group = zip3)) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-11-15"),
                ymin = -Inf,
                ymax = Inf), fill = 'lightgrey', color = "lightgrey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-25"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color = "grey", alpha = 0.5) +
  geom_rect(aes(xmin = as.Date('2024-09-27'),
                xmax = as.Date("2024-10-11"),
                ymin = -Inf,
                ymax = Inf), fill = 'darkgrey', color = "darkgrey", alpha = 0.5) +
  geom_point() +
  geom_line() +
  geom_label(x = as.Date('2024-10-04'), y = 10.25, label = "3 weeks", size = 2.5,
             color = "black") +
  geom_label(x = as.Date('2024-10-18'), y = 10.25, label = "5 weeks", size = 2.5,
             color = "black") +
  geom_label(x = as.Date('2024-11-05'), y = 10.25, label = "8 weeks", size = 2.5,
             color = "black") +
  labs(x = "Date", y = "AGI visits per 10k") +
  scale_color_manual(values = c("Flooded" = "darkblue", "Non-flooded" = "darkred"),
                     name = "") +
  ylim(0, 10.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom")

if (FALSE) {
  ggsave("figures/zip3_level_rate_trend.png", dpi = 600, height = 4, width = 6)
}

###############################################################################

short_dat <- dat %>%
      filter(date >= as.Date("2024-09-27") - 21 & date < as.Date("2024-09-27") + 21)

threeweek_avgs <- short_dat %>%
  group_by(zip3, hurricane_3week) %>%
  summarise(mean_IR = mean(case_rate_per10k)) %>%
  ungroup()

t.test(y = threeweek_avgs$mean_IR[threeweek_avgs$hurricane_3week == TRUE], 
       x = threeweek_avgs$mean_IR[threeweek_avgs$hurricane_3week == FALSE], 
       data = threeweek_avgs, mu = 0, conf.level = 0.95, paired = TRUE,
       alternative = "two.sided", var.equal = FALSE)

summary(
  lm(case_rate_per10k ~ hurricane_3week, data = short_dat)
)

short_dat <- dat %>%
  filter(date >= as.Date("2024-09-27") - 21 & date < as.Date("2024-09-27") + 35)

summary(
  lm(case_rate_per10k ~ hurricane_5week, data = short_dat)
)

short_dat <- dat %>%
  filter(date >= as.Date("2024-09-27") - 21 & date < as.Date("2024-09-27") + 56)

summary(
  lm(case_rate_per10k ~ hurricane_8week, data = short_dat)
)
  
