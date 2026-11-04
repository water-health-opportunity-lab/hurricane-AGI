# Primary Authors: Katie O'Brien, Jahred Liddie
# Purpose: This script pulls environmental data from prism and aggregates to 
# zip3 spatial scale.
# Date created: October 30, 2026
###############################################################################
library(prism)
library(terra)
library(sf)
library(tidyverse)
library(here)
library(exactextractr)
library(dplyr)

source(here("script", "1_data_wrangling", "1_zip3_wrangling.R"))

###############################################################################

# get prism estimates at daily level

prism_dir <- ".../.../.../.../prism_precip_daily"
prism_set_dl_dir(prism_dir)

# download prism data
get_prism_dailys(
  type = "ppt", 
  minDate = "2024-09-01", 
  maxDate = "2024-11-30", 
  keepZip = FALSE
)

files <- list.files(
  path = prism_dir, 
  pattern = "\\.bil$", 
  full.names = TRUE, 
  recursive = TRUE
)

# create raster stack
stack <- rast(files)

# pull date strings from file names
date_str <- str_extract(files, "\\d{8}")
dates <- as.Date(date_str, format = "%Y%m%d")
names(stack) <- paste0("ppt_", dates)

# check crs
crs(stack)
crs(nc_final)
# crs matches

# calculate area-weighted mean by zip3
precip_daily_zip3 <- exact_extract(
  stack, 
  nc_zip3_geom,
  fun = "mean", 
  append_cols = "zip3")

# rename columns
precip_daily_zip3 <- precip_daily_zip3 %>%
  rename_with(~ str_remove(., "^mean\\.ppt_"), starts_with("mean.ppt_"))

# pulling in exposure assignments
exp <- read_csv(".../.../.../.../.../.../aim3-AGI/3_processed_data/zip3_exposure_dataset.csv")
exp_c <- exp %>%
  mutate(zip3 = as.character(zip3)) %>%
  group_by(zip3) %>%
  summarize(inundation_exposure = first(inundation_exposure))

# join dataframes
precip_daily_zip3 <- precip_daily_zip3 %>%
  left_join(
    exp_c,
    by = "zip3"
  )

# pivot long
precip_daily_zip3_long <- precip_daily_zip3 %>%
  pivot_longer(
    cols = -c(zip3, inundation_exposure), 
    names_to = "date", 
    values_to = "ppt_mean"
  ) %>%
  mutate(date = as.Date(date)) %>%
  mutate(inundation_exposure_plot = ifelse(inundation_exposure, "Flooded", "Non-flooded"))

# plot
ggplot(precip_daily_zip3_long, 
         aes(x = date, y = ppt_mean, color = as.factor(inundation_exposure_plot))) +
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
  geom_point(size = 0.5) +
  geom_line() +
  facet_wrap(~zip3) +
  labs(x = "Date", y = "Mean daily precipitation [mm]") +
  scale_color_manual(values = c("Flooded" = "#4B637B",
                                "Non-flooded" = "#5C3B25"),
                     name = "Exposure group during Hurricane Helene") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
        legend.position = "bottom",
        legend.title.position = "top",
        strip.background = element_rect(fill = NA),
        legend.title.align = 0.5)

if (FALSE) {
  ggsave(here("figures", "mean_precip_trend_daily.png"), 
         dpi = 600, height = 6, width = 9)
}
