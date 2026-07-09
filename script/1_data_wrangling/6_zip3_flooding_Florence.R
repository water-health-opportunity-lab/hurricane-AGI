################################################################################
# primary authors: katie o'brien, jahred liddie
# purpose: This script imports the zip code data for the U.S., subsets it into 
# North Carolina, and then aggregates into zip 3 level. 
# It then imports the flood inundation data and calculates 
# the mean value in each zip3 level for Hurricane Florence. 
# date created: 6/12/26
################################################################################

# set up ------------
library(sf)
library(exactextractr)
library(terra)
library(tidyr)
library(tigris)
library(tidycensus)
library(tidyverse)
library(gganimate)
library(stringr)

################################################################################


# import flooding data

filepath <- ".../.../.../.../.../2-aims/aim3-AGI/2_raw_data/01_exposure_assessment/Hurricane Florence"

files <- list.files(filepath, pattern = "\\.tif$", full.names=TRUE)

raster_stack <- rast(files)

# checking one file to see spatial area of the raster files
file <- rast(".../.../.../.../.../2-aims/aim3-AGI/2_raw_data/01_exposure_assessment/Hurricane Florence/Florence_31N82W-42N74WFlood_byStor_1km_2018091621.tif")
plot(file)
# file covers mid-atlantic coast
crs(file)
# CRS is WGS 84

# confirm CRS of nc_final (transformed to WGS 84 in 1_zip3_wrangling.R script)
crs(nc_final)

# files cover eastern seaboard, need to clip to just be north carolina
nc_florence_stack <- crop(raster_stack, nc_final, mask = TRUE)

# confirming crop worked
plot(nc_florence_stack[[24]])
# adding zip3 outlines
plot(nc_final$geometry, add = TRUE)
# shows that the pixels are mainly NAs (NA = -9999)

# checking resolution
nc_florence_stack # 0.00833*0.00833 = 0.000069 km^2 or 69 m^2 resolution
st_area(nc_final)/(1000^2) # note that all areas > 156.5 km^2

# changing -9999 values to NA
nc_florence_stack[nc_florence_stack == -9999] <- NA
# confirm that it worked
summary(nc_florence_stack)

# function: compute area of non-NA cells intersecting each polygon
# while accounting for partial cell overlap
calc_nonNA_area.f <- function(r_layer, polys) {
  exact_extract(
    r_layer,
    polys,
    function(values, coverage_fractions) {
      
      # keep only non-NA raster values
      nonNA <- !is.na(values)
      
      sum(coverage_fractions[nonNA]) / sum(coverage_fractions)
    }
  )
}

# apply to all layers
nonNA_frac_list_fl <- lapply(seq_len(nlyr(nc_florence_stack)), function(i) {
  
  calc_nonNA_area.f(nc_florence_stack[[i]], nc_final)
  
})

# combine results
nonNA_frac_fl <- do.call(cbind, nonNA_frac_list_fl)
colnames(nonNA_frac_fl) <- paste0(names(nc_florence_stack), "_nonNA_fraction")


# bind results
final_df_fl <- cbind(nc_final, nonNA_frac_fl)

# cleaning up column names
final_df_fl <- final_df_fl %>% 
  rename_with(~paste0("Flood_NC_", str_extract(., "\\d{10}_nonNA_fraction")), 
              .cols = contains("nonNA_fraction"))



# creating new column with mean, median, and max % area flooded over the hurricane period
final_df_fl <- final_df_fl %>%
  rowwise() %>%
  mutate(mean_flood_value = mean(
    c_across(Flood_NC_2018091400_nonNA_fraction:Flood_NC_2018091621_nonNA_fraction), 
    na.rm=TRUE),
    median_flood_value = median(
      c_across(Flood_NC_2018091400_nonNA_fraction:Flood_NC_2018091621_nonNA_fraction), 
      na.rm=TRUE),
    max_flood_value = if(all(is.na(c_across(Flood_NC_2018091400_nonNA_fraction:Flood_NC_2018091621_nonNA_fraction)))) 
                                   NA_real_ else max(c_across(
                                     Flood_NC_2018091400_nonNA_fraction:Flood_NC_2018091621_nonNA_fraction), 
      na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(tertile_flood_value = ntile(mean_flood_value, 3),
         quartile_flood_value = ntile(mean_flood_value, 4))


final_df_fl <- final_df_fl %>%
  mutate(inundation_exposure = mean_flood_value > 0.5)


if (FALSE) {
  # shared drive: 
  write.csv(final_df_fl, ".../.../.../.../.../2-aims/aim3-AGI/3_processed_data/zip3_exposure_dataset.csv")
  
}


### figures

# histogram of inundation metrics for comparison
inundation_metrics <- final_df_fl %>%
  dplyr::select(zip3, mean_flood_value, median_flood_value, max_flood_value) %>%
  pivot_longer(cols = mean_flood_value:max_flood_value, 
               values_to = "perc_inundated", names_to = "metric") %>%
  mutate(metric_name = case_when(grepl("mean", metric) ~ "Mean % inundated",
                                 grepl("median", metric) ~ "Median % inundated",
                                 grepl("max", metric) ~ "Max % inundated"))

ggplot(inundation_metrics) +
  geom_histogram(aes(x = perc_inundated), bins = 20, color = "black", fill = "blue") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  geom_vline(xintercept = 0.5, color = "darkgrey", linetype = "dashed") +
  theme_bw() +
  labs(title = "Mean % zip3 areas with any inundation",
       subtitle = "Calculations based on 3-hour increments (9/24/24, 12:00am - 9/28/24, 9:00pm)", x = "% area inundated",
       y = "Count (n = 20)") +
  facet_wrap(~metric_name) +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 8),
        plot.subtitle = element_text( hjust = 0.5))

# only rewrite if there are edits:
if (FALSE) {
  ggsave("figures/florence_inundation_metric_comparison.png", dpi = 600, width = 7, height = 4)
}


# map of mean % area inundated
ggplot(final_df_fl) +
  geom_sf(aes(fill = mean_flood_value), color="white") +
  scale_fill_gradient(low = "white", high = "darkblue", labels = scales::percent,
                      breaks = seq(0.1, 1, by = 0.3), limits = c(0.1, 1),
                      name = "Mean % area inundated") +
  theme_void() +
  labs(title = "Mean % area inundated by zip3", 
       subtitle = "Calculations based on 3-hour increments (9/14/18, 12:00am - 9/16/18, 9:00pm)") +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text( hjust = 0.5),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        legend.spacing = unit(1, unit = 'cm')) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

# only rewrite if there are edits:
if (FALSE) {
  ggsave("figures/florence_mean_percent_inundated_map.png", dpi = 600, width = 7, height = 5)
}


