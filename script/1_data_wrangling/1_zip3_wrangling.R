################################################################################
# primary authors: katie o'brien, jahred liddie
# purpose: This script imports the zip code data for the U.S., subsets it into 
  # North Carolina, and then aggregates into zip 3 level. 
  # It then imports the flood inundation data and calculates 
  # the mean value in each zip3 level. 
# date created: 11/14/2025
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

################################################################################
# set your API key, save your API key in .renviron, don't save it in the script
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

# population data ------------

# all population data
pop_fun <- function(year) {
  all_zctas <- get_acs(
    geography = "zcta", 
    variables = "B01003_001", 
    year = year, 
    survey = "acs5", 
    geometry = FALSE,
  ) %>%
    mutate(year=year)
  
  return(all_zctas)
}

# obtaining population estimates for 2016-2023 (most recent)
population <- map_df(2016:2023, pop_fun)

# filters for north carolina
nc_pop <- population %>%
  filter(str_starts(GEOID, "27") | str_starts(GEOID, "28"))

# aggregate into zip3
nc_pop_zip3 <- nc_pop %>%
  mutate(zip3 = substr(GEOID, 1, 3)) %>%
  group_by(zip3, year) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE), 
    n_zips = n(), 
    .groups = "drop"
  )

# pivot
nc_pop_zip3_wide <- nc_pop_zip3 %>%
  pivot_wider(
    names_from = year, 
    values_from = c(population, n_zips), 
    names_sep = "_"
  )


# spatial data ------------

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

# joining spatial data and population
nc_final <- nc_pop_zip3_wide %>%
  left_join(nc_zip3_geom, by = c("zip3" = "zip3")) %>%
  st_as_sf

  # # map as test -- visualizing zip3 area
  # ggplot(nc_final) +
  #   geom_sf(aes(geometry=geometry, fill=zip3)) +
  #   theme_minimal() +
  #   labs(title = "zip3 area")
  # 
  # # map as test -- visualizing population
  # ggplot(nc_final) +
  #   geom_sf(aes(geometry=geometry,fill=population_2023), color="white") +
  #   scale_fill_viridis_c(labels=scales::comma) +
  #   theme_minimal()+
  #   labs(title="2023 Population by Zip3 Level in North Carolina", 
  #        fill = "Population")

# flood data ------

# need to change out file path for where the 'Completed .tif files' folder is stored
filepath <- '.../.../.../OneDrive-SharedLibraries-TheGeorgeWashingtonUniversity/Hu, Cindy - REACH pilot/2-aims/aim3/2_raw_data/01_exposure_assessment/Satellite-based inundation map/Completed .tif files'

files <- list.files(filepath, pattern = "\\.tif$", full.names=TRUE)

print(files)

raster_stack <- rast(files)

# checking if CRS matches
st_crs(nc_final)
crs(raster_stack)
# nc_final is NAD 83, raster_stack is WGS 84

# change nc_final to WGS 84 and confirm
nc_final <- st_transform(nc_final, crs=4326)
st_crs(nc_final)

raster_stack # 12.5*12.5 = 156.5 km^2 (1/8 degree) resolution
st_area(nc_final)/(1000^2) # note that all areas > 156.5 km^2

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
nonNA_frac_list <- lapply(seq_len(nlyr(raster_stack)), function(i) {
  
  calc_nonNA_area.f(raster_stack[[i]], nc_final)
  
})

# combine results
nonNA_frac <- do.call(cbind, nonNA_frac_list)
colnames(nonNA_frac) <- paste0(names(raster_stack), "_nonNA_fraction")

  # # calculates the mean raster value in each zip3
  # areal_flooding.f <- lapply(raster_stack, function(r){
  #   
  #   terra::extract(not.na(r), nc_final, fun = mean)[,2]
  # 
  # })
  # count_rasters.f <- lapply(raster_stack, function(r){
  #   
  #   terra::extract(r, nc_final, fun = \(x) length(na.omit(x)))[,2]
  #   
  # })

  # # combines into data frames
  # mean_df <- data.frame(zip3 = nc_final$zip3, do.call(cbind, mean_list))
  # count_df <- data.frame(zip3 = nc_final$zip3, do.call(cbind, count_rasters.f))

  # # renames column name with original raster name
  # colnames(mean_df)[-1] <- names(raster_stack)

# bind results
final_df <- cbind(nc_final, nonNA_frac)

# creating new column with max level flood inundation 
final_df <- final_df %>%
  rowwise() %>%
  mutate(mean_flood_value = mean(
    c_across(Flood_NC_2024092400_nonNA_fraction:Flood_NC_2024092821_nonNA_fraction), 
    na.rm=TRUE),
    median_flood_value = median(
      c_across(Flood_NC_2024092400_nonNA_fraction:Flood_NC_2024092821_nonNA_fraction), 
      na.rm=TRUE),
    max_flood_value = max(
      c_across(Flood_NC_2024092400_nonNA_fraction:Flood_NC_2024092821_nonNA_fraction), 
      na.rm=TRUE)) %>%
  ungroup()

final_df <- final_df %>%
  mutate(inundation_exposure = mean_flood_value > 0.5)

inundation_metrics <- final_df %>%
  dplyr::select(zip3, mean_flood_value, median_flood_value, max_flood_value) %>%
  pivot_longer(cols = mean_flood_value:max_flood_value, 
               values_to = "perc_inundated", names_to = "metric") %>%
  mutate(metric_name = case_when(grepl("mean", metric) ~ "Mean % inundated",
                                 grepl("median", metric) ~ "Median % inundated",
                                 grepl("max", metric) ~ "Max % inundated"))

# hist of inundation metrics for comparison
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
  ggsave("figures/inundation_metric_comparison.png", dpi = 600, width = 7, height = 4)
}

# map of mean % area inundated
ggplot(final_df) +
  geom_sf(aes(fill = mean_flood_value), color="white") +
  scale_fill_gradient(low = "white", high = "darkblue", labels = scales::percent,
                      breaks = seq(0.1, 1, by = 0.3), limits = c(0.1, 1),
                      name = "Mean % area inundated") +
  theme_void() +
  labs(title = "Mean % area inundated by zip3", 
       subtitle = "Calculations based on 3-hour increments (9/24/24, 12:00am - 9/28/24, 9:00pm)") +
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

if (FALSE) {
  ggsave("figures/mean_percent_inundated_map.png", dpi = 600, width = 7, height = 5)
}


#############################################################################
# making a gif of inundation time series over the zip3 units

# pivot to long form for gif and pull out time points from column titles
gif_long <- final_df %>%
  select(zip3, geometry, starts_with("Flood_")) %>%
  pivot_longer(
    cols = starts_with("Flood_"), 
    names_to = "time_point", 
    values_to = "pct_flooded"
  ) %>%
  mutate(
    time_point = str_remove(time_point, "^Flood_NC_"), 
    time_point = str_remove(time_point, "_nonNA_fraction$"), 
    datetime = ymd_h(time_point), 
    time_label = format(datetime, "%b %d, %Y %I%p")
  ) %>%
  arrange(datetime) %>%
  mutate(time_order = factor(time_label, levels = unique(time_label)))


# order by time
gif_long <- gif_long %>%
  arrange(datetime) %>% 
  mutate(time_label = factor(time_label, levels = unique(time_label)))

# making gif with 5 colors (continuous scale)
map_anim <- ggplot(gif_long) +
  geom_sf(aes(fill = pct_flooded, group=time_order), color = "gray", size = 0.1) +
  scale_fill_gradientn(
    colors = c("white", "steelblue1","steelblue2", "steelblue3", "steelblue4"), 
    name = "% Flooded", 
    breaks = seq(0, max(gif_long$pct_flooded, na.rm=TRUE), length.out=5), 
    labels = scales::percent_format(scale=1)
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.title = element_text(size = 18, face = "bold"), 
    legend.text = element_text(size = 16), 
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width = unit(0.8, "cm"), 
    plot.title = element_text(size = 24, face = "bold"), 
    plot.subtitle = element_text(size = 20), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    panel.grid = element_blank()
  ) +
  labs(
    title = "Hurricane Helene Flood Inundation by Zip3 Area in North Carolina", 
    subtitle = "{closest_state}"
  ) +
  transition_states(
    time_order, 
    transition_length = 2, 
    state_length = 3
  )+
  ease_aes('linear')

# save in figures folder
animate(map_anim, nframes = 150, fps = 10, width = 1000, height = 800, 
        renderer = gifski_renderer("figures/flooding_over_time.gif"))

################################################################################
# carrying 2019-2023 population estimates forward for 2024 estimates
final_df <- final_df %>%
  mutate(population_2024 = population_2023)

final_df_long <- final_df %>%
  dplyr::select(zip3, starts_with("population_"),
                mean_flood_value, median_flood_value, max_flood_value, inundation_exposure)

final_df_long <- pivot_longer(final_df_long, 
                              cols = c(population_2016:population_2024),
                              names_to = "acs_var",
                              values_to = "total_population")

final_df_long <- final_df_long %>%
  mutate(year = parse_number(acs_var)) %>%
  dplyr::select(-acs_var) %>%
  st_drop_geometry()

if (FALSE) {
  write.csv(final_df_long, "data/processed_data/zip3_exposure_dataset.csv")
}
