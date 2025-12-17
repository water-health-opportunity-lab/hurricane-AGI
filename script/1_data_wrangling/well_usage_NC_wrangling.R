# Primary Authors: Katie O'Brien, Jahred Liddie
# Purpose: This script pulls data on private well usages in NC and aggregates into the zip3 level.
# Date created: December 17, 2025

###############################################################################

# set up 
library(terra)
library(sf)
library(dplyr)
library(tigris)

###############################################################################

# read in well data
# is the Well_Estimates_2020_Blocks.csv (https://github.com/USEPA/ORD_Water_Source_2020/blob/main/outputs/Well_Estimates_2020_Blocks.csv)
welldata <- read.table("/Users/katieobrien/Downloads/Well_Estimates_2020_Blocks.txt", header=TRUE, sep=",")

# filtering for NC
nc_data <- welldata %>%
  filter(substr(GEOID_BlockGroup, 1, 2) == "37") %>%
  mutate(GEOID_BlockGroup = as.character(GEOID_BlockGroup))

# need to download the geometries for census block group first
nc_bg_spatial <- block_groups(state = "37", year = 2020)

nc_bg <- nc_bg_spatial %>%
  inner_join(nc_data, by = c("GEOID" = "GEOID_BlockGroup"))

# shrink to centroids
nc_bg_points <- st_centroid(nc_bg)

# make same CRS as nc_final
nc_bg_points <- st_transform(nc_bg_points, st_crs(nc_final))

# joining with nc_final
bg_with_zip3 <- st_join(nc_bg_points, nc_final, join=st_within)

# aggregating into zip3
zip3_wells <- bg_with_zip3 %>%
  st_drop_geometry() %>% 
  mutate(
    Pct_Wells = as.numeric(Pct_Wells), 
    Population = as.numeric(Population)
  ) %>%
  group_by(zip3) %>%
  summarise(
    weighted_percent=sum(Pct_Wells*Population, na.rm=TRUE) / sum(Population, na.rm=TRUE)
  )

# examining NA values
sum(is.na(bg_with_zip3$zip3))
# 1410 points did not fall into a zip3 polygon

missing_points <- bg_with_zip3 %>%
  filter(is.na(zip3))

plot(st_geometry(nc_final), col = "lightgray", border = "white")
plot(st_geometry(missing_points), col= "red", pch=15, add=TRUE)
title("Block Group Points Not Matching Any Zip3")

# appears to be a boundary issue 
# assigning points to the nearest zip3
bg_with_zip3_fixed <- nc_bg_points %>%
  mutate(
    zip3 = nc_final$zip3[st_nearest_feature(nc_bg_points, nc_final)]
    )

# aggregating
zip3_wells <- nc_final %>%
  left_join(
    bg_with_zip3_fixed %>%
      st_drop_geometry() %>%
      mutate(
        Pct_Wells = as.numeric(Pct_Wells), 
        Population = as.numeric(Population)
      ) %>%
      group_by(zip3) %>%
      summarize(
        weighted_percent_wells = sum(Pct_Wells*Population, na.rm = TRUE) / sum(Population, na.rm = TRUE)
      ), 
    by = "zip3"
  )

# visualizing as test
ggplot(zip3_wells) +
  geom_sf(aes(fill = weighted_percent_wells), color = "white", size = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                       name = "% Private Well Users", 
                       labels = scales::percent_format(scale=1)) +
  theme_minimal() +
  labs(title="Percent of Private Well Users by Zip3")

# combining with final_df_long from zip3_wrangling script
final_df_long_well <- final_df_long %>%
  left_join(
    zip3_wells %>% 
      st_drop_geometry() %>% 
      select(zip3, weighted_percent_wells), 
    by="zip3"
    )

# write.csv(final_df_long_well, "data/processed_data/zip3_exposure_dataset.csv")

