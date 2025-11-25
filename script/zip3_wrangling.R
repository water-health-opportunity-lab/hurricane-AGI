# Author: Katie O'Brien
# Date: November 14, 2025
# Description: This script imports the zip code data for the U.S., subsets it into North Carolina, and then aggregates into zip 3 level. It then imports the flood inundation data and calculates the mean value in each zip3 level. 

# set up ------------
library(sf)
library(dplyr)
library(ggplot2)
library(tidycensus)
library(tidyverse)
library(terra)
library(tidyr)
library(tigris)



# population data -----------

# make sure census api key is installed

# all population data
pop_fun <- function(year) {
  all_zctas <- get_acs(
    geography = "zcta", 
    variables = "B01003_001", 
    year = year, 
    survey = "acs5", 
    geometry = FALSE
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
    population = sum(estimate, na.rm=TRUE), 
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
zcta_geometry <- zctas(year = 2020, cb=TRUE)

# filter for NC
nc_zcta <- zcta_geometry %>%
  filter(str_starts(GEOID20, "27") | str_starts(GEOID20, "28"))

# aggregate into zip3 
nc_zip3_geom <- nc_zcta %>%
  mutate(zip3 = substr(GEOID20, 1, 3)) %>%
  group_by(zip3) %>%
  summarise(
    geometry=st_union(geometry)
  )


# joining spatial data and population
nc_final <- nc_pop_zip3_wide %>%
  left_join(nc_zip3_geom, by = c("zip3" = "zip3")) %>%
  st_as_sf



# map as test -- visualizing zip3 area
ggplot(nc_final) +
  geom_sf(aes(geometry=geometry, fill=zip3)) +
  theme_minimal() +
  labs(title = "zip3 area")

# map as test -- visualizing population
ggplot(nc_final) +
  geom_sf(aes(geometry=geometry,fill=population_2023), color="white") +
  scale_fill_viridis_c(labels=scales::comma) +
  theme_minimal()+
  labs(title="2023 Population by Zip3 Level in North Carolina", 
       fill = "Population")



# flood data ------

# need to chanage out file path for where the 'Completed .tif files' folder is stored
filepath <- "/Users/katieobrien/Library/CloudStorage/OneDrive-SharedLibraries-TheGeorgeWashingtonUniversity/Hu, Cindy - Satellite-based inundation map/Completed .tif files"

files <- list.files(filepath, , pattern = "\\.tif$", full.names=TRUE)
print(files)
raster_stack <- rast(files)

# checking if CRS matches
st_crs(nc_final)
crs(raster_stack)
# nc_final is NAD 83, raster_stack is WGS 84

# change nc_final to WGS 84 and confirm
nc_final <- st_transform(nc_final, crs=4326)
st_crs(nc_final)

# calculates the mean raster value in each zip3
mean_list <- lapply(raster_stack, function(r){
  terra::extract(r, nc_final, fun=mean, na.rm=TRUE)[,2]
})

# combines into data frames
mean_df <- data.frame(zip3 = nc_final$zip3, do.call(cbind, mean_list))

# renames column name with original raster name
colnames(mean_df)[-1] <- names(raster_stack)

# dataframe combining flood and population data
final_df <- full_join(nc_final, mean_df, by="zip3")

# creating new column with max level flood inundation 
final_df <- final_df %>%
  rowwise() %>%
  mutate(max_flood_value = max(c_across(19:58), na.rm=TRUE)) %>%
  ungroup()

# testing with map - last time point
ggplot(final_df) +
  geom_sf(aes(fill=Flood_NC_2024092718), color="white") +
  scale_fill_gradient(low = "lightblue", high="darkblue", labels=scales::comma) +
  theme_minimal()+
  labs(title="Flood Inundation by Zip3 on Sept 27, 2024 at 18:00", 
       fill = "Flood Inundation")

# testing with map - max flood inundation
ggplot(final_df) +
  geom_sf(aes(fill=max_flood_value), color="white") +
  scale_fill_gradient(low = "lightblue", high="darkblue", labels=scales::comma) +
  theme_minimal()+
  labs(title="Max Flood Inundation by Zip3", 
       fill = "Flood Inundation")


# FEMA floodplain data --------------

# change file path
fema <- st_read("/Users/katieobrien/Library/CloudStorage/OneDrive-SharedLibraries-TheGeorgeWashingtonUniversity/Hu, Cindy - FEMA Floodplain/North_Carolina_Flood_Hazard_Area_Effective.shp")

# need to join to the zip3 data using spatial overlap because only county FIPS are available in the fema data

# confirm CRS
st_crs(final_df)
# in WGS 1984; ESPG:4326

# need to transform fema data into ESPF:4326
fema_t <- st_transform(fema, st_crs(final_df))


# several geometries in fema_t are invalid (same as in fema)
# investigate invalid rows
invalid_rows <- which(!st_is_valid(fema_t))
fema_t[invalid_rows, ]
st_is_valid(fema_t[invalid_rows, ], reason=TRUE)
# make valid
fema_t[invalid_rows, ] <- st_make_valid(fema_t[invalid_rows, ])
# confirm it worked (length should equal 0)
length(which(!st_is_valid(fema_t)))


# all data combined
fema_zip3 <- st_join(
  final_df, 
  fema_t, 
  join=st_intersects
)

# calculates mean value of SFHA_TF in each zip3 level
# SFHA_TF = special flood hazard area (1=yes, 0=no)
fema_agg <- st_join(final_df, fema_t, join=st_intersects) %>%
  group_by(zip3) %>%
  summarize(
    count=n(), 
    mean_value = mean(SFHA_TF, na.rm=TRUE)
  )

# check using map
ggplot(fema_agg) +
  geom_sf(aes(fill = mean_value), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Mean Value by Zip3", 
       fill = "Mean Value")






