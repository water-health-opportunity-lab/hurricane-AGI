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

# zip 3 polygon data --------

gdb_path <- ("data/USA_Zip_Code_Boundaries/v108/zip_poly.gdb")
st_layers(gdb_path)

spatial_data <- st_read(gdb_path) # imports data into spatial_data vector
head(spatial_data) # view data preview

# subsetting into North Carolina
nc <- subset(spatial_data, STATE == "NC")

# creating new column with zip3 level
nc <- nc %>% 
  mutate(zip3 = substr(ZIP_CODE, 1, 3))

head(nc[, c("ZIP_CODE", "zip3")]) # previewing data

# aggregate into zip3 levels
zip3_data <- nc %>% 
  group_by(zip3) %>%
  summarise(
    Shape=st_union(Shape), 
    n_zipcodes = n()
  )

# remove Hoffman Forest (0 population), zip3 000, row 1
zip3_data <- zip3_data[-1,]

# population data -----------

# make sure census api key is installed

# all population data
pop_fun <- function(year) {
  all_zctas <- get_acs(
    geography = "zcta", 
    variables = "B01003_001", 
    year = year, 
    survey = "acs5"
  ) %>%
    mutate(year=year)
  
  return(all_zctas)
}

population <- map_df(2016:2023, pop_fun)

nc_pop <- population %>%
  filter(str_starts(GEOID, "27") | str_starts(GEOID, "28"))

nc_pop_zip3 <- nc_pop %>%
  mutate(zip3 = substr(GEOID, 1, 3)) %>%
  group_by(zip3, year) %>%
  summarize(
    population = sum(estimate, na.rm=TRUE), 
    n_zips = n(), 
    .groups = "drop"
  )

# wrangling dataset to only have one row per zip3 level
nc_pop_zip3_wide <- nc_pop_zip3 %>%
   pivot_wider(
     names_from = year, 
     values_from = c(population, n_zips), 
     names_sep = "_"
  )
# resulting dataset has one row per zip3 level and a column for population and number of zip codes per year


## join population and spatial data ----- 
nc_final <- zip3_data %>%
  left_join(nc_pop_zip3_wide, by = "zip3")

# map as test -- visualizing zip3 area
ggplot(nc_final) +
  geom_sf(aes(fill=zip3)) +
  theme_minimal() +
  labs(title = "zip3 area")

# map as test -- visualizing population
ggplot(nc_final) +
  geom_sf(aes(fill=population_2023), color="white") +
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
# both are WGS 84


# calculates the mean raster value in each zip3
mean_list <- lapply(raster_stack, function(r){
  terra::extract(r, nc_final, fun=mean, na.rm=TRUE)[,2]
})

# combines into data frames
mean_df <- data.frame(zip3 = nc_final$zip3, do.call(cbind, mean_list))

# renames column name with original raster name
colnames(mean_df)[-1] <- names(raster_stack)

# final dataframe combining flood and population data
final_df <- full_join(nc_final, mean_df, by="zip3")

# testing map
ggplot(final_df) +
  geom_sf(aes(fill=Flood_NC_2024092718), color="white") +
  scale_fill_gradient(low = "lightblue", high="darkblue", labels=scales::comma) +
  theme_minimal()+
  labs(title="Flood Inundation by Zip3 on Sept 27, 2024 at 18:00", 
       fill = "Flood Inundation")
