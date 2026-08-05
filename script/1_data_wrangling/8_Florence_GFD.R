# Primary Authors: Katie O'Brien, Jahred Liddie
# Purpose: This script uses Global Flood Database data to identify exposure to 
# Hurricane Florence in North Carolina. 
# Date created: August 3, 2026

###############################################################################

# set up 
library(sf)
library(terra)
library(exactextractr)
library(patchwork)
library(ggplot2)
library(ggspatial)

###############################################################################


# import data
flood_dat <- st_read(".../.../.../.../2-aims/aim3-AGI/2_raw_data/01_exposure_assessment/Global Flood Database/Global_Flood_Records.gpkg")

# testing import, plotting all events
plot(flood_dat$geom)

# testing import, plotting first event
plot(flood_dat$geom[1])

# pull out hurricane florence, identified online (report number 4676)
flor_dat <- subset(flood_dat, subset = flood_dat$ReportNumber == "4676")

# changing into a vector
flor_vect <- vect(flor_dat)
# plotting as test
plot(flor_vect)

# initializing empty raster
empty_raster <- rast(ext(flor_vect), nrows = 500, ncols = 500, crs = crs(flor_vect))
flor_vect$val <- 1

# changing polygon into raster
flor_raster <- rasterize(flor_vect, empty_raster, field = "val", background = NA)
# plotting as test
plot(flor_raster)


# plotting zip3 units
plot(final_df$geometry)
# checking coordinate reference systems of the zip3 polygons and florence raster
crs(final_df)
crs(flor_raster)
# both are WGS 84 

# plotting raster on top of zip3 polygons as test
plot(flor_raster, add = TRUE)

# units are longitude and latitude, need to change to a projected coordinate system 

# change to projected coordinate system (EPSG 32119, North Carolina localized)
flor_rast_proj <- project(flor_raster, "EPSG:32119", method = "near") # resolution is 0.78 km by 0.78 km

# creating new dataframe without Hurricane Helene flooding
flor_zip3_df <- final_df[, c("zip3", "geometry", "population_2016", "population_2017", "population_2018", 
                             "population_2019", "population_2020", "population_2021", "population_2022", 
                             "population_2023")]
# projecting into ESPG 32119
flor_zip3_df_proj <- st_transform(flor_zip3_df, crs = crs(flor_rast_proj))

# checking resolution for calculation
res(flor_rast_proj)
# resolution is 788.1645 x 788.1645

# calculating flooded area
flor_zip3_df_proj$flooded_area <- exact_extract(
  flor_rast_proj, 
  flor_zip3_df_proj, 
  fun = function(values, coverage_fraction){
    pixel_area <- 788.1645*788.1648 # calculates area of one full pixel based on resolution
    sum(values*coverage_fraction*pixel_area, na.rm = TRUE) # units are square meters
  }
)

# plotting as test
plot(flor_zip3_df_proj["flooded_area"])

# adding new column for flooded area in kilometers
flor_zip3_df_proj$flooded_area_km <- flor_zip3_df_proj$flooded_area / 1000

# side by side comparison of raster on top of zip3 vs calculated flooding
p1 <- ggplot()+
  layer_spatial(flor_raster)+
  geom_sf(data = final_df, fill = NA, color = "black")+
  scale_fill_viridis_c(guide="none", na.value = "transparent")+
  labs(title = "Flooding Raster")+
  theme_void()
p2 <- ggplot()+
  geom_sf(data = flor_zip3_df_proj, aes(fill = flooded_area_km))+
  scale_fill_gradient(low ="#B0E2ff", high = "#36648B", name = "Flooded Area (km)")+
  guides(fill = guide_colorbar(barwidth=unit(15, "lines")))+
  labs(title = "Calculated Flooding by Zip3")+
  theme_void()+
  theme(legend.position = "bottom", legend.title.position = "top")
p1+p2
par(mfrow = c(1,1))

# naming final dataframe
flor_final_df <- flor_zip3_df_proj

# write to csv
if (FALSE) {
  write.csv(flor_final_df, "data/processed_data/florence_exp.csv")
}

