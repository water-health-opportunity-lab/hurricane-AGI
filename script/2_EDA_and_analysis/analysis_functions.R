################################################################################
# Purpose: functions for statistical analyses
################################################################################

# function to evaluate residual, hurricane week-specific global spatial autocorrelation
eval_SAC.f <- function(hurricane_week = NULL, model = NULL, model_dataset = NULL) {
  
  model_dataset$model_residuals <- model$residuals
  
  dat_hurricane <- model_dataset %>%
    filter(week_start == hurricane_week) %>% 
    mutate(zip3 = as.character(zip3))
  
  dat_hurricane <- left_join(nc_zip3_geom, dat_hurricane, by = "zip3")
  dat_hurricane <- st_as_sf(dat_hurricane, coords = geometry, crs = st_crs(nc_zip3_geom))
  
  nb <- poly2nb(dat_hurricane, queen = TRUE)
  
  lw <- nb2listw(nb, zero.policy = TRUE)
  
  morans <- moran.mc(dat_hurricane$model_residuals, lw, nsim = 999, zero.policy = TRUE,
                     alternative="greater")
  
  clean_morans <- broom::tidy(morans)
  
  clean_morans$model_name <- deparse(substitute(model))
  
  clean_morans$week_start <- hurricane_week
  
  return(clean_morans)
  
}

id_neighbors.f <- function(row_numbers = NULL, zip_dataset = dat_neighbors) {
  
  one_row <- zip_dataset[row_numbers,]
  
  one_id <- one_row$id
  
  one_row$neighbors <- list(nb[[row_numbers]])
  
  return(one_row)
  
}