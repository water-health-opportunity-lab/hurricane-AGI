# Primary Authors: Katie O'Brien, Jahred Liddie
# Purpose: This script pulls environmental data from prism and aggregates to 
# zip3 spatial scale and weekly temporal scale. 
# Date created: January 16, 2026
###############################################################################
# set up 
library(prism)
library(terra)
library(lubridate)
library(sf)
library(tidyverse)
library(purrr)
library(stringr)
library(tibble)
library(dplyr)

source("script/1_data_wrangling/1_zip3_wrangling.R")


###############################################################################

# set temporary directory
# note: prism_temp_full_0128 will be a folder on your personal computer
full_dir_temp <- ".../.../.../.../prism_temp_full_0128"
prism_set_dl_dir(full_dir_temp)
dir.create(full_dir_temp, showWarnings=FALSE, recursive=TRUE)

# set directory for completed data
# note: prism_nc_full_0128 will be a folder on your personal computer
full_nc_dir <- ".../.../.../.../prism_nc_full_0128"
dir.create(full_nc_dir, showWarnings=FALSE, recursive=TRUE)

# define study period as January 1, 2016 to January 2, 2025 - full 470 weeks
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2025-01-02")

# calculating weeks - not used in function but helpful to have
week_starts_test <- seq(start_date, end_date, by = "7 days")
week_ends_test <- week_starts_test + 6 
weeks_test <- tibble(
  week_start = week_starts_test, 
  week_ends = week_ends_test
) %>%
  arrange(week_starts_test)


# define NC boundary to be used
nc_boundary <- tigris::states(cb = TRUE, year = 2020) %>%
  filter(STUSPS == "NC") %>%
  st_transform(crs = 4269)

# for tracking progress on data processing
start_time <- Sys.time()
total_weeks_processed <- 0 

# defining variables to pull; pulling dewpoint separately because not available for all weeks
variables <- c("tmin", "tmax", "tmean", "ppt", "tdmean")

# defining weeks as sequence of 7 days after the start date (Jan 1, 2016)
week_starts <- seq(start_date, end_date, by = 7)

# checking that the appropriate number of weeks will be processed - should be 470 weeks
cat("Will process", length(week_starts), "weeks\n\n")

# processing function
process_week <- function(week_start, week_end, variable,
                         nc_boundary, nc_output_dir, temp_dir) {
  
  # descriptive writing to keep track of downloading status
  cat("  Processing week:", format(week_start), "to", format(week_end), "\n")
  
  # clean prism archive 
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)
  
  # loop over each day, gathers prism data for each day 
  dates <- seq(week_start, week_end, by = "day")
  date_strings <- format(dates, "%Y%m%d")
  
  lapply(dates, function(d) {
    get_prism_dailys(
      type = variable,
      minDate = format(d, "%Y-%m-%d"),
      maxDate = format(d, "%Y-%m-%d"),
      keepZip = FALSE, 
      resolution = "800m"
    )
  })
  
  # identify the files for the weekly aggregation
  prism_files <- prism_archive_ls()
  
  if (length(prism_files) == 0) {
    cat("    No PRISM files found in archive after download\n")
    return(NULL)
  }
  
  # filter by date strings
  prism_files <- prism_files[
    Reduce(`|`, lapply(date_strings, grepl, prism_files))
  ]
  
  if (length(prism_files) == 0) {
    cat("    No files matched the expected dates\n")
    return(NULL)
  }
  
  cat("    Found", length(prism_files), "daily files\n")
  
  # crop to NC boundary
  daily_rasters_nc <- list()
  
  for (pf in prism_files) {
    file_path <- pd_to_file(pf)
    
    if (!file.exists(file_path)) next
    
    r <- rast(file_path)
    
    # clip to NC
    r_cropped <- crop(r, vect(nc_boundary))
    r_clipped <- mask(r_cropped, vect(nc_boundary))
    
    daily_rasters_nc[[length(daily_rasters_nc) + 1]] <- r_clipped
    
    # remove full US file
    unlink(dirname(file_path), recursive = TRUE)
  }
  
  if (length(daily_rasters_nc) == 0) {
    cat("    No valid rasters after clipping\n")
    return(NULL)
  }
  
  # aggregate to weekly values
  weekly_stack <- rast(daily_rasters_nc)
  
  if (variable == "ppt") {
    # calculating sum for precipitation
    weekly_sum  <- app(weekly_stack, sum, na.rm = TRUE)
    # calculating mean for precipitation
    weekly_mean <- app(weekly_stack, mean, na.rm = TRUE)
    
    # naming final file for precipitation sum
    out_sum  <- file.path(nc_output_dir,
                          paste0(variable, "_", format(week_start, "%Y%m%d"), "_weekly_sum.tif"))
    # naming final file for precipitation mean 
    out_mean <- file.path(nc_output_dir,
                          paste0(variable, "_", format(week_start, "%Y%m%d"), "_weekly_mean.tif"))
    
    # writing final raster file for precipitation sum
    writeRaster(weekly_sum,  out_sum,  overwrite = TRUE)
    # writing final raster file for precipitation mean
    writeRaster(weekly_mean, out_mean, overwrite = TRUE)
    
    # descriptive writing to indicate it was saved
    cat("    Saved:", basename(out_sum), "\n")
    cat("    Saved:", basename(out_mean), "\n")
    
    return(list(sum = out_sum, mean = out_mean))
    
  } else {
    # for all variables other than precipitation, calculate the weekly mean
    weekly_mean <- app(weekly_stack, mean, na.rm = TRUE)
    
    # naming final file
    out_file <- file.path(nc_output_dir,
                          paste0(variable, "_", format(week_start, "%Y%m%d"), "_weekly_mean.tif"))
    
    # writing final raster file
    writeRaster(weekly_mean, out_file, overwrite = TRUE)
    
    # descriptive writing to indicate it was saved
    cat("    Saved:", basename(out_file), "\n")
    
    return(out_file)
  }
}

# processing code
for (var in variables) {
  
  # descriptive writing to keep track of status
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("Processing variable:", toupper(var), "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  var_weeks <- 0
  
  for (i in seq_along(week_starts)) {
    
    # defines week as week_start + 6 days
    week_start <- week_starts[i]
    week_end   <- min(week_start + days(6), end_date)
    
    if (week_start > end_date) break 
    
    # descriptive writing to keep track of status
    cat("Week", as.character(i), "of", as.character(length(week_starts)),
        "-", as.character(var), "\n")
    
    # logic to skip file if it already exists - helpful if data download gets interrupted
    if (var == "ppt") {
      expected_sum  <- file.path(full_nc_dir,
                                 paste0("ppt_", format(week_start, "%Y%m%d"), "_weekly_sum.tif"))
      expected_mean <- file.path(full_nc_dir,
                                 paste0("ppt_", format(week_start, "%Y%m%d"), "_weekly_mean.tif"))
      
      if (file.exists(expected_sum) && file.exists(expected_mean)) {
        cat("    Already processed — skipping\n")
        next
      }
      
    } else {
      expected_file <- file.path(full_nc_dir,
                                 paste0(var, "_", format(week_start, "%Y%m%d"), "_weekly_mean.tif"))
      
      if (file.exists(expected_file)) {
        cat("    Already processed — skipping\n")
        next
      }
    }
    
    # processing
    result <- process_week(
      week_start,
      week_end,
      var,
      nc_boundary,
      full_nc_dir,      
      prism_get_dl_dir()
    )
    
    if (!is.null(result)) {
      var_weeks <- var_weeks + 1
      total_weeks_processed <- total_weeks_processed + 1
    }
    
    # lag to not overwhelm server
    Sys.sleep(1)
  }
  
  cat("\nCompleted", as.character(var), "- Total weeks:", as.character(var_weeks), "\n")
  
  # calculates how long the download took
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat("Elapsed time:", as.character(round(elapsed, 1)), "minutes\n")
}


# download time: 1749.9 minutes

# total number of weeks per variable
cat("\nFiles by variable:\n")
for (var in c(variables)) {
  var_count <- length(list.files(full_nc_dir, pattern = var, recursive = TRUE))
  if (var_count > 0) {
    cat("  ", var, ":", var_count, "files\n")
  }
}

# all show correct number of files

######################### validation check ###################################

# manually downloading data from the prism website to compare with processed data
# https://prism.oregonstate.edu/

# validation 1

# validating precipitation data for May 10-16, 2019 - mean

# data processed in R
mean_precip <- rast(".../.../.../.../prism_nc_full_0128/ppt_20190510_weekly_mean.tif")

# summary stats for processed code
vals <- values(mean_precip, na.rm = TRUE)
summary_table <- data.frame(
  mean = mean(vals), 
  min = min(vals), 
  max = max(vals),
  sd = sd(vals),
  median = median(vals)
)

summary_table
# mean: 4.041678 min: 0.04057143 max: 17.71371 sd: 3.451237 median: 2.884929

# data manually downloaded
# pulling in folder of downloaded files and stacking them
# note: precip_check will be a folder on your personal computer storing the 
# downloaded data
check <- ".../.../.../.../precip_check"
files <- list.files(check, pattern = "\\.tif$", full.names=TRUE)
check_stack <- rast(files)

# clip to NC boundary
check_cropped <- crop(check_stack, vect(nc_boundary))
check_clipped <- mask(check_cropped, vect(nc_boundary))

# calculate the mean across the raster stack
check_mean <- mean(check_clipped, na.rm=TRUE)

# summary statistics 
check_vals <- values(check_mean, na.rm = TRUE)
check_summary_table <- data.frame(
  mean = mean(check_vals), 
  min = min(check_vals), 
  max = max(check_vals),
  sd = sd(check_vals),
  median = median(check_vals)
)

check_summary_table
# mean: 4.041678 min: 0.04057143 max: 17.71372 sd: 3.451237 median: 2.884929

# direct comparison
# mean: 4.041678 min: 0.04057143 max: 17.71372 sd: 3.451237 median: 2.884929
# mean: 4.041678 min: 0.04057143 max: 17.71371 sd: 3.451237 median: 2.884929
# all match


# validation 2

# validating precipitation data for May 10-16, 2019 - sum

# data processed in R
sum_precip <- rast("C:/Users/kobri/Downloads/prism_nc_full_0128/ppt_20190510_weekly_sum.tif")

# summary stats for processed code
vals <- values(sum_precip, na.rm = TRUE)
summary_table <- data.frame(
  mean = mean(vals), 
  min = min(vals), 
  max = max(vals),
  sd = sd(vals),
  median = median(vals)
)

summary_table
# mean: 28.29175 min: 0.284 max: 123.996 sd: 24.15866 median: 20.1945

# data manually downloaded
# pulling in folder of downloaded files and stacking them
# note: precip_check will be a folder on your personal computer storing the 
# downloaded data
check <- ".../.../.../.../precip_check"
files <- list.files(check, pattern = "\\.tif$", full.names=TRUE)
check_stack <- rast(files)

# clip to NC boundary
check_cropped <- crop(check_stack, vect(nc_boundary))
check_clipped <- mask(check_cropped, vect(nc_boundary))

# calculate the mean across the raster stack
check_sum <- sum(check_clipped, na.rm=TRUE)

# summary statistics 
check_vals <- values(check_sum, na.rm = TRUE)
check_summary_table <- data.frame(
  mean = mean(check_vals), 
  min = min(check_vals), 
  max = max(check_vals),
  sd = sd(check_vals),
  median = median(check_vals)
)

check_summary_table
# mean: 28.29175 min: 0.284 max: 123.996 sd: 24.15866 median: 20.1945

# direct comparison: 
# mean: 28.29175 min: 0.284 max: 123.996 sd: 24.15866 median: 20.1945
# mean: 28.29175 min: 0.284 max: 123.996 sd: 24.15866 median: 20.1945

# all correct 

####################### aggregating into zip3 ################################

# creating new dataframe with just zip3 and geometry
nc_prism <- data.frame(nc_final$zip3, nc_final$geometry)
nc_prism <- nc_prism %>% rename(zip3 = nc_final.zip3)

# keep spatial geometry
if(!inherits(nc_prism, "sf")) {
  nc_prism <- st_as_sf(nc_prism)
}

# transform to correct crs
nc_prism <- nc_prism %>%
  st_transform(crs = 4269)

# check crs 
crs(nc_prism) #NAD 83

# pull in files and check crs
processed_files <- list.files(full_nc_dir, pattern = "\\.tif$", full.names = TRUE)
crs_info <- tibble(
  file = processed_files,
  crs  = map_chr(processed_files, ~ crs(rast(.x)))
)
crs_info # All NAD83

# transform into vector
zip3_vect <- vect(nc_prism)

# function to extract the area-weighted average
extract_means <- function(raster_file, zip3_vect, zip3 = "zip3") {
  r <- terra::rast(raster_file)
  
  # extracts area weighted average for each zip3 polygon
  vals <- terra::extract(r, zip3_vect, fun = mean, na.rm = TRUE, weights = TRUE)
  
  # attach zip3 id 
  vals[[zip3]] <- zip3_vect[[zip3]]
  
  # adds variable and week info from the file name
  fname <- basename(raster_file)
  
  variable <- sub("_.*", "", fname)
  
  week_start <- sub(".*_(\\d{8}).*", "\\1", fname)
  
  vals$variable <- variable
  vals$week_start <- week_start
  
  vals
}

# loops over all weekly rasters
weekly_files <- list.files(full_nc_dir, pattern = "\\.tif$", full.names=TRUE)

zip3_envdat <- map_dfr(
  weekly_files, 
  extract_means, 
  zip3_vect = zip3_vect, 
  zip3 = "zip3"
)


# note: takes a few hours to process

# reshaping
zip3_envdat_unique <- zip3_envdat |>
  group_by(zip3, week_start, variable) |>
  summarise(
    mean = mean(mean, na.rm = TRUE),
    sum  = mean(sum,  na.rm = TRUE),
    .groups = "drop"
  )

zip3_envdat_wide <- zip3_envdat_unique |> 
  pivot_wider(
    id_cols = c(zip3, week_start), 
    names_from = variable, 
    values_from = c(mean, sum), 
    names_sep = "_"
  ) |>
  transmute(
    zip3, 
    week_start, 
    tmean = mean_tmean, 
    tmin = mean_tmin, 
    tmax = mean_tmax, 
    ppt_mean = mean_ppt, 
    ppt_sum = sum_ppt, 
    tdmean = mean_tdmean
  )


# calculate humidity using the Magnus Equation
zip3_envdat_wide <- zip3_envdat_wide |>
  mutate(
    humidity = 100 *
      (exp((17.625 * tdmean) / (tdmean + 243.4))) /
      (exp((17.625 * tmean) / (tmean + 243.4)))
  )


# save final dataset
prism_nc_final <- zip3_envdat_wide

if (FALSE) {
  write.csv(prism_nc_final, ".../.../.../.../.../2-aims/aim3/3_processed_data/env_data.csv")
}


