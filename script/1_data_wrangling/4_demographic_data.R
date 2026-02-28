# Primary Authors: Katie O'Brien, Jahred Liddit
# Purpose: This script pulls ACS data using tidycensus to gather demographics of NC.
# Date created: February 25, 2026

###############################################################################

# set up

library(tidycensus)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(units)
library(sf)


################################################################################

# pulled zip3 geometry aggregation from 1_zip3_wrangling.R file so we can re-attach 
# the zip3 geometry without needing to source the file

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

# calculate sqmi
nc_geom_sqmi <- nc_zip3_geom %>%
  mutate(area_raw = st_area(geometry), 
         area_sqmi = set_units(area_raw, "mi^2"))

# variables wanted: population, race/ethnicity, health insurance, rurality, 
# annual income, area in sqmi



#################### population and race/ethnicity #########################

# creates list of race/ethnicity variables from ACS
race_vars <- c(
  total_population = "B03002_001", 
  white = "B03002_003", 
  black = "B03002_004", 
  AI_AN = "B03002_005", 
  asian = "B03002_006", 
  hispanic = "B03002_012", 
  two_more = "B03002_009"
)

# pulls data for full US
us_full <- get_acs(geography = "zcta", 
                  variables = race_vars, 
                  survey = "acs5",
                  year = 2024, 
                  geometry = FALSE) 

# filters for NC
nc_race <- us_full %>%
  filter(str_starts(GEOID, "27") | str_starts(GEOID, "28"))

# aggregates to zip3 level
nc_race_zip3 <- nc_race %>%
  mutate(zip3 = substr(GEOID, 1, 3)) %>%
  group_by(zip3, variable) %>%
  summarize(
    estimate = sum(estimate, na.rm = TRUE), 
    moe = sqrt(sum(moe^2, na.rm = TRUE)), 
    .groups = "drop"
  )

# pivots to wide version
nc_race_zip3_wide <- nc_race_zip3 %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))


############################## health insurance ################################

# creates sequence to pull all variables (need to sum from the health insurance 
# by sex and age table)
# sequence is every three variables in the table
unins_vars <- paste0("B27001_", sprintf("%03d", c(seq(5,29,3), seq(33, 57, 3))))

# names the sequence variables, not descriptive since the individual ages/sexes 
#don't matter
all_vars <- c(total_pop = "B27001_001", setNames(unins_vars, paste0("uninsured_", 1:length(unins_vars))))

# pulls insurance data from ACS
unins_all <- get_acs(
  geography = "zcta", 
  variables = all_vars, 
  survey = "acs5",
  year = 2024, 
  geometry = FALSE
)

# filters for NC
nc_unins <- unins_all %>%
  filter(str_starts(GEOID, "27") | str_starts(GEOID, "28"))

# aggregates to zip3
nc_unins_zip3 <- nc_unins %>%
  mutate(zip3 = substr(GEOID, 1, 3)) %>%
  mutate(var_group = ifelse(variable == "total_pop", "total_ins", "uninsured")) %>%
  group_by(zip3, var_group) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), 
            moe = sqrt(sum(moe^2, na.rm = TRUE)), 
            .groups = "drop"
            )

# calculates rate of uninsurance
# did not end up averaging these uninsurance rates by exposure level for the 
# table, but leaving this here in case we want to do that instead of summing
# uninsured/total insurance for both exposures
nc_unins_rates <- nc_unins_zip3 %>%
  pivot_wider(names_from = var_group, values_from = c(estimate, moe)) %>%
  mutate(uninsured_rate = (estimate_uninsured / estimate_total_ins) *100)

######################### rurality ###########################################

# using number of rural households as a proxy

# getting number of rural households from 2020 census
us_rural <- get_decennial(
  geography = "zcta", 
  variables = c(total_hhs = "H2_001N", urban_hhs = "H2_002N", rural_hhs = "H2_003N", undefined_hhs = "H2_004N"), 
  year = 2020, 
  sumfile = "dhc"
)

# filtering for NC
nc_rural <- us_rural %>%
  filter(str_starts(GEOID, "27") | str_starts(GEOID, "28"))

# aggregating to zip3 level
nc_rural_zip3 <- nc_rural %>%
  mutate(zip3 = substr(GEOID, 1, 3)) %>%
  group_by(zip3, variable) %>%
  summarize(
    value = sum(value, na.rm = TRUE), 
    .groups = "drop"
  )

# pivoting to wide version
nc_rural_zip3_wide <- nc_rural_zip3 %>%
  pivot_wider(names_from = variable, values_from = value)



##################### median annual income ####################################

# pulls median household income for full US
us_income <- get_acs(
  geography = "zcta", 
  variables = c(med_income = "B19013_001", total_households = "B11001_001"), 
  year = 2024
)

# filters for NC
nc_income <- us_income %>%
  filter(str_starts(GEOID, "27") | str_starts(GEOID, "28"))

# pivots to wide version 
nc_income_wide <- nc_income %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

# aggreates to zip3, weighed average of median household income (weight is number
# of households)
nc_income_zip3 <- nc_income_wide %>%
  mutate(zip3 = substr(GEOID, 1, 3)) %>%
  group_by(zip3) %>%
  summarize(
    avg_med_income = weighted.mean(estimate_med_income, estimate_total_households, na.rm = TRUE), 
    total_households = sum(estimate_total_households, na.rm = TRUE)
  )

########################### combining dataframes ############################

# list of dataframes
demodata_list <- list(nc_geom_sqmi, nc_race_zip3_wide, nc_unins_rates, nc_income_zip3, nc_rural_zip3_wide)

# join by zip3 column
demodata <- demodata_list %>%
  reduce(left_join, by = "zip3")

# write to csv
if (FALSE) {
  write.csv(demodata, ".../.../.../.../.../2-aims/aim3/3_processed_data/demodata.csv")
}


