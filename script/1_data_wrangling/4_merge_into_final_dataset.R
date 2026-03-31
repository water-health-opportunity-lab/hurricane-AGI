###############################################################################
# Primary Authors: Jahred Liddie, Katie O'Brien
# Purpose: Merge together outcome/case data, exposure data, and covariates
# Date created: 2/18/2026
###############################################################################

library(tidyverse)

outcomes <- read_csv("data/raw_data/raw_truveta_export_01272026.csv")

exposure <- read.csv("data/processed_data/zip3_exposure_dataset.csv")

covariates <- read.csv("data/processed_data/env_data.csv")

###############################################################################
# reformat outcome data into clean table
outcomes <- separate_wider_delim(outcomes, cols = 1, delim = ",",
                                 names = c("x", "weeks_since_anchor", "StateOrProvinceConceptId",
                                           "location_formatted", "full_location", "encounter_year",
                                           "n_events", "n_foodborne")) 

outcomes <- outcomes %>%
  dplyr::select(-x) %>%
  mutate(weeks_since_anchor = as.numeric(weeks_since_anchor),
         location_formatted = str_remove_all(string = location_formatted, pattern = "\""),
         n_events = as.numeric(n_events),
         n_foodborne = as.numeric(n_foodborne),
         days_since_anchor = weeks_since_anchor * 7,
         date = as.Date("2016/01/01") + days_since_anchor,
         week_start = paste(year(date), 
                            ifelse(str_length(month(date)) == 1, paste("0", month(date), sep = ""),  month(date)),
                            ifelse(str_length(day(date)) == 1, paste("0", day(date), sep = ""), day(date)), sep = ""),
         zip3 = as.factor(substr(location_formatted, 
                                        start = str_length(location_formatted) - 2, 
                                        stop = str_length(location_formatted)
                                 )
                          )
         )

covariates <- covariates %>%
  mutate(week_start = as.factor(week_start),
         zip3 = as.factor(zip3))

outcomes$week_start <- as.factor(outcomes$week_start)

outcomes <- outcomes %>%
  dplyr::select(-location_formatted, -StateOrProvinceConceptId, -full_location,
                -location_formatted)

# joining this way to allow all zip3 by week combinations to be represented
join_outcomes <- left_join(covariates, outcomes, by = c("week_start", "zip3"))

join_outcomes <- join_outcomes %>%
  group_by(zip3) %>%
  mutate(n_weeks = n(),
         dup_week = duplicated(week_start)) %>%
  ungroup()

  # check on weeks split across years
  check_weeks <- join_outcomes %>%
    filter(n_weeks > 470)
  
  # confirmed this is due to weeks that are split across years
  # view(check_weeks %>% filter(dup_week)) 

  weeks_over_years <- check_weeks %>%
    filter(dup_week) %>%
    pull(week_start) %>%
    unique()
  
  add_weeks <- join_outcomes %>%
    filter(week_start %in% weeks_over_years) %>%
    group_by(zip3, week_start) %>%
    mutate(sum_dup = sum(dup_week)) %>%
    filter(sum_dup == 0) %>%
    ungroup()

weeks_to_add <- tibble(zip3 = add_weeks$zip3,
                       week_start = add_weeks$week_start,
                       encounter_year = ifelse(is.na(add_weeks$encounter_year), as.numeric(substr(add_weeks$week_start, start = 1, stop = 4)) + 1, 
                                               ifelse(as.numeric(add_weeks$encounter_year) == as.numeric(substr(add_weeks$week_start, start = 1, stop = 4)) & !is.na(add_weeks$encounter_year), 
                                                      as.numeric(substr(add_weeks$week_start, start = 1, stop = 4)) + 1, as.numeric(substr(add_weeks$week_start, start = 1, stop = 4)))
                                                      )
                                               )

weeks_to_add <- left_join(weeks_to_add, covariates, by = c("zip3", "week_start"))
weeks_to_add <- left_join(weeks_to_add, 
                          outcomes %>% dplyr::select(-n_events, -n_foodborne, -encounter_year),
                          by = c("zip3", "week_start"))
  
join_outcomes <- plyr::rbind.fill(join_outcomes, weeks_to_add)

################################################################################
# final preprocessing steps: (1) add substitutions for missing / zero events, 
  # (2) drop 2016 data due to unclear backfilling in EHR, and 
  # and (3) drop in proximity to other hurricanes
join_outcomes <- join_outcomes %>%
    mutate(n_events = ifelse(is.na(n_events), 0, n_events),
           n_foodborne = ifelse(is.na(n_foodborne), 0, n_foodborne),
           encounter_year = as.factor(encounter_year),
           date = as.Date(join_outcomes$week_start, format = "%Y%m%d"),
           
           # note: these month and year indicators are based on the date that the week started
           month = month(date),
           year = year(date)
           )

join_outcomes <- join_outcomes %>%
  mutate(days_since_anchor = ifelse(is.na(days_since_anchor), 
                                    date - as.Date("2016-01-01"), days_since_anchor),
         weeks_since_anchor = ifelse(is.na(weeks_since_anchor), floor(days_since_anchor / 7), weeks_since_anchor))
         
join_outcomes <- join_outcomes %>%       
  filter(year(as.Date(join_outcomes$week_start, format = "%Y%m%d")) != 2016)

# removing 8-week period after other major hurricanes, including
  # Hurricane Florence: September 14, 2018
    # Hurricane Dorian: Sep. 6, 2019,
    # Hurricane Isaias: Aug. 3, 2020
  # source: https://products.climate.ncsu.edu/weather/hurricanes/nc-landfalls/

other_hurricanes <- tibble(
  hurricane = c("Florence", "Dorian", "Isaias"),
  start = as.Date(c("2018-09-14","2019-09-06", "2020-08-03")),
  end = start + 8*7
)

join_outcomes <- join_outcomes %>%
  filter( (date < other_hurricanes$start[other_hurricanes$hurricane == "Florence"]) |
           (date > other_hurricanes$end[other_hurricanes$hurricane == "Florence"])
        ) %>%
  filter(
          (date < other_hurricanes$start[other_hurricanes$hurricane == "Dorian"]) |
            (date > other_hurricanes$end[other_hurricanes$hurricane == "Dorian"])
        ) %>%
  filter(
    (date < other_hurricanes$start[other_hurricanes$hurricane == "Isaias"]) |
      (date > other_hurricanes$end[other_hurricanes$hurricane == "Isaias"])
  )

exposure$zip3 <- as.factor(exposure$zip3)

join_outcomes_exposure <- left_join(join_outcomes, 
                                    exposure %>% 
                                      dplyr::select(zip3:year),
                                    by = c("zip3", "year"))

join_outcomes_exposure <- join_outcomes_exposure %>%
  # 3-, 5-, and 8-week intervals after hurricane
  mutate(hurricane_3week = ifelse(date >= as.Date("2024-09-24") & date <= as.Date("2024-09-24") + 21, TRUE, FALSE),
         hurricane_5week = ifelse(date >= as.Date("2024-09-24") & date <= as.Date("2024-09-24") + 35, TRUE, FALSE),
         hurricane_8week = ifelse(date >= as.Date("2024-09-24") & date <= as.Date("2024-09-24") + 56, TRUE, FALSE),
         # based on whether the majority of the month is in that season
         season = case_when(month %in% c(1, 2, 3) ~ "Winter",
                            month %in% c(4, 5, 6) ~ "Spring",
                            month %in% c(7, 8, 9) ~ "Summer",
                            month %in% c(10, 11, 12) ~ "Fall"))

join_outcomes_exposure <- join_outcomes_exposure %>%
  dplyr::select(-X, -encounter_year)

if (FALSE) {
  write_csv(join_outcomes_exposure, "data/processed_data/analytic_dataset.csv")
}

###############################################################################
# repeat for data with masked units
outcomes_all <- read_csv("data/raw_data/all_raw_truveta_export_01272026.csv")

# reformat outcome data into clean table
outcomes_all <- separate_wider_delim(outcomes_all, cols = 1, delim = ",",
                                     names = c("x", "weeks_since_anchor", "StateOrProvinceConceptId",
                                               "location_formatted", "full_location", "encounter_year",
                                               "n_events", "n_foodborne")) 

outcomes_all <- outcomes_all %>%
  dplyr::select(-x) %>%
  mutate(weeks_since_anchor = as.numeric(weeks_since_anchor),
         location_formatted = str_remove_all(string = location_formatted, pattern = "\""),
         n_events = as.numeric(n_events),
         n_foodborne = as.numeric(n_foodborne),
         days_since_anchor = weeks_since_anchor * 7,
         date = as.Date("2016/01/01") + days_since_anchor,
         week_start = paste(year(date), 
                            ifelse(str_length(month(date)) == 1, paste("0", month(date), sep = ""),  month(date)),
                            ifelse(str_length(day(date)) == 1, paste("0", day(date), sep = ""), day(date)), sep = ""),
         zip3 = as.factor(substr(location_formatted, 
                                 start = str_length(location_formatted) - 2, 
                                 stop = str_length(location_formatted)
         )
         )
  )

outcomes_all$week_start <- as.factor(outcomes_all$week_start)

outcomes_all <- outcomes_all %>%
  # those that are filtered out are outside of NC
  filter(StateOrProvinceConceptId == "1067509") %>%
  dplyr::select(-location_formatted, -full_location,
                -location_formatted, -StateOrProvinceConceptId)

# tibble for disaggregating events
zip3_match <- tibble(
  zip_masked = c(rep("2**", 20), rep("27*", 10), rep("28*", 10), rep("_NA", 20), as.character(unique(join_outcomes_exposure$zip3))),
  zip3_unmasked = c(rep(as.character(unique(join_outcomes_exposure$zip3)), 4))
)

outcomes_all_disagg <- left_join(outcomes_all, zip3_match,
                                 by = c("zip3" = "zip_masked"))

outcomes_all_disagg <- left_join(outcomes_all_disagg, exposure %>% mutate(year = as.character(year)),
                                 by = c("zip3_unmasked" = "zip3", "encounter_year" = "year"))

outcomes_all_disagg <- outcomes_all_disagg %>%
  group_by(zip3, date, week_start, encounter_year, 
           weeks_since_anchor, days_since_anchor) %>%
  mutate(n_events_disagg = round((n_events * total_population) / sum(total_population), 0),
         n_foodborne_disagg = round((n_foodborne * total_population) / sum(total_population), 0)) %>%
  ungroup()

outcomes_all_reagg <- outcomes_all_disagg %>%
  group_by(zip3_unmasked, date, week_start, encounter_year, weeks_since_anchor, 
           days_since_anchor, mean_flood_value, median_flood_value, max_flood_value, 
           inundation_exposure, quartile_flood_value, tertile_flood_value,
           total_population) %>%
  summarise(n_events = sum(n_events_disagg),
            n_foodborne = sum(n_foodborne_disagg)) %>%
  ungroup()

# joining this way to allow all zip3 by week combinations to be represented
join_outcomes_all_reagg <- left_join(covariates, outcomes_all_reagg, 
                                     by = c("week_start", "zip3" = "zip3_unmasked"))

join_outcomes_all_reagg <- join_outcomes_all_reagg %>%
  group_by(zip3) %>%
  mutate(n_weeks = n(),
         dup_week = duplicated(week_start)) %>%
  ungroup()

  # check on weeks split across years
  check_weeks <- join_outcomes_all_reagg %>%
    filter(n_weeks > 470)
   
  weeks_over_years <- check_weeks %>%
    filter(dup_week) %>%
    pull(week_start) %>%
    unique()
  
  # none needed
  add_weeks <- join_outcomes_all_reagg %>%
    filter(week_start %in% weeks_over_years) %>%
    group_by(zip3, week_start) %>%
    mutate(sum_dup = sum(dup_week)) %>%
    filter(sum_dup == 0) %>%
    ungroup()

  # weeks_to_add <- left_join(weeks_to_add, covariates, by = c("zip3", "week_start"))
  # 
  # weeks_to_add <- left_join(weeks_to_add, 
  #                           outcomes %>% dplyr::select(-n_events, -n_foodborne, -encounter_year),
  #                           by = c("zip3", "week_start"))

# final preprocessing steps: (1) add substitutions for missing / zero events,
# (2) drop 2016 data due to unclear backfilling in EHR, and
# and (3) drop in proximity to other hurricanes
join_outcomes_all_reagg <- join_outcomes_all_reagg %>%
  mutate(encounter_year = as.factor(encounter_year),
         date = as.Date(join_outcomes_all_reagg$week_start, format = "%Y%m%d"),

         # note: these month and year indicators are based on the date that the week started
         month = month(date),
         year = year(date)
  )

join_outcomes_all_reagg <- join_outcomes_all_reagg %>%
  filter(year(as.Date(join_outcomes_all_reagg$week_start, format = "%Y%m%d")) != 2016)

join_outcomes_all_reagg <- join_outcomes_all_reagg %>%
  filter( (date < other_hurricanes$start[other_hurricanes$hurricane == "Florence"]) |
            (date > other_hurricanes$end[other_hurricanes$hurricane == "Florence"])
  ) %>%
  filter(
    (date < other_hurricanes$start[other_hurricanes$hurricane == "Dorian"]) |
      (date > other_hurricanes$end[other_hurricanes$hurricane == "Dorian"])
  ) %>%
  filter(
    (date < other_hurricanes$start[other_hurricanes$hurricane == "Isaias"]) |
      (date > other_hurricanes$end[other_hurricanes$hurricane == "Isaias"])
  )

join_outcomes_all_reagg <- join_outcomes_all_reagg %>%
  # 3-, 5-, and 8-week intervals after hurricane
  mutate(hurricane_3week = ifelse(date >= as.Date("2024-09-24") & date <= as.Date("2024-09-24") + 21, TRUE, FALSE),
         hurricane_5week = ifelse(date >= as.Date("2024-09-24") & date <= as.Date("2024-09-24") + 35, TRUE, FALSE),
         hurricane_8week = ifelse(date >= as.Date("2024-09-24") & date <= as.Date("2024-09-24") + 56, TRUE, FALSE),
         # based on whether the majority of the month is in that season
         season = case_when(month %in% c(1, 2, 3) ~ "Winter",
                            month %in% c(4, 5, 6) ~ "Spring",
                            month %in% c(7, 8, 9) ~ "Summer",
                            month %in% c(10, 11, 12) ~ "Fall"))

state_by_week <- join_outcomes_all_reagg %>%
  group_by(week_start, date, weeks_since_anchor, days_since_anchor, month, year, season, encounter_year) %>%
  summarise(n_events = sum(n_events),
            n_foodborne = sum(n_foodborne),
            hurricane_3week = unique(hurricane_3week),
            hurricane_5week = unique(hurricane_5week),
            hurricane_8week = unique(hurricane_8week)) %>%
  ungroup()

state_pop <- exposure %>%
  group_by(year) %>%
  summarise(total_population = sum(total_population)) %>%
  ungroup()

state_by_week <- left_join(state_by_week, state_pop %>% mutate(year = as.character(year)), 
                           by = c("encounter_year" = "year"))

join_outcomes_all_reagg <- join_outcomes_all_reagg %>%
  dplyr::select(-X, -encounter_year)

state_by_week <- state_by_week %>%
  dplyr::select(-encounter_year)

if (FALSE) {
  write_csv(join_outcomes_all_reagg, "data/processed_data/dataset_with_added_masked_units.csv")
  write_csv(state_by_week, "data/processed_data/state_by_week_with_added_masked_units.csv")
}
