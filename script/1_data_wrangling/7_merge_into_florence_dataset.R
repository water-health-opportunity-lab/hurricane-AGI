###############################################################################
# Primary Authors: Jahred Liddie, Katie O'Brien
# Purpose: Merge together outcome/case data, exposure data, and covariates
# Date created: 2/18/2026
###############################################################################

library(tidyverse)

outcomes <- read_csv("data/raw_data/raw_truveta_export_01272026.csv")

covariates <- read.csv("data/processed_data/env_data.csv")

florence_exposure <- read.csv("data/processed_data/florence_exp.csv")

###############################################################################
florence_exposure_long <- florence_exposure %>%
  pivot_longer(cols = starts_with("population"),
               names_to = c("year"),
               values_to = c("total_population")) %>%
  mutate(year = parse_number(year))

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
  group_by(zip3) %>%
  mutate(lag1_n_events = dplyr::lag(n_events, n = 1, default = 0),
         lag2_n_events = dplyr::lag(n_events, n = 2, default = 0),
         lag3_n_events = dplyr::lag(n_events, n = 3, default = 0),
         lag10_n_events = dplyr::lag(n_events, n = 10, default = 0)) %>%
  ungroup()

join_outcomes <- join_outcomes %>%       
  filter(year(as.Date(join_outcomes$week_start, format = "%Y%m%d")) != 2016)

florence_exposure_long$zip3 <- as.factor(florence_exposure_long$zip3)

join_outcomes_exposure <- left_join(join_outcomes, 
                                    florence_exposure_long %>% 
                                      dplyr::select(zip3, total_population, year,
                                                    inundation_exposure),
                                    by = c("zip3", "year"))

# drop data after 2018 for florence analyses
join_outcomes_exposure <- join_outcomes_exposure %>%
  filter(year < 2019)

join_outcomes_exposure <- join_outcomes_exposure %>%
  # 3-, 5-, and 8-week intervals after hurricane
  mutate(hurricane_3week = ifelse(date >= as.Date("2018-09-14") & date <= as.Date("2018-09-14") + 14, TRUE, FALSE),
         hurricane_5week = ifelse(date >= as.Date("2018-09-14") & date <= as.Date("2018-09-14") + 28, TRUE, FALSE),
         hurricane_8week = ifelse(date >= as.Date("2018-09-14") & date <= as.Date("2018-09-14") + 49, TRUE, FALSE),
         # based on whether the majority of the month is in that season
         season = case_when(month %in% c(1, 2, 3) ~ "Winter",
                            month %in% c(4, 5, 6) ~ "Spring",
                            month %in% c(7, 8, 9) ~ "Summer",
                            month %in% c(10, 11, 12) ~ "Fall"))

join_outcomes_exposure <- join_outcomes_exposure %>%
  dplyr::select(-X, -encounter_year)

if (FALSE) {
  write_csv(join_outcomes_exposure, "data/processed_data/florence_analytic_dataset.csv")
}
