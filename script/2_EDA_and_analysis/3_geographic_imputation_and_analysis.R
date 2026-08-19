################################################################################
# primary authors: pramita bagchi, jahred liddie
# purpose: allocate geographically masked ZIP2/ZIP1 counts to compatible 
#   ZIP3 regions. Within each masked geographic unit and week, 
#   predicted intensities are
#   normalized to sum to 1. The observed masked total is then allocated across
#   the compatible ZIP3 regions.
# date created: 8/17/26
################################################################################
library(broom)
library(spdep)
library(mitools)
library(tigris)
library(tidyverse)

dat_masked <- read.csv(
  "data/processed_data/dataset_with_added_masked_units_for_imputation.csv"
)

dat <- read_csv(
  "data/processed_data/analytic_dataset.csv",
  show_col_types = FALSE
)

dat$zip3 <- as.character(dat$zip3)

################################################################################
# 1. CREATE AN IDENTIFIER FOR EACH MASKED AGGREGATE TOTAL
################################################################################
dat_masked <- dat_masked %>%
  group_by(zip3, week_start, year) %>%
  mutate(dup_week = duplicated(zip3_masked)) %>%
  ungroup %>%
  mutate(
    zip3 = as.character(zip3),
    mask_geography = as.character(zip3_masked),
    mask_level = case_when(grepl("27/*", zip3_masked) | 
                             grepl("28/*", zip3_masked) ~ "zip2",
                           TRUE ~ as.character("zip1")),
    
    # this uniquely represents one observed, masked total (zip2- or zip1-by-week)
    masked_block_id = interaction(
      week_start,
      dup_week,
      mask_level,
      mask_geography,
      drop = TRUE
    )
  )

################################################################################
# 2. RESTRICT MODEL FITTING TO THE PRE-HURRICANE PERIOD
################################################################################
dat_masked_train <- dat_masked %>%
  filter(week_start < 20240927)

################################################################################
# 3. FIT A MODEL FOR ZIP3-SPECIFIC EXPECTED INTENSITY
################################################################################
# The model is fitted to the observed ZIP3-specific counts. Population is 
# included as an offset, so the model is estimating incidence
# rates while allowing expected counts to scale with population. The ZIP3 
# term allows baseline AGI rates to differ across ZIP3 regions.

allocation_model <- glm(
  obs_events ~
    zip3 +
    tmean +
    ppt_mean +
    humidity +
    as.factor(year) +
    as.factor(month) +
    weeks_since_anchor,
    offset = log(total_population),
  data = dat_masked_train,
  family = "quasipoisson"
)

summary(allocation_model)

################################################################################
# 4. PREDICT EXPECTED ZIP3 INTENSITIES
################################################################################
# These are NOT the imputed masked case counts. They represent the expected 
# relative intensity of AGI cases in each candidate ZIP3-week, based on 
# population, baseline ZIP3 differences, weather, seasonality, 
# and the pre-hurricane time trend.

dat_masked$pred_intensity <- predict(
  allocation_model,
  newdata = dat_masked,
  type = "response"
)

################################################################################
# 5. CHECK THE PREDICTIONS
################################################################################
if (any(is.na(dat_masked$pred_intensity))) {
  stop(
    "Some predicted intensities are missing. Check missing covariates, ",
    "new ZIP3 factor levels, or other differences between training and ",
    "prediction data."
  )
}

if (any(dat_masked$pred_intensity < 0)) {
  stop("Predicted intensities should not be negative.")
}

################################################################################
# 6. NORMALIZE THE INTENSITIES WITHIN EACH MASKED UNIT
################################################################################
# For masked block g in week t:
#     p_zt = mu_hat_zt / sum(mu_hat_kt)
# where the denominator is summed over all ZIP3 regions compatible with the
# same ZIP2- or ZIP1-level masked total. The allocation probabilities 
# therefore sum to 1 within each masked block.

dat_masked <- dat_masked %>%
  group_by(masked_block_id) %>%
  mutate(
    sum_pred_intensity = sum(pred_intensity),
    
    allocation_probability =
      pred_intensity / sum_pred_intensity,
    
    # deterministic expected allocation:
    expected_allocated_events =
      n_events_agg * allocation_probability
  ) %>%
  ungroup()

################################################################################
# 7. VERIFY THAT THE ALLOCATED COUNTS PRESERVE THE MASKED TOTALS
################################################################################
allocation_check <- dat_masked %>%
  group_by(masked_block_id) %>%
  summarise(
    mask_level = first(mask_level),
    mask_geography = first(mask_geography),
    week_start = first(week_start),
    
    masked_total = first(n_events_agg),
    
    sum_probabilities =
      sum(allocation_probability),
    
    sum_expected_allocations =
      sum(expected_allocated_events),
    
    .groups = "drop"
  )

print(allocation_check)

# these differences should be zero apart from numerical precision
allocation_check <- allocation_check %>%
  mutate(
    probability_error =
      sum_probabilities - 1,
    
    allocation_error =
      sum_expected_allocations - masked_total
  )

summary(allocation_check$probability_error)
summary(allocation_check$allocation_error) 

################################################################################
# 8. CREATE INTEGER ALLOCATIONS USING A MULTINOMIAL DRAW
################################################################################
# The expected allocations above may be fractional. For multiple imputation, 
# draw integer allocations from a multinomial distribution. 
# Each draw preserves the observed masked total exactly.

allocate_one_masked_block <- function(block_data) {
  
  # all rows in this block should correspond to the same aggregate total
  masked_total <- unique(block_data$n_events_agg)
  
  if (length(masked_total) != 1) {
    stop(
      "A masked block contains more than one value of n_events_agg. ",
      "Check how masked_block_id was constructed."
    )
  }
  
  if (masked_total %% 1 != 0) {
    stop("n_events_agg must be an integer for multinomial allocation.")
  }
  
  probabilities <- block_data$allocation_probability
  
  # normalize again to protect against small floating-point discrepancies
  probabilities <- probabilities / sum(probabilities)
  
  allocated_counts <- as.vector(
    rmultinom(
      n = 1,
      size = masked_total,
      prob = probabilities
    )
  )
  
  block_data$allocated_events <- allocated_counts
  
  return(block_data)
}

################################################################################
# 9. GENERATE ONE IMPUTED DATASET
################################################################################
set.seed(1001)

imputed_allocation_1 <- dat_masked %>%
  group_by(masked_block_id) %>%
  group_modify(
    ~ allocate_one_masked_block(.x)
  ) %>%
  ungroup()

################################################################################
# 10. VERIFY THE INTEGER ALLOCATIONS
################################################################################
integer_allocation_check <- imputed_allocation_1 %>%
  group_by(masked_block_id) %>%
  summarise(
    masked_total = first(n_events_agg),
    allocated_total = sum(allocated_events),
    .groups = "drop"
  )

if (any(
  integer_allocation_check$masked_total !=
  integer_allocation_check$allocated_total
)) {
  stop("At least one imputed allocation does not preserve the masked total.")
}

################################################################################
# 11. SUM ALLOCATED MASKED CASES BY ZIP3 AND WEEK
################################################################################
# A ZIP3-week could potentially receive cases from more than one masked block,
# particularly if both ZIP1- and ZIP2-level masked records exist.
allocated_by_zip3_week <- imputed_allocation_1 %>%
  group_by(zip3, week_start, dup_week) %>%
  summarise(
    imputed_masked_events = sum(allocated_events),
    .groups = "drop"
  )

################################################################################
# 12. ADD THE IMPUTED MASKED COUNTS TO THE OBSERVED ZIP3 COUNTS
################################################################################
dat_completed <- dat %>%
  left_join(
    allocated_by_zip3_week,
    by = c("zip3", "week_start", "dup_week")
  ) %>%
  mutate(
    imputed_masked_events =
      replace_na(imputed_masked_events, 0),
    
    completed_events =
      n_events + imputed_masked_events
  )

################################################################################
# 13. GENERATE MULTIPLE IMPUTED DATASETS
################################################################################
n_imputations <- 20

all_imputed_allocations <- map_dfr(
  1:n_imputations,
  function(imputation_number) {
    
    set.seed(1000 + imputation_number)
    
    dat_masked %>%
      group_by(masked_block_id) %>%
      group_modify(
        ~ allocate_one_masked_block(.x)
      ) %>%
      ungroup() %>%
      mutate(
        imputation = imputation_number
      )
  }
)

sum_imputed_allocations <- all_imputed_allocations %>%
  group_by(zip3, week_start, dup_week, imputation) %>%
  summarise(
    imputed_masked_events = sum(allocated_events),
    .groups = "drop"
  )

dat_all_imputed <- expand_grid(dat, imputation = 1:20)

dat_all_imputed <- dat_all_imputed %>%
  left_join(
    sum_imputed_allocations,
    by = c("zip3", "week_start", "dup_week", "imputation")
  ) %>%
  mutate(
    imputed_masked_events =
      replace_na(imputed_masked_events, 0),
    
    completed_events =
      n_events + imputed_masked_events
  )

################################################################################
# 14. RECALCULATE SPATIAL LAGS 
################################################################################
source("script/2_EDA_and_analysis/analysis_functions.R")

# getting zcta shapes using tigris package - 2020 is most recent available
zcta_geometry <- tigris::zctas(year = 2020, cb = TRUE)

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

dat_neighbors <- dat_all_imputed %>%
  group_by(zip3) %>%
  slice(1) %>%
  mutate(zip3 = as.character(zip3)) %>%
  ungroup() %>%
  mutate(id = as.factor(row_number()))

dat_neighbors <- left_join(nc_zip3_geom, dat_neighbors, by = "zip3")
dat_neighbors <- st_as_sf(dat_neighbors, coords = geometry, crs = st_crs(nc_zip3_geom))

nb <- poly2nb(dat_neighbors, queen = TRUE)

dat_neighbors <- map_dfr(1:20, ~id_neighbors.f(row_numbers = .x))
dat_neighbors <- dat_neighbors %>% 
  dplyr::select(zip3, id, neighbors) %>%
  st_drop_geometry()

dat_all_imputed <- left_join(dat_all_imputed, dat_neighbors)

dat_all_imputed <- dat_all_imputed %>% 
  group_by(week_start, imputation) %>% 
  rowwise() %>%
  mutate(neighbor_weight = 1/length(unlist(neighbors))) %>%
  mutate(neighbor_cases_weighted = sum( neighbor_weight * dat_all_imputed$completed_events[dat_all_imputed$id %in% unlist(neighbors) &
                                                                                             dat_all_imputed$week_start == week_start &
                                                                                             dat_all_imputed$imputation == imputation] ),
         neighbor_cases_unweighted = sum( dat_all_imputed$completed_events[dat_all_imputed$id %in% unlist(neighbors) & 
                                                                             dat_all_imputed$week_start == week_start &
                                                                             dat_all_imputed$imputation == imputation])) %>%
  ungroup()

################################################################################
# 15. RERUN MAIN REGRESSION MODELS
################################################################################
nested_data <- dat_all_imputed %>%
  group_by(imputation) %>%
  nest()

fit_model <- function(df, formula) {
  
  reg_formula <- as.formula(formula)
  
  m_initial <- glm(reg_formula,
                   offset = log(total_population),
                   data = df, 
                   family = "quasipoisson")
}

nested_data_m1 <- nested_data %>%
  mutate(model = map(data, 
                     ~fit_model(df = .x, 
                     formula = "completed_events ~ inundation_exposure*hurricane_3week +
                     inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
                     log(neighbor_cases_weighted + 1)")),
         output = map(model, broom::tidy))

res1 <- nested_data_m1 %>% 
  unnest(output) %>%
  ungroup()

nested_data_m2 <- nested_data %>%
  mutate(model = map(data, 
                     ~fit_model(df = .x, 
                     formula = "completed_events ~ inundation_exposure*hurricane_5week +
                     inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
                     log(neighbor_cases_weighted + 1)")),
         output = map(model, broom::tidy))

res2 <- nested_data_m2 %>% 
  unnest(output) %>%
  ungroup()

nested_data_m3 <- nested_data %>%
  mutate(model = map(data, 
                     ~fit_model(df = .x, 
                                formula = "completed_events ~ inundation_exposure*hurricane_8week +
                     inundation_exposure*as.factor(year) + inundation_exposure*as.factor(month) +
                     log(neighbor_cases_weighted + 1)")),
         output = map(model, broom::tidy))

res3 <- nested_data_m3 %>% 
  unnest(output) %>%
  ungroup()

################################################################################
# 16. POOL RESULTS FROM MULTIPLY IMPUTED DATASETS
################################################################################
betas_m1 <- MIextract(res1$model, fun = coef) 
var_m1 <- MIextract(res1$model, fun = vcov)

m1_pooled_results <- summary(MIcombine(betas_m1, var_m1))

betas_m2 <- MIextract(res2$model, fun = coef) 
var_m2 <- MIextract(res2$model, fun = vcov)

m2_pooled_results <- summary(MIcombine(betas_m2, var_m2))

betas_m3 <- MIextract(res3$model, fun = coef) 
var_m3 <- MIextract(res3$model, fun = vcov)

m3_pooled_results <- summary(MIcombine(betas_m3, var_m3))
