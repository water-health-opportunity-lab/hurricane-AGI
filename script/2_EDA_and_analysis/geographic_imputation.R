################################################################################
# Purpose:
#   Allocate geographically masked ZIP2/ZIP1 counts to compatible ZIP3 regions.
#
# Main conceptual change from the original code:
#   The regression predictions are used as RELATIVE ZIP3 INTENSITIES.
#   They are not treated directly as the number of imputed cases.
#
#   Within each masked geographic unit and week, predicted intensities are
#   normalized to sum to 1. The observed masked total is then allocated across
#   the compatible ZIP3 regions.
################################################################################

library(MASS)
library(tidyverse)

dat_masked <- read.csv(
  "data/processed_data/dataset_with_added_masked_units_for_imputation.csv"
)

dat_masked$zip3 <- as.character(dat_masked$zip3)

dat <- read_csv(
  "data/processed_data/analytic_dataset.csv",
  show_col_types = FALSE
)

dat$zip3 <- as.character(dat$zip3)


################################################################################
# ASSUMPTIONS I made - PLEASE CHECK!
################################################################################

# ASSUMPTION 1:
# obs_events is the observed ZIP3-specific AGI count for each ZIP3-week.
#
# This should be the outcome used to estimate the expected relative incidence
# across ZIP3 regions.
#
# If obs_events has a different meaning, replace it with the actual observed
# ZIP3-specific count variable.


# ASSUMPTION 2:
# n_events_agg is the known masked aggregate count for a ZIP2- or ZIP1-week.
#
# For example, suppose ZIP2 = "27" has 15 masked cases during a particular week.
# The compatible ZIP3 rows may each contain n_events_agg = 15.
#
# If n_events_agg is repeated across compatible ZIP3 rows, it should NOT be
# treated as an independently observed ZIP3-level outcome. It is one fixed total
# that must be distributed across those ZIP3 regions.


# ASSUMPTION 3:
# The expanded dat_masked dataset contains variables identifying:
#
#   mask_level       = "zip2" or "zip1"
#   mask_geography   = the corresponding ZIP2 or ZIP1 code
#
# Replace these names below if the actual variables have different names.


# ASSUMPTION 4:
# Each row represents one candidate ZIP3 for one masked geographic total and
# week. All rows corresponding to the same masked total have the same:
#
#   week_start
#   mask_level
#   mask_geography
#   n_events_agg
#
# We create masked_block_id from these variables.


################################################################################
# 1. CREATE AN IDENTIFIER FOR EACH MASKED AGGREGATE TOTAL
################################################################################

dat_masked <- dat_masked %>%
  mutate(
    zip3 = as.character(zip3),
    mask_level = as.character(mask_level),
    mask_geography = as.character(mask_geography),
    
    # This identifier should uniquely represent one observed masked total.
    masked_block_id = interaction(
      week_start,
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

# The model is fitted to the observed ZIP3-specific counts.
#
# Population is included as an offset, so the model is estimating incidence
# rates while allowing expected counts to scale with population.
#
# The ZIP3 term allows baseline AGI rates to differ across ZIP3 regions.
#
# I would initially avoid the extensive ZIP3-by-year, ZIP3-by-month, and
# ZIP3-by-weather interactions in m4-m6. Those models may contain many unstable
# parameters and are not needed for the basic allocation procedure.

allocation_model <- glm.nb(
  obs_events ~
    zip3 +
    tmean +
    ppt_mean +
    humidity +
    as.factor(year) +
    as.factor(month) +
    weeks_since_anchor +
    offset(log(total_population)),
  data = dat_masked_train
)

summary(allocation_model)


################################################################################
# 4. PREDICT EXPECTED ZIP3 INTENSITIES
################################################################################

# These are NOT the imputed masked case counts.
#
# They represent the expected relative intensity of AGI cases in each candidate
# ZIP3-week, based on population, baseline ZIP3 differences, weather,
# seasonality, and the pre-hurricane time trend.

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
#
#     p_zt = mu_hat_zt / sum(mu_hat_kt)
#
# where the denominator is summed over all ZIP3 regions compatible with the
# same ZIP2- or ZIP1-level masked total.
#
# The allocation probabilities therefore sum to 1 within each masked block.

dat_masked <- dat_masked %>%
  group_by(masked_block_id) %>%
  mutate(
    sum_pred_intensity = sum(pred_intensity),
    
    allocation_probability =
      pred_intensity / sum_pred_intensity,
    
    # Deterministic expected allocation:
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

# These differences should be zero apart from numerical precision.
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
# 8. OPTIONAL: CREATE INTEGER ALLOCATIONS USING A MULTINOMIAL DRAW
################################################################################

# The expected allocations above may be fractional.
#
# For multiple imputation, draw integer allocations from a multinomial
# distribution. Each draw preserves the observed masked total exactly.

allocate_one_masked_block <- function(block_data) {
  
  # All rows in this block should correspond to the same aggregate total.
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
  
  # Normalize again to protect against small floating-point discrepancies.
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
  group_by(zip3, week_start) %>%
  summarise(
    imputed_masked_events = sum(allocated_events),
    .groups = "drop"
  )


################################################################################
# 12. ADD THE IMPUTED MASKED COUNTS TO THE OBSERVED ZIP3 COUNTS
################################################################################

# ASSUMPTION 5:
# In the analytic dataset, n_events is the directly observed ZIP3-specific
# event count to which the imputed masked cases should be added.

dat_completed <- dat %>%
  left_join(
    allocated_by_zip3_week,
    by = c("zip3", "week_start")
  ) %>%
  mutate(
    imputed_masked_events =
      replace_na(imputed_masked_events, 0),
    
    completed_events =
      n_events + imputed_masked_events
  )


################################################################################
# 13. OPTIONAL: GENERATE MULTIPLE IMPUTED DATASETS
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

# The interrupted time-series analysis should then be fitted separately in
# each completed dataset, and the estimates should be combined across
# imputations.


################################################################################
# IMPORTANT INTERPRETIVE NOTE
################################################################################

# This procedure assumes that the fitted pre-hurricane model provides useful
# estimates of the relative distribution of AGI cases across compatible ZIP3s.
#
# It does not establish that the geographic masking process is random.
# If small counts are more likely to be masked, the observed ZIP3 counts may
# not be fully representative of the masked cases.
#
# A useful validation analysis would artificially aggregate known
# pre-hurricane ZIP3 counts to ZIP2, apply this method, and compare the
# reconstructed ZIP3 counts with the actual counts.
################################################################################