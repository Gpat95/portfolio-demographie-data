# ============================================================================== 
# PROJECT: INTERNATIONAL MIGRATION FLOW DATA QUALITY & ANOMALY DETECTION
# ==============================================================================
#
# Purpose:
# This script presents a portfolio version of a larger analytical workflow
# developed to assess the quality, coverage, and consistency of international
# migration-flow data.
#
# Main components:
#   1. Data Preparation & Quality Checks
#   2. Log-GAM Trend Modelling
#   3. Anomaly Detection with Tolerance Bands
#   4. Temporal Coverage Analysis
#   5. Geographic Coverage Analysis
#   6. Outlier Pattern Analysis
#
# NOTE:
# The original database and complete production workflow are not included.
# This script is a simplified portfolio implementation intended to demonstrate
# the analytical methodology and R programming skills.
#
# ==============================================================================


# ==============================================================================
# 0. PACKAGES
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(sf)
library(rnaturalearth)


# ==============================================================================
# PART 1 — DATA PREPARATION & QUALITY CHECKS
# ==============================================================================

# Purpose:
# Validate the structure of the migration database and prepare the analytical
# sample used throughout the workflow.

required_columns <- c(
  "from.ISO3c",
  "to.ISO3c",
  "year",
  "value",
  "source"
)

missing_columns <- setdiff(
  required_columns,
  names(FULL_MIG)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# Prepare analytical dataset
analysis_data <- FULL_MIG %>%
  filter(
    from.ISO3c != "OTH",
    to.ISO3c != "OTH",
    from.ISO3c != to.ISO3c,
    !is.na(year),
    !is.na(value)
  ) %>%
  arrange(
    from.ISO3c,
    to.ISO3c,
    year,
    source
  )


# Basic quality-control information
cat("Number of observations:", nrow(analysis_data), "\n")

cat(
  "Number of origin-destination pairs:",
  n_distinct(
    paste(
      analysis_data$from.ISO3c,
      analysis_data$to.ISO3c
    )
  ),
  "\n"
)

cat(
  "Number of data sources:",
  n_distinct(analysis_data$source),
  "\n"
)



# ==============================================================================
# PART 2 — LOG-GAM TREND MODELLING BY ORIGIN–DESTINATION PAIR
# ==============================================================================

# Purpose:
# Estimate a flexible temporal trend in migration flows for each
# origin-destination pair using a Generalized Additive Model (GAM).
#
# The logarithmic transformation reduces the influence of very large
# migration-flow values.


MIN_OBSERVATIONS <- 8


# Automatically select spline complexity
automatic_k <- function(data) {
  min(10, max(4, nrow(data) - 1))
}


# Fit one Log-GAM
fit_log_gam <- function(data) {
  
  k <- automatic_k(data)
  
  model <- gam(
    log(value + 10) ~
      s(year, bs = "cs", k = k),
    data = data,
    method = "REML"
  )
  
  prediction_log <- predict(
    model,
    newdata = data
  )
  
  expected_flow <- pmax(
    0,
    exp(prediction_log) - 10
  )
  
  data %>%
    mutate(
      expected_flow = expected_flow
    )
}


# Safe modelling function
fit_od_pair <- function(data) {
  
  # Insufficient observations
  if (nrow(data) < MIN_OBSERVATIONS) {
    
    return(
      data %>%
        mutate(
          expected_flow = NA_real_,
          model_status = "insufficient_observations"
        )
    )
  }
  
  
  # Model estimation with error handling
  tryCatch(
    
    fit_log_gam(data) %>%
      mutate(
        model_status = "ok"
      ),
    
    error = function(e) {
      
      data %>%
        mutate(
          expected_flow = NA_real_,
          model_status = "model_error"
        )
    }
  )
}


# Apply the model automatically to every O-D pair
predicted_flows <- analysis_data %>%
  group_by(
    from.ISO3c,
    to.ISO3c
  ) %>%
  group_modify(
    ~ fit_od_pair(.x)
  ) %>%
  ungroup()



# ==============================================================================
# PART 3 — ANOMALY DETECTION USING TOLERANCE BANDS
# ==============================================================================

# Purpose:
# Compare observed migration flows with GAM-based expected flows.
#
# An observation is flagged as a potential anomaly when it lies outside
# a tolerance interval around the model prediction.


MIN_ABSOLUTE_TOLERANCE <- 100


add_tolerance_band <- function(
    data,
    tolerance = 0.10,
    min_absolute = MIN_ABSOLUTE_TOLERANCE
) {
  
  data %>%
    mutate(
      
      # Difference between observed and predicted flow
      difference =
        value - expected_flow,
      
      # Relative deviation
      relative_difference =
        if_else(
          expected_flow > 0,
          difference / expected_flow,
          NA_real_
        ),
      
      # Combine relative and absolute tolerance
      tolerance_value =
        pmax(
          tolerance * expected_flow,
          min_absolute
        ),
      
      # Lower tolerance limit
      lower_bound =
        pmax(
          0,
          expected_flow - tolerance_value
        ),
      
      # Upper tolerance limit
      upper_bound =
        expected_flow + tolerance_value,
      
      # Potential anomaly
      is_outlier =
        if_else(
          model_status == "ok",
          value < lower_bound |
            value > upper_bound,
          NA
        )
    )
}


# Example: 10% tolerance
gam_results <- predicted_flows %>%
  add_tolerance_band(
    tolerance = 0.10
  )


# Summary
anomaly_summary <- gam_results %>%
  filter(model_status == "ok") %>%
  summarise(
    observations = n(),
    outliers = sum(is_outlier, na.rm = TRUE),
    outlier_share =
      100 * mean(is_outlier, na.rm = TRUE)
  )

print(anomaly_summary)



# ==============================================================================
# PART 4 — TEMPORAL COVERAGE OF MIGRATION DATA
# ==============================================================================

# Purpose:
# Evaluate how migration-data availability varies across sources and years.


temporal_coverage <- analysis_data %>%
  filter(
    !is.na(source),
    !is.na(year)
  ) %>%
  count(
    source,
    year,
    name = "n_observations"
  )


# Summary by source
temporal_coverage_summary <- temporal_coverage %>%
  group_by(source) %>%
  summarise(
    first_year = min(year),
    last_year = max(year),
    n_years = n_distinct(year),
    total_observations =
      sum(n_observations),
    .groups = "drop"
  )

print(temporal_coverage_summary)


# Visualize temporal coverage
figure_temporal_coverage <- ggplot(
  temporal_coverage,
  aes(
    x = year,
    y = source,
    fill = n_observations
  )
) +
  geom_tile() +
  scale_fill_viridis_c(
    trans = "log10",
    name = "Observations"
  ) +
  labs(
    title = "Temporal Coverage by Data Source",
    subtitle = "Availability of migration-flow observations across years",
    x = "Year",
    y = "Data source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank()
  )

figure_temporal_coverage



# ==============================================================================
# PART 5 — GEOGRAPHIC COVERAGE OF MIGRATION DATA
# ==============================================================================

# Purpose:
# Examine geographic differences in data availability by calculating
# the number of distinct migration-data sources available for each
# destination country.


sources_by_destination <- analysis_data %>%
  filter(
    !is.na(to.ISO3c),
    !is.na(source)
  ) %>%
  group_by(to.ISO3c) %>%
  summarise(
    n_sources =
      n_distinct(source),
    .groups = "drop"
  )


# Load world boundaries
world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# Join migration information to geographic data
map_data <- world %>%
  left_join(
    sources_by_destination,
    by = c(
      "iso_a3_eh" = "to.ISO3c"
    )
  )


# Identify unmatched country codes
unmatched_destinations <- sources_by_destination %>%
  anti_join(
    world,
    by = c(
      "to.ISO3c" = "iso_a3_eh"
    )
  )

print(unmatched_destinations)


# Map geographic coverage
figure_geographic_coverage <- ggplot(
  map_data
) +
  geom_sf(
    aes(fill = n_sources),
    linewidth = 0.1
  ) +
  scale_fill_viridis_c(
    na.value = "grey90",
    name = "Number\nof sources"
  ) +
  labs(
    title = "Geographic Coverage of International Migration Data",
    subtitle = "Number of distinct data sources by destination country"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

figure_geographic_coverage



# ==============================================================================
# PART 6 — OUTLIER PATTERN ANALYSIS
# ==============================================================================

# Purpose:
# Move beyond individual anomaly detection and investigate whether
# unusual observations are systematically associated with particular
# data sources or other characteristics.


# ------------------------------------------------------------------------------
# 6.1 Outlier prevalence by data source
# ------------------------------------------------------------------------------

outliers_by_source <- gam_results %>%
  filter(
    model_status == "ok",
    !is.na(source)
  ) %>%
  group_by(source) %>%
  summarise(
    observations = n(),
    
    outliers =
      sum(
        is_outlier,
        na.rm = TRUE
      ),
    
    outlier_share =
      100 *
      mean(
        is_outlier,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(outlier_share)
  )

print(outliers_by_source)


# Visualize outlier prevalence
figure_outliers_source <- ggplot(
  outliers_by_source,
  aes(
    x = outlier_share,
    y = reorder(
      source,
      outlier_share
    )
  )
) +
  geom_col() +
  labs(
    title = "Prevalence of Potential Anomalies by Data Source",
    subtitle = "Observations outside the GAM-based tolerance interval",
    x = "Outlier observations (%)",
    y = "Data source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y =
      element_blank()
  )

figure_outliers_source


# ------------------------------------------------------------------------------
# 6.2 Overall distribution of model results
# ------------------------------------------------------------------------------

outlier_distribution <- gam_results %>%
  filter(
    model_status == "ok"
  ) %>%
  mutate(
    classification =
      if_else(
        is_outlier,
        "Potential anomaly",
        "Within tolerance"
      )
  ) %>%
  count(
    classification,
    name = "n_observations"
  ) %>%
  mutate(
    percentage =
      100 *
      n_observations /
      sum(n_observations)
  )

print(outlier_distribution)



# ==============================================================================
# END OF PORTFOLIO ANALYSIS
# ==============================================================================
#
# This public script demonstrates:
#
#   • Data validation and cleaning
#   • Functional programming in R
#   • Automated model fitting
#   • Generalized Additive Models (GAM)
#   • Error handling
#   • Anomaly detection
#   • Temporal data-quality assessment
#   • Geographic data analysis
#   • Spatial joins and mapping
#   • Data visualization with ggplot2
#
# The original migration database, complete analytical workflow,
# internal outputs, and project-specific analyses are intentionally
# excluded from this portfolio version.
#
# ==============================================================================