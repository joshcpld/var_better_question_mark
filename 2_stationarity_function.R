################################################################################
################################### SET UP #####################################
################################################################################

library(tidyverse)
library(urca)


################################################################################
################################ FUNCTION ######################################
################################################################################

stationarity_tests <- function(data, test_types = NULL, diff_order = 0) {
  
  # Tidy the data by pivoting it longer and grouping by name
  data_tidy <- data %>%
    pivot_longer(cols = -date, names_to = "name", values_to = "value") %>%
    group_by(name) %>%
    nest()
  
  # Apply the ADF test to each time series and store the results
  stationarity_results <- data_tidy %>%
    mutate(
      adf_test = map2(name, data, ~ {
        # Extract time series values
        ts_data <- .y$value
        
        # Apply differencing based on the specified order
        if (diff_order > 0) {
          ts_data <- diff(ts_data, differences = diff_order)
        }
        
        # Determine the test type (default to "drift" if not specified)
        test_type <- if (!is.null(test_types) && .x %in% names(test_types)) {
          test_types[[.x]]
        } else {
          "drift"
        }
        
        # Perform ADF test with BIC-based lag selection
        adf_result <- ur.df(ts_data, type = test_type, selectlags = "BIC")
        
        # Extract test statistic and critical values
        test_stat <- adf_result@teststat[1]
        crit_1pc <- adf_result@cval[1, 1]
        crit_5pc <- adf_result@cval[1, 2]
        crit_10pc <- adf_result@cval[1, 3]
        
        # Determine rejection status (TRUE means we reject null of unit root -> I(0))
        reject_1pc <- test_stat < crit_1pc
        reject_5pc <- test_stat < crit_5pc
        reject_10pc <- test_stat < crit_10pc
        
        # Determine integration order based on rejection levels
        integration_order <- case_when(
          reject_1pc ~ "I(0) at 1% significance",
          reject_5pc ~ "I(0) at 5% significance",
          reject_10pc ~ "I(0) at 10% significance",
          TRUE ~ "I(1)"
        )
        
        # Return results as a dataframe
        data.frame(
          test_statistic = test_stat,
          crit_value_1pc = crit_1pc,
          crit_value_5pc = crit_5pc,
          crit_value_10pc = crit_10pc,
          reject_1pc = reject_1pc,
          reject_5pc = reject_5pc,
          reject_10pc = reject_10pc,
          integration_order = integration_order,
          test_type = test_type
        )
      })
    ) %>%
    select(name, adf_test) %>%
    unnest(cols = adf_test)
  
  # Check if all variables share the same integration order
  integration_levels <- unique(stationarity_results$integration_order)
  
  # Determine the strongest level of confidence at which integration orders are consistent
  if (length(integration_levels) == 1) {
    message <- paste("All variables share the same integration order:", integration_levels)
  } else if (all(stationarity_results$reject_5pc == stationarity_results$reject_10pc)) {
    message <- "All variables share the same integration order at the 10% significance level."
  } else if (all(stationarity_results$reject_1pc == stationarity_results$reject_5pc)) {
    message <- "All variables share the same integration order at the 5% significance level."
  } else {
    message <- "The variables do not share the same integration order at any conventional significance level."
  }
  
  # Print the message
  print(message)
  
  return(stationarity_results)
}











# Example usage with default test type (drift)

stationarity_results <- stationarity_tests(model_1_data)
# print(stationarity_results)

# Example with custom test types

# test_types <- list(gdp = "trend", rnu = "none", cpi = "drift")
# 
# stationarity_results_custom <- stationarity_tests(model_1_data, test_types)
# 
# print(stationarity_results_custom)
  







