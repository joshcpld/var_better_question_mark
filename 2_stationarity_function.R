################################################################################
################################### SET UP #####################################
################################################################################

library(tidyverse)
library(urca)


################################################################################
################################ FUNCTION ######################################
################################################################################

stationarity_tests <- function(data, test_types = NULL, diff_order) {
  
  # Tidy the data by pivoting it longer and grouping by name
  data_tidy <- data %>%
    pivot_longer(cols = -date, names_to = "name", values_to = "value") %>%
    group_by(name) %>%
    nest()
  
  # Apply the ADF test to each time series and store the results
  stationarity_results <- data_tidy %>%
    mutate(
      adf_test = map2(name, data, ~ {
        ts_data <- .y$value
        
        if (diff_order > 0) {
          ts_data <- diff(ts_data, differences = diff_order)
        }
        
        test_type <- if (!is.null(test_types) && .x %in% names(test_types)) {
          test_types[[.x]]
        } else {
          "drift"
        }
        
        adf_result <- ur.df(ts_data, type = test_type, selectlags = "BIC")
        
        test_stat <- adf_result@teststat[1]
        crit_1pc <- adf_result@cval[1, 1]
        crit_5pc <- adf_result@cval[1, 2]
        crit_10pc <- adf_result@cval[1, 3]
        
        reject_1pc <- test_stat < crit_1pc
        reject_5pc <- test_stat < crit_5pc
        reject_10pc <- test_stat < crit_10pc
        
        data.frame(
          test_statistic = test_stat,
          reject_1pc = reject_1pc,
          reject_5pc = reject_5pc,
          reject_10pc = reject_10pc,
          test_type = test_type
        )
      })
    ) %>%
    select(name, adf_test) %>%
    unnest(cols = adf_test, names_repair = "unique")
  
  # Categorize variables based on rejection levels
  cannot_reject <- stationarity_results %>%
    filter(!reject_10pc) %>%
    pull(name)
  reject_10pc <- stationarity_results %>%
    filter(reject_10pc & !reject_5pc) %>%
    pull(name)
  reject_5pc <- stationarity_results %>%
    filter(reject_5pc & !reject_1pc) %>%
    pull(name)
  reject_1pc <- stationarity_results %>%
    filter(reject_1pc) %>%
    pull(name)
  
  # Print messages
  if (length(cannot_reject) > 0) {
    message("Cannot reject the null hypothesis for: ", paste(cannot_reject, collapse = ", "))
  }
  if (length(reject_10pc) > 0) {
    message("Evidence of stationarity at the 10% level for: ", paste(reject_10pc, collapse = ", "))
  }
  if (length(reject_5pc) > 0) {
    message("Evidence of stationarity at the 5% level for: ", paste(reject_5pc, collapse = ", "))
  }
  if (length(reject_1pc) > 0) {
    message("Evidence of stationarity at the 1% level for: ", paste(reject_1pc, collapse = ", "))
  }
  
  # Determine overall conclusion
  if (length(cannot_reject) == nrow(stationarity_results)) {
    message("There is no evidence of stationarity in any of these variables.")
  } else {
    unique_rejection_levels <- list(reject_10pc, reject_5pc, reject_1pc) %>%
      map(~ sort(.)) %>%
      discard(~ length(.) == 0) %>%
      unique()
    
    if (length(unique_rejection_levels) == 1) {
      message("These variables all support stationarity at the ",
              ifelse(length(reject_1pc) > 0, "1%", ifelse(length(reject_5pc) > 0, "5%", "10%")),
              " significance level.")
    } else {
      message("The variables do not support stationarity at the same significance level.")
    }
  }
  
  return(stationarity_results)
}











# Example usage with default test type (drift)

stationarity_results <- stationarity_tests(model_3_data, diff_order = 0)
# print(stationarity_results)

# Example with custom test types

# test_types <- list(gdp = "trend", rnu = "none", cpi = "drift")
# 
# stationarity_results_custom <- stationarity_tests(model_1_data, test_types)
# 
# print(stationarity_results_custom)
  







