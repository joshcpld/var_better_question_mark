################################################################################
################################### SET UP #####################################
################################################################################

library(tidyverse)
library(urca)

source("2_stationarity_function.R")

###############################################################################
############################### DATA IMPORT ####################################
################################################################################

data <- read_csv("data/data.csv")



################################################################################
################################# MODEL 1 ######################################
################################################################################

model_1_data <- data %>% 
  select(date,gdp,rnu,cpi)

######################## CREATING STATIONARY VARIABLES #########################

model_1_data %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free")

# These series are clearly not stationary: therefore try differencing and doing unit root tests.

model_1_data <- model_1_data %>%
  pivot_longer(-date) %>% 
  group_by(name) %>% 
  mutate(value = value - lag(value)) %>% 
  na.omit()

ggplot(model_1_data, aes(date, value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free")

# These differenced series definitely look better


######################### TESTING STATIONARITY #################################

test_types <- list(gdp = "drift", rnu = "none", cpi = "drift")

stationarity_results_custom <- stationarity_tests(model_1_data, test_types)
