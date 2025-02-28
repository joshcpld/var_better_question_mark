################################################################################
################################### SET UP #####################################
################################################################################

library(tidyverse)

source("2_stationarity_function.R")

###############################################################################
############################### DATA IMPORT ####################################
################################################################################

data <- read_csv("data/data.csv")

################################################################################
############################ CHECKING MODEL STATIONARITY #######################
################################################################################

################################## DATAFRAMES ##################################

model_1_data <- data %>% 
  select(date,gdp,rnu,cpi)

model_2_data <- data %>% 
  na.omit()

######################## CREATING STATIONARY VARIABLES #########################

# model_1 ----------

model_1_data %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free")

# These series are clearly not stationary: therefore try differencing and doing unit root tests.

model_1_data %>% 
  pivot_longer(-date) %>% 
  group_by(name) %>% 
  mutate(value = value - lag(value)) %>% 
  na.omit() %>% 
  ggplot(aes(x = date, y = value, colour = name)) + 
  geom_line() + 
  facet_wrap(~name, scales = "free")

# These differenced series definitely look better





# model_2 ----------

model_2_data %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free")

# These series are clearly not stationary: therefore try differencing and doing unit root tests.

model_2_data %>% 
  pivot_longer(-date) %>% 
  group_by(name) %>% 
  mutate(value = value - lag(value)) %>% 
  na.omit() %>% 
  ggplot(aes(x = date, y = value, colour = name)) + 
  geom_line() + 
  facet_wrap(~name, scales = "free")

# These differenced series definitely look better




######################### TESTING STATIONARITY #################################

# First need to check none of the variables are I(0)

model_1_stationarity_results <- stationarity_tests(model_1_data, diff_order = 0)

model_2_stationarity_results <- stationarity_tests(model_2_data, diff_order = 0)



# Now we see if the variables are I(1)

model_1_stationarity_results <- stationarity_tests(model_1_data, diff_order = 1)

model_2_stationarity_results <- stationarity_tests(model_2_data, diff_order = 1)


# All of these variables in each model are stationary at the 1% level.
# As the variables in each model are I(1), it is appropriate to model them as a VAR.



