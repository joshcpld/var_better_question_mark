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
############################ CHECKING MODEL STATIONARITY #######################
################################################################################

################################## DATAFRAMES ##################################

model_1_data <- data %>% 
  select(date,gdp,rnu,cpi)

model_2_data <- data %>% 
  select(date, gdp, rnu, cpi, fce, gfcf, exports, imports)

model_3_data <- data %>% 
  select(date, gdp, cpi, exports, imports, hfce, bi, di, gfce, pub_gfcf, emp, unemp, twi) %>% 
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






# model_3 ----------

model_3_data %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free")

# These series are clearly not stationary: therefore try differencing and doing unit root tests.

model_3_data %>% 
  pivot_longer(-date) %>% 
  group_by(name) %>% 
  mutate(value = value - lag(value)) %>% 
  na.omit() %>% 
  ggplot(aes(x = date, y = value, colour = name)) + 
  geom_line() + 
  facet_wrap(~name, scales = "free")

# These differenced series definitely look better


######################### TESTING STATIONARITY #################################

model_1_stationarity_results <- stationarity_tests(model_1_data, diff_order = 1)

model_2_stationarity_results <- stationarity_tests(model_2_data)

model_3_stationarity_results <- stationarity_tests(model_3_data)

# According to the ADF test, all variables share the same level of integration.
# Therefore, we can forecast produce forecasts using a VAR model.





################################################################################
############################### PRODUCING FORECASTS#############################
################################################################################


