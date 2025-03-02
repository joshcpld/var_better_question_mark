################################################################################
################################### SET UP #####################################
################################################################################

library(tidyverse)
library(forecast)

################################################################################
############################### DATA IMPORT ####################################
################################################################################

data <- read_csv("data/data.csv")

################################## DATAFRAMES ##################################

model_1_data <- data %>% 
  select(date,gdp,rnu,cpi)

model_1_data_d <- model_1_data %>% 
  pivot_longer(-date) %>%
  group_by(name) %>% 
  mutate(value = value - lag(value)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  na.omit()


model_2_data <- data %>% 
  na.omit()

model_2_data_d <- model_2_data %>% 
  pivot_longer(-date) %>%
  group_by(name) %>% 
  mutate(value = value - lag(value)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  na.omit()


# There are a few steps to selecting the optimal VAR model:
# 1. Ensure all your variables share the same order of integration but are not I(0). We have already done this.
# 2. Use the VARselect() function to suggests the lowest number of lags to start with.
# 3. Test this lag order suggestion for serial autocorrelation in the residuals.
# 4. Estimate the model with the given number of lags and ensure all coefficients are significantly < 1. 

library(vars) # We import this here because vars overrides dplyr's select function.

################################################################################
############################## MODEL SELECTION #################################
################################################################################

################################ MODEL 1 #######################################

model_1_data_d %>% 
  dplyr::select(-date) %>% 
  VARselect(type = "const")

# It suggests starting at lag 1

model_1_data_d %>% 
  dplyr::select(-date) %>% 
  VAR(p=1, type = "const") %>% 
  serial.test()

# The p-value is large enough to maintain the null hypothesis of there being no autocorrelation in the residuals in a VAR(1).


model_1 <- model_1_data_d %>% 
  dplyr::select(-date) %>% 
  VAR(p=1, type = "const")


summary(model_1)

################################ MODEL 2 #######################################


model_2_data_d %>% 
  dplyr::select(-c(date)) %>% 
  VARselect(type = "const")

# It suggests starting at lag 1

model_2_data_d %>% 
  dplyr::select(-c(date)) %>% 
  VAR(p=2, type = "const") %>% 
  serial.test()

# We find a VAR(2) is appropriate for this system.

model_2 <- model_2_data_d %>% 
  dplyr::select(-date) %>% 
  VAR(p=2, type = "const")


summary(model_2)




################################################################################
################################# FORECASTS ####################################
################################################################################

detach("package:vars", unload = TRUE)

# I'm going to be producing 3 year-ahead forecasts. This equates to 12 quarters into the future.


################################### MODEL 1 ####################################



model_1_fcast <- forecast(model_1, h = 12)




