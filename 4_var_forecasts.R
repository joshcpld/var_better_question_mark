################################################################################
################################### SET UP #####################################
################################################################################

library(tidyverse)

################################################################################
############################### DATA IMPORT ####################################
################################################################################

data <- read_csv("data/data.csv")

################################## DATAFRAMES ##################################

model_1_data <- data %>% 
  select(date,gdp,rnu,cpi)

model_2_data <- data %>% 
  na.omit()


# There are a few steps to selecting the optimal VAR model:
# 1. Ensure all your variables share the same order of integration but are not I(0). We have already done this.
# 2. Use the VARselect() function to suggests the lowest number of lags to start with.
# 3. Test this lag order suggestion for serial autocorrelation in the residuals.
# 4. Estimate the model with the given number of lags and ensure all coefficients are significantly < 1. 
# 5. Actually produce your forecast

library(vars) # We import this here because vars overrides dplyr's select function.

################################################################################
############################## MODEL SELECTION #################################
################################################################################

################################ MODEL 1 #######################################

model_1_data %>% 
  dplyr::select(-date) %>% 
  VARselect(type = "const")

# It suggests starting at lag 2

model_1_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=2, type = "const") %>% 
  serial.test(lags.bg = 2)

# The p-value is very small. We cannot maintain the null hypothesis of there is not 1st/2nd order autocorrelation in the residuals.

# Let's try increasing the lags.

model_1_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=3, type = "const") %>% 
  serial.test(lags.bg = 3)

# P-value large enough to maintain the null hypothesis.


model_1 <- model_1_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=3, type = "const")


summary(model_1)

################################ MODEL 2 #######################################


model_2_data %>% 
  dplyr::select(-c(date)) %>% 
  VARselect(type = "const")

# It suggests starting at lag 1

model_2_data %>% 
  dplyr::select(-c(date)) %>% 
  VAR(p=3, type = "const") %>% 
  serial.test(lags.bg = 3)

# We also find a VAR(3) is appropriate for this system as well.

model_2 <- model_2_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=3, type = "const")


summary(model_2)

