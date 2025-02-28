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
  select(date, gdp, rnu, cpi, fce, gfcf, exports, imports)

model_3_data <- data %>% 
  select(date, gdp, cpi, exports, imports, hfce, bi, di, gfce, pub_gfcf, emp, unemp, twi) %>% 
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
  serial.test(lags.bg = 2, type = "BG")

# The p-value is very small. We cannot maintain the null hypothesis of there is not 1st/2nd order autocorrelation in the residuals.

# Let's try increasing the lags.

model_1_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=3, type = "const") %>% 
  serial.test(lags.bg = 2, type = "BG")

# P-value is still too small.

model_1_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=4, type = "const") %>% 
  serial.test(lags.bg = 1, type = "BG")

# After experimenting with different models, I land on VAR(7) for model 1.

model_1 <- model_1_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=7, type = "const")


summary(model_1)

################################ MODEL 2 #######################################


model_2_data %>% 
  dplyr::select(-date) %>% 
  VARselect(type = "const")

# It suggests starting at lag 1

model_2_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=1, type = "const") %>% 
  serial.test(lags.bg = 1, type = "BG")

# Tiny p-value, need larger lag.

model_2_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=7, type = "const") %>% 
  serial.test(lags.bg = 1, type = "BG")

# This result tells us we can maintain the null hypothesis of there being no 1st order autocorrelation in the residuals for a VAR(7)

model_2_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=20, type = "const") %>% 
  serial.test(lags.bg = 2, type = "BG")

# Unfortunately, we cannot maintain the hypothesis of there being no 1st/2ns order autocorrelation in the residuals in any VAR order.

# What does this suggest? 

model_2_residual_acf <- model_2_data %>% 
  dplyr::select(-date) %>% 
  VAR(p=7, type = "const") %>% 
  residuals() %>% 
  acf()

