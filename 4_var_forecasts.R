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

# I need to reshape the model data to produce level forecast efficiently

model_1_data_long <- model_1_data %>% 
  pivot_longer(-date)

# Actually producing and retrieving forecasts

model_1_fc_d <- map_dfc(predict(model_1, h = 12)$fcst, 
                        ~ as.data.frame(.x[, c("fcst", "lower", "upper")])) %>%
  set_names(rep(paste0(rep(names(predict(model_1, h = 12)$fcst), each = 3), c("_fcst", "_lower", "_upper")), length.out = ncol(.))) %>% 
  mutate(date = (max(data$date) %m+% months(3)) + months(rep(0:11, length.out = nrow(.)))) %>% 
  dplyr::select(date, everything()) %>% 
  pivot_longer(cols = -date,  
               names_to = c("name", "fc_type"),  
               names_pattern = "(.*)_(.*)",
               values_to = "value") %>% 
  mutate(step_ahead = 1:n()) %>% 
  mutate(fc_start_date = min(date))

# Turning these differenced forecasts into levels

model_1_fc <- model_1_fc_d %>% 
  bind_rows(model_1_data_long %>% filter(date == max(date))) %>% 
  arrange(date) %>% 
  group_by(name) %>% 
  mutate(level = cumsum(value)) %>% 
  ungroup() %>% 
  dplyr::select(date, name, fc_type, fc_start_date, step_ahead, value = level)

# Combining with actuals data to chart

model_1_data_w_fcast  <- model_1_data_long %>% 
  bind_rows(model_1_fc) %>% 
  mutate(fc_type = case_when(
    
    is.na(fc_type) ~ "actual",
    TRUE ~ fc_type
    
  ))




# These outputs are not centring around zero, which is causing an explosive series.

model_1_fc_d %>% 
  # filter(date > as.Date("2022-06-01")) %>% 
ggplot(aes(date, value, colour = fc_type)) + 
  geom_line() +
  facet_wrap(~name, scales = "free")


irf_result <- irf(model_1, impulse = "rnu", response = "cpi", n.ahead = 10, boot = TRUE)
plot(irf_result)




