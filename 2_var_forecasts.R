################################################################################
################################### SET UP #####################################
################################################################################

library(tidyverse)
library(urca)

setwd("C:/Users/gtd/OneDrive - Treasury/Documents/var_project")

################################################################################
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






var=exp(-1*(-0.391648*(LOG(MEGCON/CNR)+0.217492*(LOG(PMEGCON*(1+RTCDMEG)/PCNR))-0.010937*TREND+0.007503*TRENDBR_2008Q4+4.820867)))*cnr











model_1_data <- model_1_data %>% 
  group_by(name) %>% 
  mutate(value = value - lag(value)) %>% 
  na.omit()

ggplot(model_1_data, aes(date, value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free")

# These differenced series definitely look better


# GDP --------------------------------------------------------------------------

gdp_d <- model_1_data %>% 
  filter(name == "gdp") %>% 
  pull()

plot(gdp_d, type = "l")

adf_gdp_d <- ur.df(gdp_d, type = "drift")

summary(adf_gdp_d)


# Unemployment rate ------------------------------------------------------------


