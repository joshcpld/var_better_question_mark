
################################################################################
################################### SET-UP #####################################
################################################################################

library(tidyverse)
library(readabs)
library(lubridate)

################################################################################
############################ SOURCING VARIABLE IDS #############################
################################################################################

series_ids <- tibble(
  
  variable = c(
    
    # Model 1
    
    "gdp", # Gross Domestic Product
    "cpi", # Inflation
    "rnu", # Unemployment rate
    
    # Model 2 additions
    
    "hfce", # Household consumption
    "priv_gfcf", # Private investment
    "pfd", # Public final demand
    "xgs", # Exports
    "mgs", # Imports
    "twi",
    "tot" # Terms of Trade

  ),
  
  series_id = c(
    
    # Model 1
    
    "A2304402X", # GDP
    "A2325846C", # inflation
    "A84423050A", # rnu
    
    # Model 2 additions
    
    "A2304081W", # hfce
    "A2304100T", # priv_gfcf
    "A124830485W", # pfd  
    "A2304114F", # xgs
    "A2304115J", # mgs
    "A3534043J", #twi 
    "A3534885C" # tot
    
  )
  
  ) %>% 
  
  mutate(release = case_when(
    
    variable == "cpi" ~ "cpi",
    
    variable %in% c("rnu") ~ "lfs",
    
    variable == "tot" ~ "bop",
    
    TRUE ~ "na"
    
  ))







################################################################################
################################## DATA DOWNLOAD ###############################
################################################################################

################################## NON-LFS DATA ################################

# Nataccs data

na_ids <- series_ids %>% filter(release == "na")

na_data <- map2_dfr(
  
  na_ids$series_id,
  na_ids$variable,
  ~{
    
    read_abs(series_id = .x, metadata = FALSE) %>%
      mutate(variable = .y) %>%
      select(date, value, variable) 
    
  }
  
) %>% 
  pivot_wider(names_from = variable)


# CPI  & BoP data


cpi_ids <- series_ids %>% filter(release == "cpi" | release == "bop")

cpi_data <- map2_dfr(
  
  cpi_ids$series_id,
  cpi_ids$variable,
  ~{
    
    read_abs(series_id = .x, metadata = FALSE) %>%
      mutate(variable = .y) %>%
      select(date, value, variable) 
    
  }
  
) %>% 
  pivot_wider(names_from = variable)

################################### LFS DATA ##################################

# Modern monthly LFS data 


lfs_ids <- series_ids %>% filter(release == "lfs")

lfs_data <- map2_dfr(
  
  lfs_ids$series_id,
  lfs_ids$variable,
  ~{
    
    read_abs(series_id = .x, metadata = FALSE) %>%
      mutate(variable = .y) %>%
      select(date, value, variable) 
    
  }
  
) %>% 
  na.omit()


# Convert to quarterly format

lfs_data <- lfs_data %>% 
  mutate(quarter = ceiling_date(date, "quarter")) %>% 
  mutate(quarter = quarter %m-% months(1)) %>% 
  group_by(quarter, variable) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  select(date = quarter, variable, value) %>% 
  filter(date > "1990-12-01")



# Import and convert historical lfs data

lfs_data_old <- read_csv("data/lfs_historical.csv") %>%
  mutate(date = dmy(date)) %>% 
  pivot_longer(-date, names_to = "variable") %>% 
  mutate(quarter = ceiling_date(date, "quarter")) %>% 
  mutate(quarter = quarter %m-% months(1)) %>% 
  group_by(quarter, variable) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  select(date = quarter, variable, value) %>% 
  na.omit() %>% 
  filter(date <= "1990-12-01")


# Apppending modern and old data


lfs_data <- lfs_data %>% 
  bind_rows(lfs_data_old) %>% 
  arrange(date) %>% 
  na.omit() %>% 
  pivot_wider(names_from = variable) %>%
  select(date, rnu)
  
  


################################################################################
################################ JOINING DATA ##################################
################################################################################

data <- na_data %>% 
  inner_join(lfs_data, by = "date") %>% 
  inner_join(cpi_data, by = "date")

write_csv(data, "data/data.csv")

