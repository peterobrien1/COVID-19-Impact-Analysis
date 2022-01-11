#### Peter O'Brien 
### Bell Lab
## COVID-19 & County Data Merge 
# September 11, 2021 


# Housekeeping #
rm(list = ls())
options(scipen = 999)
here::i_am("SCRIPTS/Loading_Cleaning/LYME_CLEAN_MERGE.R") # Establish relative wd
library(tidyverse)
library(lubridate)
library(zoo)
library(lorenz)
library(scales)
library(ggthemes)
library(beepr)
library(here)

# Load in data
df_lyme <- read_csv(here("DATA/HEALTH/lyme_cnty_annual_00_18.csv"))
df_county_typology <- read_csv(here("DATA/RECREATION/ERS_COUNTY_TYPOLOGY_US_2015.csv"))
df_county_pop <- read_csv(here("DATA/COVID/US_COUNTY_POPULATIONS.csv")) # Source: USDA ERS
state_abb <- read_csv(here("DATA/HEALTH/state_abb.csv")) # List of state abbreviations to join with state names (makes census region grouping easier)



# Prep Lyme Data #
df_lyme <- read_csv(here("DATA/HEALTH/lyme_cnty_annual_00_18.csv"))
state_abb <- read_csv(here("DATA/HEALTH/state_abb.csv")) # List of state abbreviations to join with state names (makes census region grouping easier)
state_abb <- state_abb %>% 
  select(State, Code) %>% 
  rename(state_abb = Code)
df_lyme <- rename(df_lyme, State = Stname) # prep for join
df_lyme2 <- left_join(df_lyme, state_abb, by = "State")



# Group into census regions

df_lyme3 <- df_lyme2 %>% 
  mutate(census_region = if_else(state_abb %in% west_nc, "West North Central",
                                 if_else(state_abb %in% west_sc, "West South Central",
                                         if_else(state_abb %in% east_sc, "East South Central",
                                                 if_else(state_abb %in% east_nc, "East North Central",
                                                         if_else(state_abb %in% south_atl, "South Atlantic",
                                                                 if_else(state_abb %in% mid_atl, "Middle Atlantic",
                                                                         if_else(state_abb %in% pacific, "Pacific",
                                                                                 if_else(state_abb %in% mountain, "Mountain",
                                                                                         if_else(state_abb %in% NE_states, "New England", 
                                                                                                 "NA"))))))))))

df_lyme_long <- df_lyme3 %>% 
  pivot_longer(Cases2000:Cases2018, names_to = "Year", values_to = "Cases") %>% # Wide to long
  mutate(year = as.numeric(str_sub(Year, start = -4))) # Get year information from chr vector


# Merge w/ recreation county data (DITCH THIS BEFORE COPYING INTO MAIN SCRIPT)
df_county_typology <- df_county_typology %>% 
  rename(county = County_name,
         FIPS = FIPStxt)              # Prep for merge with covid data
df_county_typology <- df_county_typology %>% 
  select(FIPS, county, Economic_Type_Label)


# Create FIPS code for lyme data
df_lyme_long$STCODE <- str_pad(as.character(df_lyme_long$STCODE), 2, pad = "0")
df_lyme_long$CTYCODE <- str_pad(as.character(df_lyme_long$CTYCODE), 3, pad = "0")
df_lyme_long <- df_lyme_long %>% 
  rename(county = Ctyname) %>% 
  mutate(FIPS = paste(as.character(STCODE), as.character(CTYCODE), sep=""))



# Join county typology with lyme data
df_lyme_type <- inner_join(df_lyme_long, df_county_typology, by = c("FIPS", "county")) # double check why some obs dropped



df_lyme_type <- df_lyme_type %>% 
  mutate(recreation = if_else(Economic_Type_Label == "Recreation",
                              "Recreation",
                              "Non-Recreation"))

df_county_pop2 <- df_county_pop %>% 
  rename(county = `County Name`,
         FIPS = countyFIPS,
         state_abb = State)
# Join w/population data
df_lyme_type <- df_lyme_type %>% 
  inner_join(df_county_pop2,
             by = c("county","FIPS", "state_abb"))

#write_csv(df_lyme_type, "DATA/MERGED_DATA/lyme_cleaned.csv")

beepr::beep(2) # Script is done

