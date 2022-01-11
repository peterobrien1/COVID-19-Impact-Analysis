#### Peter O'Brien 
### Bell Lab
## COVID-19 & County Data Merge 
# September 11, 2021 


# Housekeeping #
rm(list = ls())
options(scipen = 999)
here::i_am("SCRIPTS/Loading_Cleaning/COVID_CLEAN_MERGE2.R") # Establish relative wd
library(tidyverse)
library(lubridate)
library(zoo)
library(lorenz)
library(scales)
library(ggthemes)
library(beepr)
library(here)


# This script pulls in csv files contained within the BELL_RESEARCH project directory related to COVID-19 caseloads and reported deaths, 
# as well as county population, typology, and unemployment data 
# The infection rate metrics are calculated using the same methodology as in Cho et. al (2021) "Choices" study




# Load data #
df_covid_cases <- read_csv(here("DATA/COVID/covid_confirmed_usafacts.csv")) # Source: USAFACTS
df_covid_deaths <- read_csv(here("DATA/COVID/covid_deaths_usafacts.csv"))
df_vaccines_raw <- read_csv(here("DATA/COVID/vaccinations_state_level_Nov21.csv")) 
df_county_pop <- read_csv(here("DATA/COVID/US_COUNTY_POPULATIONS.csv")) 
df_county_urban_rural <- read_csv(here("DATA/RECREATION/ERS_RURAL_URBAN_CODES_US.csv")) # Source: USDA ERS
df_county_typology <- read_csv(here("DATA/RECREATION/ERS_COUNTY_TYPOLOGY_US_2015.csv"))
df_land_area <- read_csv(here("DATA/RECREATION/land_cnty_data.csv")) # Source: Kathleen?
df_acs_raw <- read_csv(here("DATA/Demographic/nhgis_2015_2019_county.csv")) # Source: US Census
df_med_inc <- read_csv(here("DATA/Demographic/nhgis_med_inc_15_19.csv"))
aggregate_inc <- read_csv(here("DATA/Demographic/nhgis_mean_inc.csv"))
state_abb <- read_csv(here("DATA/HEALTH/state_abb.csv")) # List of state abbreviations to join with state names (makes census region grouping easier)
state_fips <- read_csv(here("DATA/COVID/us-state-ansi-fips.csv"))




df_covid_cases$countyFIPS <- str_pad(as.character(df_covid_cases$countyFIPS), 5, pad = "0") # Add leading zeros
df_covid_deaths$countyFIPS <- str_pad(as.character(df_covid_deaths$countyFIPS), 5, pad = "0")
df_county_pop$countyFIPS <- str_pad(as.character(df_county_pop$countyFIPS), 5, pad = "0")
df_county_typology$FIPStxt <- str_pad(as.character(df_county_typology$FIPStxt), 5, pad = "0")


df_covid_cases_long <- df_covid_cases %>%
  mutate(FIPS = as.character(countyFIPS)) %>% 
  pivot_longer(cols = `2020-01-22`:`2021-11-10`, # Convert wide > long
               names_to = "Date",
               values_to = "cum_cases") 

df_covid_deaths_long <- df_covid_deaths %>% 
  mutate(FIPS = as.character(countyFIPS)) %>% 
  pivot_longer(cols = `2020-01-22`:`2021-11-10`,
               names_to = "Date",
               values_to = "cum_cases") 




# Merge covid & county population data #


pop_join <- function(df1, df2){ # Reusable function to merge with county population data
  dfnew <- df1 %>%
    inner_join(df2,
               by = c("County Name", "countyFIPS", "State")) %>%
    select(State, `County Name`, Date, cum_cases, population, FIPS) %>% # Drop unnecessary columns
    rename(county = `County Name`) %>%
    filter(county != "Statewide Unallocated") # DOUBLE CHECK THIS
  return(dfnew)
}

df_covid_cases_long2 <- pop_join(df1 = df_covid_cases_long, 
                                 df2 = df_county_pop)
df_covid_deaths_long2 <- pop_join(df1 = df_covid_deaths_long, 
                                  df2 = df_county_pop)






# Calculate new cases per month (monthly infection rate) #


aggregate_cases <- function(df){
  df <- df %>%
    mutate(year = if_else(str_detect(Date, "2020"), # Extract month and year info from date variable
                          2020,
                          2021),
           month_num = lubridate::month(as.Date(Date, format = "%Y-%m-%d")), # 'lubridate' function call to extract month number
           month = factor(month.abb[month_num], levels = month.abb)) %>% 
    group_by(FIPS) %>% 
    mutate(new_cases = c(cum_cases[1], diff(cum_cases))) %>% # calculate new cases per day 
    ungroup() %>% 
    group_by(FIPS, month, month_num, year, population, county, State) %>% 
    summarise(month_new_case_total = sum(new_cases)) # calculate total new cases per month
  return(df)
}


df_covid_cases_bymonth <- aggregate_cases(df_covid_cases_long2)
df_covid_deaths_bymonth <- aggregate_cases(df_covid_deaths_long2)





# Calculate per capita metrics #
df_covid_cases_bymonth$date <- zoo::as.yearmon(paste(df_covid_cases_bymonth$year, 
                                                     df_covid_cases_bymonth$month_num), "%Y %m") # `zoo` function call to recreate date object
df_covid_deaths_bymonth$date <- zoo::as.yearmon(paste(df_covid_deaths_bymonth$year, 
                                                      df_covid_deaths_bymonth$month_num), "%Y %m")
class(df_covid_cases_bymonth$date) # Check variable class

percap <- function(df){
  df <- df %>%
    mutate(new_cases_percap = round((month_new_case_total / population), digits = 4),
           new_per_100k = new_cases_percap * 100000) # Per cap values are too small to be useful --> per 100,000 is commonly used
  return(df)
}

df_covid_cases_percap <- percap(df_covid_cases_bymonth)
df_covid_deaths_percap <- percap(df_covid_deaths_bymonth)





# Initial plots of data to look for anomalies #

# Look at monthly infection rate trends in MAINE as a good reference/sanity check
# df_covid_cases_percap %>%
#   filter(State == "ME") %>%
#   ggplot(aes(x = as.Date(date),
#              y = new_per_100k,
#              col = county)) +
#   geom_line() +
#   scale_x_date(date_breaks = "4 month",
#                labels = date_format("%b-%Y")) +
#   labs(y = "New Cases Per 100k Residents", x = "Date", col = "State") +
#   ggtitle("COVID-19 Monthly Infection Rate in ME Counties", subtitle = "January 2020 - August 2021") +
#   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
#   theme_minimal() # Numbers & trends look reasonable





# What about all New England States?
NE_states <- c("ME", "MA", "VT", "NH", "RI", "CT")
# df_covid_cases_percap %>%
#   filter(date != "Sep 2021") %>%
#   group_by(State, date) %>%
#   summarize(total_population = sum(population),
#             total_cases = sum(month_new_case_total)) %>%
#   mutate(total_cases_per_100k = (total_cases / total_population) * 100000) %>%
#   ggplot(aes(x = as.Date(date),
#              y = total_cases_per_100k,
#              col = State)) +
#   geom_line() +
#   scale_x_date(date_breaks = "4 month",
#                labels = date_format("%b-%Y")) +
#   labs(y = "New Cases Per 100k Residents", x = "Date", col = "State") +
#   ggtitle("COVID-19 Monthly Infection Rate in US States", subtitle = "January 2020 - August 2021") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))






# Merge cases & deaths dfs & with county metro/nonmetro data

df_covid_cases_percap2 <- df_covid_cases_percap %>% 
  rename(new_cases_per100k = new_per_100k) # Uniquely identify column before merge

df_covid_deaths_percap2 <- df_covid_deaths_percap %>% 
  rename(new_deaths_per100k = new_per_100k,
         month_new_death_total = month_new_case_total,
         new_deaths_percap = new_cases_percap) 

# Join
df_covid_full <- df_covid_cases_percap2 %>% 
  inner_join(df_covid_deaths_percap2, by = c("FIPS", "county", "State", "month", "month_num", "year", "population", "date"))


df_county_urban_rural <- df_county_urban_rural %>% 
  rename(county = County_Name,
         type = Description) %>% 
  select(FIPS, county, type) # Prep for merge w/ covid data

df_covid_full2 <- df_covid_full %>%    # Note: some obs were dropped here. WHY?? 
  inner_join(df_county_urban_rural, by = c("FIPS", "county")) %>% 
  mutate(county_type = if_else(str_detect(type, "Non"), 
                               "Nonmetro",
                               "Metro"))


rm(df_covid_cases, df_covid_deaths, df_covid_cases_bymonth, df_covid_cases_percap, df_covid_cases_percap2, 
   df_covid_deaths_percap, df_covid_deaths_percap2) # Clearing up space



# Merge with recreation typology data #
df_county_typology <- df_county_typology %>% 
  rename(county = County_name,
         FIPS = FIPStxt)              # Prep for merge with covid data
df_county_typology <- df_county_typology %>% 
  select(FIPS, State, county, Economic_Type_Label) 


df_covid_full3 <- inner_join(df_county_typology, df_covid_full2, by = c("State", "FIPS", "county"))

rm(df_covid_full2, df_covid_full)


# make categories metro & rec, nonmetro & rec, nonrec & metro, nonrec & nonmetro
df_covid_full3 <- df_covid_full3 %>% 
  mutate(recreation = if_else(Economic_Type_Label == "Recreation",
                              "Recreation",
                              "Non-Recreation"))


df_covid_full3 <- df_covid_full3 %>% 
  mutate(designation = if_else(county_type == "Metro" & recreation == "Recreation", 
                               "Metro recreation",
                               if_else(county_type == "Metro" & recreation == "Non-Recreation",
                                       "Metro non-recreation",
                                       if_else(county_type == "Nonmetro" & recreation == "Recreation",
                                               "Nonmetro recreation",
                                               if_else(county_type == "Nonmetro" & recreation == "Non-Recreation",
                                                       "Nonmetro non-Recreation",
                                                       "Other")))))

# Group into census regions
west_sc <- c("OK", "AR", "LA", "TX")
west_nc <- c("ND", "MN", "SD", "NE", "IA", "KS", "MO")
east_sc <- c("KY", "TN", "MS", "AL")
south_atl <- c("DC", "MD", "DE", "WV", "VA", "NC", "SC", "GA", "FL") 
east_nc <- c("MI", "WI", "IL", "IN", "OH")
mid_atl <- c("PA", "NY", "NJ")
pacific <- c("AK", "HI", "WA", "OR", "CA")
mountain <- c("MT", "ID", "WY", "CO", "UT", "AZ", "NM")
# Already defined new england above


df_covid_full4 <- df_covid_full3 %>% 
  mutate(census_region = if_else(State %in% west_nc, "West North Central",
                                 if_else(State %in% west_sc, "West South Central",
                                         if_else(State %in% east_sc, "East South Central",
                                                 if_else(State %in% east_nc, "East North Central",
                                                         if_else(State %in% south_atl, "South Atlantic",
                                                                 if_else(State %in% mid_atl, "Middle Atlantic",
                                                                         if_else(State %in% pacific, "Pacific",
                                                                                 if_else(State %in% mountain, "Mountain",
                                                                                         if_else(State %in% NE_states, "New England",
                                                                                                 "NA"))))))))))

# write_csv(df_covid_full4, 
#          "DATA/MERGED_DATA/us_counties_data_full_V2.csv") # Export cleaned data




# Calculate covid summary statistics by census region and cnty type


length(unique(df_covid_full4$date))

total_cases_total_deaths_census <- df_covid_full4 %>% 
  group_by(census_region) %>% 
  summarise(total_pop = sum(population) / 23,  # Remember to divide by number of time periods in sample to avoid summing multiple times # beentheredonethat
            total_cases = sum(month_new_case_total),
            total_cases_per_100k = (total_cases / total_pop) * 100000,
            total_deaths = sum(month_new_death_total),
            total_deaths_per_100k = (total_deaths / total_pop) * 100000) 
total_cases_total_deaths_census <- na.omit(total_cases_total_deaths_census)


total_cases_total_deaths_designation <- df_covid_full4 %>% 
  group_by(designation) %>% 
  summarise(total_pop = (sum(population)) / 23, 
            total_cases = sum(month_new_case_total),
            total_cases_per_100k = (total_cases / total_pop) * 100000,
            total_deaths = sum(month_new_death_total),
            total_deaths_per_100k = (total_deaths / total_pop) * 100000) 
total_cases_total_deaths_designation <- na.omit(total_cases_total_deaths_designation)

# Check metro vs. nonmetro as well
total_cases_total_deaths_metro <- df_covid_full4 %>% 
  group_by(county_type) %>% 
  summarise(total_pop = (sum(population)) / 23, 
            total_cases = sum(month_new_case_total),
            total_cases_per_100k = (total_cases / total_pop) * 100000,
            total_deaths = sum(month_new_death_total),
            total_deaths_per_100k = (total_deaths / total_pop) * 100000) 
total_cases_total_deaths_metro <- na.omit(total_cases_total_deaths_metro)




total_us_population <- sum(total_cases_total_deaths_census$total_pop) # 2019 Census estimate
total_us_deaths <- sum(total_cases_total_deaths_census$total_deaths) # as of August 2021
total_us_cases = sum(total_cases_total_deaths_census$total_cases)

(total_us_cases / total_us_population) * 100000 # total U.S. cases per 100,000
(total_us_deaths / total_us_population) * 100000 # total U.S. deaths per 100,000







# Prep ACS data #
df_acs <- df_acs_raw %>% 
  select(YEAR, STUSAB, STATEA, COUNTYA, # location identifiers
         ALT0E001:ALT0E049,     # Age breakdown by sex
         ALUDE001:ALUDE009,     # Race breakdown
         ALWGE001:ALWGE025, # Education attainment
         ALW0E001:ALW0E017, # Income breakdown
         ALZHE001:ALZHE055, # Employment by industry by sex
         ALZJE001, # Total housing units
         ALZKE001:ALZKE003) # Occupancy status

df_acs <- df_acs %>% 
  mutate(FIPS = paste(as.character(STATEA), as.character(COUNTYA), sep=""))


# Merge w/ rec & metro data
df_acs <- df_acs %>% 
  rename(State = STUSAB)

df_acs_1 <- df_acs %>% 
  left_join(df_county_typology, by = c("FIPS", "State")) 

df_acs_merged <- df_acs_1 %>% 
  left_join(df_county_urban_rural, by = c("FIPS", "county")) 

rm(df_acs, df_acs_1)


# make categories metro & rec, nonmetro & rec, nonrec & metro, nonrec & nonmetro
df_acs_merged <- df_acs_merged %>% 
  mutate(county_type = if_else(str_detect(type, "Non"), 
                               "Nonmetro",
                               "Metro"), 
         recreation = if_else(Economic_Type_Label == "Recreation",
                              "Recreation",
                              "Non-Recreation"))


# Assign census region designations
df_acs_merged <- df_acs_merged %>% 
  mutate(designation = if_else(county_type == "Metro" & recreation == "Recreation", 
                               "Metro recreation",
                               if_else(county_type == "Metro" & recreation == "Non-Recreation",
                                       "Metro non-recreation",
                                       if_else(county_type == "Nonmetro" & recreation == "Recreation",
                                               "Nonmetro recreation",
                                               if_else(county_type == "Nonmetro" & recreation == "Non-Recreation",
                                                       "Nonmetro non-Recreation",
                                                       "Other")))))

# Group into census regions

df_acs_merged <- df_acs_merged %>% 
  mutate(census_region = if_else(State %in% west_nc, "West North Central",
                                 if_else(State %in% west_sc, "West South Central",
                                         if_else(State %in% east_sc, "East South Central",
                                                 if_else(State %in% east_nc, "East North Central",
                                                         if_else(State %in% south_atl, "South Atlantic",
                                                                 if_else(State %in% mid_atl, "Middle Atlantic",
                                                                         if_else(State %in% pacific, "Pacific",
                                                                                 if_else(State %in% mountain, "Mountain",
                                                                                         if_else(State %in% NE_states, "New England",
                                                                                                 "NA"))))))))))





# Caculate housing metrics
df_acs_merged <- df_acs_merged %>% 
  mutate(num_housing = as.numeric(ALZKE001),
         p_vacant = round(as.numeric(ALZKE003) / as.numeric(ALZKE001), digits = 3)) # Proportion vacant


acs <- df_acs_merged %>% 
  mutate(population = as.numeric(ALT0E001)) # create population variable


# Need to convert county data to numeric
acs[,5:163] <- sapply(acs[,5:163], as.numeric)



# Calulate proportion of pop under 18
acs <- acs %>% 
  mutate(under_18 = round((rowSums(select(.,`ALT0E003`:`ALT0E006`), na.rm = T) + rowSums(select(.,`ALT0E027`:`ALT0E030`), na.rm = T)) / population, digits = 4))

# Proportion 18-64
acs2 <- acs %>% 
  mutate(ages_18_64 = round((rowSums(select(.,`ALT0E007`:`ALT0E019`), na.rm = T) + rowSums(select(.,`ALT0E031`:`ALT0E043`), na.rm = T)) / population, digits = 4))  


# Proportion 65 and older
acs2 <- acs2 %>% 
  mutate(ages_65_up = round((rowSums(select(.,`ALT0E020`:`ALT0E025`), na.rm = T) + rowSums(select(.,`ALT0E044`:`ALT0E049`), na.rm = T)) / population, digits = 4))  


# Calculate income breakdowns
acs3 <- acs2 %>% 
  mutate(under_25k = round((rowSums(select(.,ALW0E002:ALW0E005), na.rm = T)) / ALW0E001, digits = 4)) 

acs3 <- acs3 %>% 
  mutate(`25k_250k`= round((rowSums(select(.,ALW0E006:ALW0E016), na.rm = T)) / ALW0E001, digits = 4),
         above_200k = round((rowSums(select(.,ALW0E017), na.rm = T)) / ALW0E001, digits = 4))

# Median income
# Need to main acs df with median income df
df_med_inc <- df_med_inc %>% 
  mutate(FIPS = paste(as.character(STATEA), as.character(COUNTYA), sep="")) %>% 
  select(AL6NE001, FIPS) %>% 
  rename(median_inc = AL6NE001)
df_med_inc$median_inc <- as.numeric(df_med_inc$median_inc)

acs4 <- inner_join(acs3, df_med_inc) 
rm(acs, acs2, acs3, df_acs_merged)




# Calculate mean income
aggregate_inc <- aggregate_inc %>% 
  mutate(FIPS = paste(as.character(STATEA), as.character(COUNTYA), sep="")) %>% 
  select(ALYFE001, FIPS) %>% 
  rename(aggregate_inc = ALYFE001)
aggregate_inc$aggregate_inc <- as.numeric(aggregate_inc$aggregate_inc)

acs5 <- inner_join(acs4, aggregate_inc) 
acs5$aggregate_inc <- as.numeric(acs5$aggregate_inc)

acs5 <- acs5 %>% 
  mutate(mean_inc = round((aggregate_inc / ALW0E001), digits = 4)) # mean income (ALW0E001 is the number of households in each county)



# Proportion with college degree (associates) or higher
acs5 <- acs5 %>% 
  mutate(college_degree_more = round(rowSums(select(.,`ALWGE021`:`ALWGE025`), na.rm = T) / `ALWGE001`, digits = 4)) 


# Calculate proportions of individuals identifying as given races/ethnicities (note: they ONLY identify as white, af amer, asian, etc.)
# Hispanic not available (race vs. ethnicity??)
acs5 <- acs5 %>% 
  mutate(white_only = round(rowSums(select(.,`ALUDE003`), na.rm = T) / `ALUDE001`, digits = 4),
         african_amer = round(rowSums(select(.,`ALUDE004`), na.rm = T) / `ALUDE001`, digits = 4),
         asian = round(rowSums(select(.,`ALUDE006`), na.rm = T) / `ALUDE001`, digits = 4),
         native_amer = round(rowSums(select(.,`ALUDE005`), na.rm = T) / `ALUDE001`, digits = 4))








#install.packages("devtools")
#devtools::install_github("datadiarist/lorenz")


# Calculate gini for each cnty designation using mean-constrained integration over brackets (Technique developed by Jargowsky & Wheeler, 2018)
cnty_type_inc <- acs5 %>% 
  group_by(designation) %>% 
  summarise(under_10k = sum(ALW0E002), # Sum the number of households in each income bracket in 
            `10k_15k` = sum(ALW0E003),
            `15k_20k` = sum(ALW0E004),
            `20k_25k` = sum(ALW0E005),
            `25k_30k` = sum(ALW0E006),
            `30k_35k` = sum(ALW0E007),
            `35k_40k` = sum(ALW0E008),
            `40k_45k` = sum(ALW0E009),
            `45k_50k` = sum(ALW0E010),
            `50k_60k` = sum(ALW0E011),
            `60k_75k` = sum(ALW0E012),
            `75k_100k` = sum(ALW0E013),
            `100k_125k` = sum(ALW0E014),
            `125k_150k` = sum(ALW0E015),
            `150k_200k` = sum(ALW0E016),
            over_200k = sum(ALW0E017),
            total_households = sum(ALW0E001),
            total_inc = sum(aggregate_inc),
            average_inc = total_inc / total_households) # calculate mean income for each cnty type

cnty_type_inc <- na.omit(cnty_type_inc)

cnty_type_inc_long <- cnty_type_inc %>% 
  pivot_longer(under_10k:over_200k, names_to = "bracket", values_to = "num_households") # wide > long

frequencies_metro_nonrec <- as.vector(filter(cnty_type_inc_long, designation == "Metro non-recreation") %>% select(num_households)) # Frequency of households in each inc bracket by cnty type
frequencies_metro_rec <- as.vector(filter(cnty_type_inc_long, designation == "Metro recreation") %>% select(num_households))
frequencies_nonmetro_rec <- as.vector(filter(cnty_type_inc_long, designation == "Nonmetro recreation") %>% select(num_households))
frequencies_nonmetro_nonrec <- as.vector(filter(cnty_type_inc_long, designation == "Nonmetro non-Recreation") %>% select(num_households))


boundaries <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
                100000, 125000, 150000, 200000) # Vector of income brackets in data
boundaries <- data.frame(boundaries) # Need vectors in same data format

gini_metro_nonrec <- mcib(freqs = frequencies_metro_nonrec$num_households, bounds = boundaries$boundaries, mean = 95812.95) # Uses 'lorenz' package
gini_metro_rec <- mcib(freqs = frequencies_metro_rec$num_households, bounds = boundaries$boundaries, mean = 96404.20)
gini_nonmetro_nonrec <- mcib(freqs = frequencies_nonmetro_nonrec$num_households, bounds = boundaries$boundaries, mean = 66270.63)
gini_nonmetro_rec <- mcib(freqs = frequencies_nonmetro_rec$num_households, bounds = boundaries$boundaries, mean = 76893.69)

# NOTE: U.S. coefficient is 0.48 in 2020 according to Statista









# Calculate Gini coefficients by census region #


census_region_inc <- acs5 %>% 
  group_by(census_region) %>% 
  summarise(under_10k = sum(ALW0E002), # Sum the number of households in each income bracket in 
            `10k_15k` = sum(ALW0E003),
            `15k_20k` = sum(ALW0E004),
            `20k_25k` = sum(ALW0E005),
            `25k_30k` = sum(ALW0E006),
            `30k_35k` = sum(ALW0E007),
            `35k_40k` = sum(ALW0E008),
            `40k_45k` = sum(ALW0E009),
            `45k_50k` = sum(ALW0E010),
            `50k_60k` = sum(ALW0E011),
            `60k_75k` = sum(ALW0E012),
            `75k_100k` = sum(ALW0E013),
            `100k_125k` = sum(ALW0E014),
            `125k_150k` = sum(ALW0E015),
            `150k_200k` = sum(ALW0E016),
            over_200k = sum(ALW0E017),
            total_households = sum(ALW0E001),
            total_inc = sum(aggregate_inc),
            average_inc = total_inc / total_households) # calculate mean income for each cnty type

census_region_inc <- na.omit(census_region_inc)


census_region_inc_long <- census_region_inc %>% 
  pivot_longer(under_10k:over_200k, names_to = "bracket", values_to = "num_households") # wide > long



freq_east_north_central <- as.vector(filter(census_region_inc_long, census_region == "East North Central") %>% select(num_households)) # Frequency of households in each inc bracket by census region
freq_east_south_central <- as.vector(filter(census_region_inc_long, census_region == "East South Central") %>% select(num_households))
freq_mid_atlantic <- as.vector(filter(census_region_inc_long, census_region == "Middle Atlantic") %>% select(num_households))
freq_mtn <- as.vector(filter(census_region_inc_long, census_region == "Mountain") %>% select(num_households))
freq_NE <- as.vector(filter(census_region_inc_long, census_region == "New England") %>% select(num_households))
freq_pacific <- as.vector(filter(census_region_inc_long, census_region == "Pacific") %>% select(num_households))
freq_south_atl <- as.vector(filter(census_region_inc_long, census_region == "South Atlantic") %>% select(num_households))
freq_west_north_central <- as.vector(filter(census_region_inc_long, census_region == "West North Central") %>% select(num_households))
freq_west_south_central <- as.vector(filter(census_region_inc_long, census_region == "West South Central") %>% select(num_households))




gini_east_nc <- mcib(freqs = freq_east_north_central$num_households, bounds = boundaries$boundaries, mean = 83528.21) 
gini_east_sc <- mcib(freqs = freq_east_south_central$num_households, bounds = boundaries$boundaries, mean = 73192.27) 
gini_mid_atl <- mcib(freqs = freq_mid_atlantic$num_households, bounds = boundaries$boundaries, mean = 101650.86) 
gini_mtn <- mcib(freqs = freq_mtn$num_households, bounds = boundaries$boundaries, mean = 87590.73) 
gini_NE <- mcib(freqs = freq_NE$num_households, bounds = boundaries$boundaries, mean = 107143.38) 
gini_pacific <- mcib(freqs = freq_pacific$num_households, bounds = boundaries$boundaries, mean = 107424.17) 
gini_south_atl <- mcib(freqs = freq_south_atl$num_households, bounds = boundaries$boundaries, mean = 89322.85) 
gini_west_nc <- mcib(freqs = freq_west_north_central$num_households, bounds = boundaries$boundaries, mean = 83993.98) 
gini_west_sc <- mcib(freqs = freq_west_south_central$num_households, bounds = boundaries$boundaries, mean = 85694.32) 



# Summarize income across county types and census regions
# Could make function at later date.....
census_inc_summary <- acs5 %>% 
  group_by(census_region) %>% 
  summarise(median_income = mean(median_inc),
            under_25k = mean(under_25k),
            over_200k = mean(above_200k))  # average median inc across all cnties in each region

census_inc_summary <- na.omit(census_inc_summary)
mean(census_inc_summary$median_income) # U.S. average median income
mean(census_inc_summary$under_25k)
mean(census_inc_summary$over_200k)



cnty_type_inc_summary <- acs5 %>% 
  group_by(designation) %>% 
  summarise(median_income = mean(median_inc),
            under_25k = mean(under_25k),
            over_200k = mean(above_200k))  # average median inc across all cnties in each region

cnty_type_inc_summary <- na.omit(cnty_type_inc_summary)


acs_summary_census <- acs5 %>% 
  select(FIPS:native_amer) %>% 
  group_by(census_region) %>% 
  summarise(prop_college_degree = mean(college_degree_more),
            prop_white = mean(white_only),
            prop_af_amer = mean(african_amer),
            prop_asian = mean(asian),
            under_18 = mean(under_18),
            over_64 = mean(ages_65_up),
            prop_vacant = mean(p_vacant))

acs_summary_designation <- acs5 %>% 
  select(FIPS:native_amer) %>% 
  group_by(designation) %>% 
  summarise(prop_college_degree = mean(college_degree_more),
            prop_white = mean(white_only),
            prop_af_amer = mean(african_amer),
            prop_asian = mean(asian),
            under_18 = mean(under_18),
            over_64 = mean(ages_65_up),
            prop_vacant = mean(p_vacant))

mean(acs_summary_census$prop_college_degree, na.rm = T)
mean(acs_summary_census$prop_white, na.rm = T)
mean(acs_summary_census$prop_af_amer, na.rm = T)
mean(acs_summary_census$prop_asian, na.rm = T)
mean(acs_summary_census$under_18, na.rm = T)
mean(acs_summary_census$over_64, na.rm = T)
mean(acs_summary_census$prop_vacant, na.rm = T)


# Join w/covid data to create regression data
acs6 <- acs5 %>% 
  select(FIPS:native_amer) %>% 
  select(FIPS, county, census_region, county_type, recreation, median_inc, college_degree_more, ages_18_64, p_vacant, white_only) %>% 
  rename(proportion_white = white_only) %>% 
  filter(FIPS != "State CodeCounty Code")


df_covid_reg <- df_covid_full4 %>% 
  group_by(FIPS, county, date, population) %>% 
  summarise(total_cases = sum(month_new_case_total),
            cumulative_cases_per_cap = total_cases / population) %>% 
  group_by(FIPS, population) %>% 
  summarise(cumulative_cases_per_1k = round((sum(cumulative_cases_per_cap) * 1000), digits = 5)) %>%  # Get cumulative cases per 1k residents for each county Jan 2020-Nov 2021
  mutate(cumulative_cases_per_1k = if_else(cumulative_cases_per_1k == 0, cumulative_cases_per_1k + 0.000001, cumulative_cases_per_1k),
         ln_cum_cases_per_cap = log(cumulative_cases_per_1k)) # Ln transformation

hist(df_covid_reg$ln_cum_cases_per_cap)
hist(df_covid_reg$cumulative_cases_per_1k)


# Join w/ ACS data by FIPS, county
regression_data <- inner_join(df_covid_reg, acs6, by = "FIPS")

# Join with vaccination data (by state)
df_vaccines <- df_vaccines_raw %>% # Prep data
  select(1,3) %>%
  rename(state = `State/Territory/Federal Entity`, 
         doses_per_100k = `Doses Delivered per 100K`) %>% 
  filter(doses_per_100k != "N/A") %>% 
  mutate(doses_per_100k = as.numeric(doses_per_100k))
  
regression_data <- regression_data %>% 
  mutate(state_code = str_extract(FIPS, "^.{2}")) # grab state FIPS code from combined FIPS code column

# Consolidate this
state_fips$st <- str_pad(as.character(state_fips$st), 2, pad = "0") # Add leading zeros to fips code
state_fips <- rename(state_fips, state = stname)
state_fips <- rename(state_fips, state_code = st) # prep for merge
state_fips <- select(state_fips, state, state_code)

regression_data <- regression_data %>% 
  inner_join(state_fips, by = "state_code")


# Join reg data with vaccine data
regression_data <- regression_data %>% 
  inner_join(df_vaccines, by = "state")
hist(regression_data$doses_per_100k)


# Bring in land area data
land_df <- df_land_area %>% 
  select(CNTY5FIP_NUM, ALAND10_SQMI, PPROT) %>%  # Get land area & proportion conserved land variables
  rename(FIPS = CNTY5FIP_NUM)
land_df$FIPS <- str_pad(as.character(land_df$FIPS), 5, pad = "0") # Add leading zeros to merge with reg data

regression_data2 <- regression_data %>% 
  inner_join(land_df, by = "FIPS") # Join land area data with reg data
# Note that proportion land conserved is not available for Alaska & Hawaii (23 cnties total)

regression_data2 <- regression_data2 %>% 
  mutate(pop_density = round((population / ALAND10_SQMI), digits = 5), # Calculate number of people per sq mile in each cnty
         PPROT = round(PPROT, digits = 4)) 


#write_csv(regression_data2,
#         "DATA/MERGED_DATA/covid_regression_data_V4.csv")


# mapping_data <- regression_data %>%
#   select(FIPS, cumulative_cases_per_1k, county_type, recreation)
# 
# write_csv(mapping_data,
#           "DATA/MERGED_DATA/cnty_mapping_data.csv")
beepr::beep(2) # Script is done


