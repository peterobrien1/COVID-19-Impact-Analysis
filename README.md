# COVID-19-Impact-Analysis
Project Name: COVID-19 County Impacts

Author: Peter O'Brien (Bell Research Lab)

Last Edited: 12/1/2021


Description:
This project folder contains all relevant data, scripts, and outputs created during my fall 2021 research into the heterogeneous impacts of 
COVID-19 across county regions & typologies in the U.S. My primary research question is whether or not "recreation" counties were more
resillient than non-recreation counties to the adverse health outcomes resulting from the pandemic.

Contents:

Subfolders:
a) Loading_Cleaning
- "COVID_CLEAN_MERGE2.R" pulls in data containing covid case & death counts, county populations and typologies, land area, and ACS demographic 
characteristics. It outputs "covid_regression_data_final.csv" & "us_counties_data_full_V2.csv" which are used in "COVID_EDA_V1.Rmd"

- "LYME_CLEAN_MERGE.R" pulls in state level lyme disease data (annual) and preps for analysis. It outputs "lyme_cleaned.csv" which
is used in "LYME_EDA.Rmd"


b) Analysis
- "LYME_EDA.Rmd" creates several exploratory plots of lyme disease prevalance over time (state-level)

- "COVID_EDA_V1.Rmd" creates several exploratory plots of COVID-19 prevalance over time (county-level) & contains several
preliminary regression models. The output from this analysis can be found in OUTPUT.


Data Sources:
- USAFACTS
- US Census Bureau ACS 2015-2019
- Bureau of Economic Analysis
- USDA Economic Research Service

