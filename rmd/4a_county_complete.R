# Open Intro County Complete Data Set
#Source:https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/  
  
#Then: https://www.openintro.org/data/?data=county_complete
  # The data prior to 2011 was from http://census.gov, though the exact page it came from is no longer available.
  # 
  # More recent data comes from the following sources.
  # 
  # *Downloaded via the tidycensus R package.
  # *Download links for spreadsheets were found on https://www.ers.usda.gov/data-products/county-level-data-sets/download-data
  # *Unemployment - Bureau of Labor Statistics - LAUS data - https://www.bls.gov/lau/.
  # *Median Household Income - Census Bureau - Small Area Income and Poverty Estimates (SAIPE) data.
  # *The original data table was prepared by USDA, Economic Research Service.
  # *Census Bureau.
  # *2012-16 American Community Survey 5-yr average.
  # *The original data table was prepared by USDA, Economic Research Service.
  # *Tim Parker (tparker at ers.usda.gov) is the contact for much of the new data incorporated into this data set.
  # ```{r include=FALSE}

library(tidyverse)

#Read in data and pick 6 to 10 factors
cc <- read_csv(here::here("raw/CountyPredictors/Main County Data", "county_complete.csv"))
cc2 <- cc %>% 
  select(fips, state, name, pop2017, some_college_2017, hs_grad_2017, broadband_2017, median_household_income_2017, poverty_2017, unemployment_rate_2017   ) %>% 
  mutate(fips_clean = as.character(sprintf("%05d", fips)))
  

saveRDS(cc2, here::here("rda","countycomplete2017.rds"))
