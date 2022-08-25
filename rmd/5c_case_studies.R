#load packages
library(tidyverse)

# load the data set
dat <-readRDS(here::here("rda","analysis_vi_2017.rds"))

dat2 <- dat %>% 
  select(group, segregation_white_nonwhite,
         `% < 18`, `% 65 and over`, `% African American`,
         `% American Indian/Alaskan Native`,`% Native Hawaiian/Other Pacific Islander`,
         `% Hispanic`, `% Non-Hispanic White`, `% Not Proficient in English`, `% Female`,
         `% Rural`,  pop2017,`country_division_east north central`, `country_division_east south central`,
         `country_division_middle atlantic`,country_division_mountain,`country_division_new england`,
         country_division_pacific,
         #Grp1, Grp2, Grp3, Grp4,Grp5, Grp6,
         #state_abb,state, fips_clean, name,
         #country_division,  region, 
         prekpct, 
         deaths_age_adj_rate2017, 
         some_college_2017, 
         hs_grad_2017, 
         broadband_2017, 
         median_household_income_2017,  
         poverty_2017,  
         unemployment_rate_2017,  
         incarcerated_rate, 
         voting_pct, 
         volunteer_pct, 
         bankrate, 
         rentburden_pct, 
         `PCP Rate`, 
         `Income Ratio`,
         `% LBW`,
         `% Uninsured`, 
         `Violent Crime Rate`, 
         `% Disconnected Youth`, 
         food)


summary.dat <- dat2 %>%  group_by(group) %>% 
  summarise_all(mean, na.rm = TRUE)
