#load packages
library(tidyverse)

# load the data set
dat <-readRDS(here::here("rda","analysis_vi_2017.rds"))

######## Process for selecting counties
#: Started by selecting on indicators that had highest positive association rate;
#: if number of potentials was still high, added demographic and/or geographic
#characteristics with highest positive association; if that still left too many (> 30)
#options, then I excluded those counties with high negative values; also considered
# getting some geographic spread;



############# Select Group 1 County
grp1 <- dat %>% 
  filter(Grp1==1,
        volunteer_2017.gtp75 ==1,
         pcp_2017.gtp75 ==1,
         median_household_income_2017.gtp75==1,
       #  income_ratio_2017.ltp25 ==1,
         bankrate2017.gtp75==1,
         some_college_2017.gtp75 ==1) %>% 
  select(!contains("state_")) %>% 
  select(!contains("p25top75")) %>% 
  select(!contains("region_")) %>% 
  select(!contains("division")) %>% 
  select(!contains("Grp"))
# Ramsey County, ND 38071


############## Select Group 2 County
grp2 <- dat %>% 
filter(Grp2==1,
       violent.2017.ltp25 ==1,
       rural_2017.gtp75 ==1,
       rentburden_2017.ltp25==1,
       black_2017.gtp75 ==1)
select(!contains("state_")) %>% 
select(!contains("p25top75")) %>% 
select(!contains("region_")) %>% 
select(!contains("division")) %>% 
select(!contains("Grp"))
# Powhatan County, Virginia 51145


############## Select Group 3 County

grp3 <- dat %>% 
  filter(Grp3==1,
         poverty_2017.ltp25 ==1,
         unemployment_rate_2017.ltp25 ==0,
         disconnected.2017.ltp25==0,
         voting_2017.gtp75==0,
         rural_2017.gtp75==0,
         country_division_mountain==0)
select(!contains("state_")) %>% 
  select(!contains("p25top75")) %>% 
  select(!contains("region_")) %>% 
  select(!contains("division")) %>% 
  select(!contains("Grp"))
#Brazoria County, TX 48039


  
############### Select Group 4 County

grp4 <- dat %>% 
  filter(Grp4==1,
         voting_2017.gtp75 ==1,
         incarcerated_2017.ltp25 ==1,
         unemployment_rate_2017.ltp25==1,
         over64_2017.gtp75==1,
         region_Midwest==0,
         region_West==0) %>% 
select(!contains("state_")) %>% 
  select(!contains("p25top75")) %>% 
  select(!contains("region_")) %>% 
  select(!contains("division")) %>% 
  select(!contains("Grp"))
#Lincoln County, ME 23015
  
  
############### Select Group 5 County

grp5 <- dat %>% 
  filter(Grp5==1,
         volunteer_2017.gtp75 ==1,
         uninsured_2017.ltp25 ==1,
         female_2017.ltp25==1) %>% 
  select(!contains("state_")) %>% 
  select(!contains("p25top75")) %>% 
  select(!contains("region_")) %>% 
  select(!contains("division")) %>% 
  select(!contains("Grp"))
#Chisago County, MN 27025


############### Select Group 6 County
grp6 <- dat %>% 
  filter(Grp6==1,
         voting_2017.gtp75 ==1,
         disconnected.2017.ltp25 ==1,
         lbw_2017.ltp25==1) %>% 
  select(!contains("state_")) %>% 
  select(!contains("p25top75")) %>% 
  select(!contains("region_")) %>% 
  select(!contains("division")) %>% 
  select(!contains("Grp"))
#Snohomish County, WA 53061




# dat2 <- dat %>% 
#   select(group, segregation_white_nonwhite,
#          `% < 18`, `% 65 and over`, `% African American`,
#          `% American Indian/Alaskan Native`,`% Native Hawaiian/Other Pacific Islander`,
#          `% Hispanic`, `% Non-Hispanic White`, `% Not Proficient in English`, `% Female`,
#          `% Rural`,  pop2017,`country_division_east north central`, `country_division_east south central`,
#          `country_division_middle atlantic`,country_division_mountain,`country_division_new england`,
#          country_division_pacific,
#          #Grp1, Grp2, Grp3, Grp4,Grp5, Grp6,
#          #state_abb,state, fips_clean, name,
#          #country_division,  region, 
#          prekpct, 
#          deaths_age_adj_rate2017, 
#          some_college_2017, 
#          hs_grad_2017, 
#          broadband_2017, 
#          median_household_income_2017,  
#          poverty_2017,  
#          unemployment_rate_2017,  
#          incarcerated_rate, 
#          voting_pct, 
#          volunteer_pct, 
#          bankrate, 
#          rentburden_pct, 
#          `PCP Rate`, 
#          `Income Ratio`,
#          `% LBW`,
#          `% Uninsured`, 
#          `Violent Crime Rate`, 
#          `% Disconnected Youth`, 
#          food)


summary.dat <- dat2 %>%  group_by(group) %>% 
  summarise_all(mean, na.rm = TRUE)
