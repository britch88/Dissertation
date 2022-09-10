# Load packa
library(tidyverse)
library(nnet)

# load the data set
dat <-readRDS(here::here("rda","analysis_vi_2017.rds"))

# Specify learners
# Insert both X and A predictors
predset_x01 <- c(
  "bankrate2017.na",                    "bankrate2017.gtp75",   "bankrate2017.p25top75",         
  "poverty_2017.ltp25",                 "poverty_2017.p25top75",          
  "unemployment_rate_2017.ltp25",       "unemployment_rate_2017.p25top75",          
  "median_household_income_2017.gtp75", "median_household_income_2017.p25top75",    
  "some_college_2017.gtp75",            "some_college_2017.p25top75",     
  "hs_grad_2017.gtp75",                 "hs_grad_2017.p25top75",          
  "broadband_2017.gtp75",               "broadband_2017.p25top75",
  "disconnected.2017.na",               "disconnected.2017.ltp25",              "disconnected.2017.p25top75",    
  "food.2017.na",                       "food.2017.gtp75",     "food.2017.p25top75",   
  "violent.2017.na",                    "violent.2017.ltp25",     "violent.2017.p25top75",          
  "segregation.2017.na",                "segregation.2017.ltp25",                  "segregation.2017.p25top75",     
  "pcp_2017.gtp75",                     "pcp_2017.p25top75",   
  "lbw_2017.ltp25",                     "lbw_2017.p25top75",   
  "income_ratio_2017.ltp25",            "income_ratio_2017.p25top75",    
  "uninsured_2017.ltp25",               "uninsured_2017.p25top75",       
  
  
  "under18_2017.ltp25",   "under18_2017.gtp75",            
  "over64_2017.ltp25",    "over64_2017.gtp75",              
  "black_2017.ltp25",     "black_2017.gtp75",                
  "native_2017.ltp25",    "native_2017.gtp75",              
  "pacific_islander_2017.ltp25",    "pacific_islander_2017.gtp75",             
  "hispanic_2017.ltp25",  "hispanic_2017.gtp75",         
  "white_2017.ltp25",     "white_2017.gtp75",               
  "lim_eng_2017.ltp25",   "lim_eng_2017.gtp75",      
  "female_2017.ltp25",    "female_2017.gtp75",              
  "rural_2017.ltp25",     "rural_2017.gtp75",     
  
  "prek_2017.gtp75",      "prek_2017.p25top75",  
  "rentburden_2017.ltp25",                  "rentburden_2017.p25top75",      
  "deaths_2017.lt10",     "deaths_2017.ltp25",    "deaths_2017.p25top75",           
  "incarcerated_2017.ltp25",  "incarcerated_2017.p25top75",     
  "voting_2017.gtp75",             "voting_2017.p25top75",           
  "volunteer_2017.gtp75",          "volunteer_2017.p25top75",        
  
  # Leaving out South which has most counties
  "region_Midwest",       "region_Northeast",  "region_other",        "region_West", 
  
  # Leaving out west north central
  "country_division_east north central",     "country_division_east south central",     "country_division_middle atlantic",       
  "country_division_mountain",     "country_division_new england",  "country_division_pacific",     
  "country_division_possessions",  "country_division_south atlantic",         "country_division_west north central",    
  "country_division_west south central"
  
)  


dat$group2 <- relevel(as.factor(dat$group), ref = "Grp1")
test <- multinom(group2 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   `Violent Crime Rate` + `% Disconnected Youth` + prekpct + deaths_age_adj_rate2017 +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct +
                   bankrate + food
                 , data = dat)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

##### individual logits
dat$rank <- factor(mydata$rank)
mylogitg1 <- glm(Grp1 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                 prekpct +   bankrate + `Violent Crime Rate` +
                 some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                 poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  
               # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
               , data = dat, family = "binomial")

summary(mylogitg1)


## group 2
mylogitg2 <- glm(Grp2 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat, family = "binomial")

summary(mylogitg2)

## group 3
mylogitg3 <- glm(Grp3 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat, family = "binomial")

summary(mylogitg3)


## group 4
mylogitg4 <- glm(Grp4 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat, family = "binomial")

summary(mylogitg4)



## group 5
mylogitg5 <- glm(Grp5 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat, family = "binomial")

summary(mylogitg5)


## group 6
mylogitg6 <- glm(Grp6 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat, family = "binomial")

summary(mylogitg6)

