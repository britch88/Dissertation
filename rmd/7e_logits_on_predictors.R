# Load packages ----
library(tidyverse)
library(nnet)

# load the data set ----
dat <-readRDS(here::here("rda","analysis_vi_2017.rds"))
dat2 <- dat %>% 
  select(-contains("Grp")) %>% 
  select(-contains("group"))

sample <- readRDS(here::here("rda","gbtm4g2p.rds")) 
sample2 <- sample %>% 
  select(fips_clean,cluster) %>% 
  mutate(Grp1 = as.numeric(cluster==1),
         Grp2 = as.numeric(cluster==2),
         Grp3 = as.numeric(cluster==3),
         Grp4 = as.numeric(cluster==4))


dat3<- merge(sample2,dat2,by="fips_clean",all.x =TRUE, all.y = FALSE)
saveRDS(dat3,here::here("rda","logit_analysis_file.rds"))


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



##### individual logits
mylogitg1 <- glm(Grp1 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                 prekpct +   bankrate + `Violent Crime Rate` +
                 some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                 poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  +
                   `% < 18` + `% 65 and over` + `% Not Proficient in English` + `% Rural` + `% Female` +
                   `% Non-Hispanic White` + `segregation_white_nonwhite` + `pop2017`
               # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
               , data = dat3, family = "binomial")

summary(mylogitg1)
mod_beta1 <- as.data.frame(summary(mylogitg1)$coefficients) %>% mutate(model = 1)



## group 2
mylogitg2 <- glm(Grp2 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  +
                   `% < 18` + `% 65 and over` + `% Not Proficient in English` + `% Rural` + `% Female` +
                   `% Non-Hispanic White` + `segregation_white_nonwhite` + `pop2017` 
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat3, family = "binomial")

summary(mylogitg2)
mod_beta2 <- as.data.frame(summary(mylogitg2)$coefficients) %>% mutate(model = 2)


## group 3
mylogitg3 <- glm(Grp3 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct   +
                   `% < 18` + `% 65 and over` + `% Not Proficient in English` + `% Rural` + `% Female` +
                   `% Non-Hispanic White` + `segregation_white_nonwhite` + `pop2017`
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat3, family = "binomial")

summary(mylogitg3)
mod_beta3 <- as.data.frame(summary(mylogitg3)$coefficients) %>% mutate(model = 3)


## group 4
mylogitg4 <- glm(Grp4 ~ rentburden_pct + `PCP Rate` + `Income Ratio` + `% LBW` + `% Uninsured` + 
                   prekpct +   bankrate + `Violent Crime Rate` +
                   some_college_2017 + hs_grad_2017 + broadband_2017 + median_household_income_2017 +
                   poverty_2017 + unemployment_rate_2017 + incarcerated_rate + voting_pct + volunteer_pct  +
                   `% < 18` + `% 65 and over` + `% Not Proficient in English` + `% Rural` + `% Female` +
                   `% Non-Hispanic White` + `segregation_white_nonwhite` + `pop2017` 
                 # `% Disconnected Youth` + bankrate + food +  `Violent Crime Rate`  + deaths_age_adj_rate2017
                 , data = dat3, family = "binomial")

summary(mylogitg4)
mod_beta4 <- as.data.frame(summary(mylogitg4)$coefficients) %>% mutate(model = 4)

# Merging models ----
mod_beta1x <- mod_beta1 %>% 
  rename('Estimate1' = "Estimate",
         'Std. Error1' = "Std. Error",
         'z value1'= "z value",
         'Pr(>|z|)1' = "Pr(>|z|)") %>% 
  rownames_to_column("var")



mod_beta2x <- mod_beta2 %>% 
  rename('Estimate2' = "Estimate",
         'Std. Error2' = "Std. Error",
         'z value2'= "z value",
         'Pr(>|z|)2' = "Pr(>|z|)") %>% 
  rownames_to_column("var")


mod_beta3x <- mod_beta3 %>% 
  rename('Estimate3' = "Estimate",
         'Std. Error3' = "Std. Error",
         'z value3'= "z value",
         'Pr(>|z|)3' = "Pr(>|z|)") %>% 
  rownames_to_column("var")


mod_beta4x <- mod_beta4 %>% 
  rename('Estimate4' = "Estimate",
         'Std. Error4' = "Std. Error",
         'z value4'= "z value",
         'Pr(>|z|)4' = "Pr(>|z|)") %>% 
  rownames_to_column("var")



#put all data frames into list
df_list <- list(mod_beta1x, mod_beta2x, mod_beta3x,mod_beta4x)
all.mods <- df_list %>% reduce(full_join, by='var')

write.csv(all.mods, here::here("rda","lim_traj_logit_results230113.csv"))



