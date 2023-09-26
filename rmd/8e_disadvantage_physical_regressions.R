

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
options(scipen = 100)

# run script that creates data set
#source("/data/share/xproject/Training/Practice/henderson/Dissertation/rmd/8e_Subgroups.R")


# Checking distribution of outcome variable and predictors ----
hist(disadvantage$`Poor physical health days raw value`, breaks=100)


# Running Models: Poor physical health days    ----


#### Arrests with state and year FE
poor.fair.state.year <- lm(`Poor physical health days raw value` ~ young_adult_rate + lag.ya.rate  + as.factor(year) + state ,
                           data = disadvantage)

results.state.year <- as.data.frame(summary(poor.fair.state.year)$coefficients) %>% mutate(model = 'poor.fair.state.year') %>% 
  rownames_to_column()



#### Arrests current and previous plus demographics

poor.fair.arrest.demo <- lm(`Poor physical health days raw value` ~ as.factor(year) + state +  young_adult_rate + lag.ya.rate +
                         `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                         `% below 18 years of age raw value` + `% Females raw value` + 
                         `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                         `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`,                                  
                       data = disadvantage)

results.poor.fair.arrest.demo <- as.data.frame(summary(poor.fair.arrest.demo)$coefficients) %>% mutate(model = 'poor.fair.arrest.demo') %>% 
  rownames_to_column()



#### Structural factors

poor.fair.structural <- lm(`Poor physical health days raw value` ~ as.factor(year) + state + 
                          `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                          `% below 18 years of age raw value` + `% Females raw value` + 
                          `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                          `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                          young_adult_rate + lag.ya.rate + 
                          `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                          `Children in poverty raw value` + `Children in single-parent households raw value`,
                  data = disadvantage)

results.poor.fair.structural <- as.data.frame(summary(poor.fair.structural)$coefficients) %>% mutate(model = 'poor.fair.structural') %>% 
  rownames_to_column()





# combining models ----
df_list.poor.fair <- list(results.state.year, results.poor.fair.arrest.demo, 
                     results.poor.fair.structural)

all.mods.poor.fair <- df_list.poor.fair %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods.poor.fair, here::here("rda","disadvantaged_health_results_physical.rds"))


write.csv(all.mods.poor.fair, here::here("Output","disadvantaged_health_results_physical.csv"))

                  

