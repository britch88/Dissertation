

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
#install.packages("lfe")
library(lfe)

# run script that creates data set
source("/data/share/xproject/Training/Practice/henderson/Dissertation/rmd/8e_Subgroups.R")



# Advantaged    ----


adv <-felm(`Poor or fair health raw value` ~  young_adult_rate + lag.ya.rate +
             `% 65 and older raw value` +  `% below 18 years of age raw value` + `% Females raw value` + 
             `% American Indian and Alaskan Native raw value` + `% Asian raw value` + 
             `% Non-Hispanic African American raw value` +   `% Hispanic raw value` +   
             `% Native Hawaiian/Other Pacific Islander raw value` + 
            `% not proficient in English raw value` + `% Rural raw value` +
             
             `Unemployment raw value` + `Some college raw value`  +  `Children in poverty raw value` + 
             `Children in single-parent households raw value` + `Median household income raw value`
           | fips_clean | 0 | fips_clean , advantage2) 

summary(adv)

results.adv <- as.data.frame(summary(adv)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()



# Disadvantaged ----
dis <-felm(`Poor or fair health raw value` ~  young_adult_rate + lag.ya.rate +
             `% 65 and older raw value` +  `% below 18 years of age raw value` + `% Females raw value` + 
             `% American Indian and Alaskan Native raw value` + `% Asian raw value` + 
             `% Non-Hispanic African American raw value` +   `% Hispanic raw value` +   
             `% Native Hawaiian/Other Pacific Islander raw value` + 
             `% not proficient in English raw value` + `% Rural raw value` +
             
             `Unemployment raw value` + `Some college raw value`  +  `Children in poverty raw value` + 
             `Children in single-parent households raw value` + `Median household income raw value`
           | fips_clean | 0 | fips_clean , disadvantage2) 


summary(dis)

results.dis <- as.data.frame(summary(dis)$coefficients) %>% mutate(model = 'Disadvantaged') %>% 
  rownames_to_column()



# combining models ----
df_list<- list(results.adv, results.dis)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods, here::here("rda","averaged_subgroup_crfe_mental.rds"))


write.csv(all.mods, here::here("Output","averaged_subgroup_crfe_mental.csv"))

                  

