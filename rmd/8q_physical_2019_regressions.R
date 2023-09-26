

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
library(lme4)
options(scipen = 100)


# read in data ----
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")
health.dat2 <- filter(health.dat, is.na(`Poor physical health days raw value`)==0)



#### Arrests with county RE

lm1 <- lm(`Poor physical health days raw value` ~  young_adult_rate + lag.ya.rate + fips_clean,
             data = health.dat2)

summary(lm1)
results.fe.arrests <- as.data.frame(summary(lm1)$coefficients) %>% mutate(model = 'FE arrests') %>% 
  rownames_to_column()

#### Arrests current and previous plus demographics
lm2 <- lm(`Poor physical health days raw value` ~  young_adult_rate + lag.ya.rate +
               `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
               `% below 18 years of age raw value` + `% Females raw value` + 
               `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
               `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + fips_clean, 
             data = health.dat2)

summary(lm2)
results.fe.demo <- as.data.frame(summary(lm2)$coefficients) %>% mutate(model = 'arrests + demographics') %>% 
  rownames_to_column()



#### Structural factors

lm3 <- lm(`Poor physical health days raw value` ~  young_adult_rate + lag.ya.rate + 
               `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
               `% below 18 years of age raw value` + `% Females raw value` + 
               `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
               `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
               
               `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
               `Children in poverty raw value` + `Children in single-parent households raw value` + 
               fips_clean,
             data = health.dat2)

summary(lm3)
results.fe.full <- as.data.frame(summary(lm3)$coefficients) %>% mutate(model = 'FE full') %>% 
  rownames_to_column()


# combining models ----
df_list <- list(results.fe.arrests, results.fe.demo, results.fe.full)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods, here::here("rda","fe_physical"))


write.csv(all.mods, here::here("Output","fe_physical.csv"))

                  

