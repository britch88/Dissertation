

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
health.dat2 <- filter(health.dat, is.na(`Poor mental health days raw value`)==0 & year==2019) %>% 
  mutate(ya_change = young_adult_rate - lag.ya.rate)



#### Arrests 2019

lmm1current <- lm(`Poor mental health days raw value` ~  young_adult_rate,
             data = health.dat2)

summary(lmm1current)
res.lmm1current <- as.data.frame(summary(lmm1current)$coefficients) %>% 
  mutate(model = 'arrests') %>% 
  rownames_to_column()


lmm1previous <- lm(`Poor mental health days raw value` ~ lag.ya.rate,
                  data = health.dat2)

summary(lmm1previous)
res.lmm1previous <- as.data.frame(summary(lmm1previous)$coefficients) %>% 
  mutate(model = 'arrests') %>% 
  rownames_to_column()


lmm1change <- lm(`Poor mental health days raw value` ~  ya_change,
                   data = health.dat2)

summary(lmm1change)
res.lmm1change <- as.data.frame(summary(lmm1change)$coefficients) %>% 
  mutate(model = 'arrests') %>% 
  rownames_to_column()





#### Arrests current and previous plus demographics 2019
lmm2current <- lm(`Poor mental health days raw value` ~  young_adult_rate +
               `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
               `% below 18 years of age raw value` + `% Females raw value` + 
               `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
               `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`, 
             data = health.dat2)

summary(lmm2current)
results.lmm2current <- as.data.frame(summary(lmm2current)$coefficients) %>% mutate(model = 'arrests + demographics') %>% 
  rownames_to_column()


lmm2previous <- lm(`Poor mental health days raw value` ~  lag.ya.rate +
                    `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                    `% below 18 years of age raw value` + `% Females raw value` + 
                    `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                    `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`, 
                  data = health.dat2)

summary(lmm2previous)
results.lmm2previous <- as.data.frame(summary(lmm2previous)$coefficients) %>% mutate(model = 'arrests + demographics') %>% 
  rownames_to_column()


lmm2change <- lm(`Poor mental health days raw value` ~  ya_change +
                     `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                     `% below 18 years of age raw value` + `% Females raw value` + 
                     `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                     `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`, 
                   data = health.dat2)

summary(lmm2change)
results.lmm2change <- as.data.frame(summary(lmm2change)$coefficients) %>% mutate(model = 'arrests + demographics') %>% 
  rownames_to_column()



#### Structural factors

lmm3current <- lm(`Poor mental health days raw value` ~  young_adult_rate  + 
               `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
               `% below 18 years of age raw value` + `% Females raw value` + 
               `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
               `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
               
               `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
               `Children in poverty raw value` + `Children in single-parent households raw value`,
             data = health.dat2)

summary(lmm3current)
results.lmm3current <- as.data.frame(summary(lmm3current)$coefficients) %>% mutate(model = 'full') %>% 
  rownames_to_column()

lmm3previous <- lm(`Poor mental health days raw value` ~   lag.ya.rate + 
                    `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                    `% below 18 years of age raw value` + `% Females raw value` + 
                    `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                    `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                    
                    `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                    `Children in poverty raw value` + `Children in single-parent households raw value`,
                  data = health.dat2)

summary(lmm3previous)
results.lmm3previous <- as.data.frame(summary(lmm3previous)$coefficients) %>% mutate(model = 'full') %>% 
  rownames_to_column()


lmm3change <- lm(`Poor mental health days raw value` ~  ya_change + 
                     `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                     `% below 18 years of age raw value` + `% Females raw value` + 
                     `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                     `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                     
                     `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                     `Children in poverty raw value` + `Children in single-parent households raw value`,
                   data = health.dat2)

summary(lmm3change)
results.lmm3change <- as.data.frame(summary(lmm3change)$coefficients) %>% mutate(model = 'full') %>% 
  rownames_to_column()



lmm3 <- lm(`Poor mental health days raw value` ~  lag.ya.rate + young_adult_rate +
                   `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                   `% below 18 years of age raw value` + `% Females raw value` + 
                   `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                   `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                   
                   `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                   `Children in poverty raw value` + `Children in single-parent households raw value`,
                 data = health.dat2)

summary(lmm3)
results.lmm3 <- as.data.frame(summary(lmm3)$coefficients) %>% mutate(model = 'full') %>% 
  rownames_to_column()


# combining models ----
df_list.poor.fair <- list(res.lmm1current, res.lmm1previous, res.lmm1change,
                          results.lmm2current, results.lmm2previous, results.lmm2change,
                          results.lmm3current, results.lmm3previous, results.lmm3change)

all.mods.poor.fair <- df_list.poor.fair %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods.poor.fair, here::here("rda","mental2019.rds"))


write.csv(all.mods.poor.fair, here::here("Output","mental2019.csv"))

                  

