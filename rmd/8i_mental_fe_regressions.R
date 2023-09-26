

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
health.dat2 <- filter(health.dat, is.na(`Poor mental health days raw value`)==0)



#### Arrests with county RE

lmm1 <- lm(`Poor mental health days raw value` ~  young_adult_rate + lag.ya.rate + fips_clean,
             data = health.dat2)

summary(lmm1)
results.fe.arrests <- as.data.frame(summary(lmm1)$coefficients) %>% mutate(model = 'FE arrests') %>% 
  rownames_to_column()

#### Arrests current and previous plus demographics
lmm2 <- lm(`Poor mental health days raw value` ~  young_adult_rate + lag.ya.rate +
               `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
               `% below 18 years of age raw value` + `% Females raw value` + 
               `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
               `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + fips_clean, 
             data = health.dat2)

summary(lmm2)
results.fe.demo <- as.data.frame(summary(lmm2)$coefficients) %>% mutate(model = 'arrests + demographics') %>% 
  rownames_to_column()



#### Structural factors

lmm3 <- lmer(`Poor mental health days raw value` ~  young_adult_rate + lag.ya.rate + 
              `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
              `% below 18 years of age raw value` + `% Females raw value` + 
              `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
              `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
             
              `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
              `Children in poverty raw value` + `Children in single-parent households raw value` + 
              (1 | fips_clean), 
            data = health.dat2,
            REML = FALSE)
summary(lmm3)
results.lme.full <- as.data.frame(summary(lmm3)$coefficients) %>% mutate(model = 'lmm full') %>% 
  rownames_to_column()

fish <- lmer(`Poor mental health days raw value` ~  young_adult_rate + lag.ya.rate + 
               `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
               `% below 18 years of age raw value` + `% Females raw value` + 
               `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
               `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
               
               `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
               `Children in poverty raw value` + `Children in single-parent households raw value` + 
               fips_clean +
               (1 | year), 
             data = health.dat2,
             REML = FALSE)
summary(fish)


fish2 <- lm(`Poor mental health days raw value` ~  young_adult_rate + lag.ya.rate + 
               `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
               `% below 18 years of age raw value` + `% Females raw value` + 
               `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
               `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
               
               `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
               `Children in poverty raw value` + `Children in single-parent households raw value` + 
               fips_clean,
             data = health.dat2)

summary(fish2)
summary(fish)
results.fe.full <- as.data.frame(summary(fish2)$coefficients) %>% mutate(model = 'FE full') %>% 
  rownames_to_column()


# combining models ----
df_list.poor.fair <- list(results.fe.arrests, results.fe.demo, results.fe.full)

all.mods.poor.fair <- df_list.poor.fair %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods.poor.fair, here::here("rda","fe_mental"))


write.csv(all.mods.poor.fair, here::here("Output","fe_mental.csv"))

                  

