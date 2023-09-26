

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
options(scipen = 100)


# read in data ----
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")
health.dat2 <- filter(health.dat, is.na(`Poor physical health days raw value`)==0)

# Checking distribution of outcome variable and predictors ----
hist(health.dat2$`Poor physical health days raw value`, breaks=100)
qqp(health.dat2$`Poor physical health days raw value`, "norm")
qqp(health.dat2$`Poor physical health days raw value`, "lnorm")
poisson <- fitdistr(health.dat2$`Poor physical health days raw value`, "Poisson")
qqp(health.dat2$`Poor physical health days raw value`, "pois", poisson$estimate)

hist(health.dat2$`Primary care physicians raw value`, breaks=100)
qqp(health.dat2$`Primary care physicians raw value`, "lnorm")
qqp(health.dat2$`Primary care physicians raw value`, "norm")


# Outcome 1: Poor Physical Health Days   ----

#### Base
physical.base <- lm(`Poor physical health days raw value` ~ as.factor(year) + state,
                    data = health.dat)

results.physical.base <- as.data.frame(summary(physical.base)$coefficients) %>% mutate(model = 'physical.base') %>% 
  rownames_to_column()


#### Demographics
physical.base.demo <- lm(`Poor physical health days raw value` ~ as.factor(year) + state +  
                    `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                    `% below 18 years of age raw value` + `% Females raw value` + 
                    `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                    `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`,                                  
                  data = health.dat)

results.physical.base.demo <- as.data.frame(summary(physical.base.demo)$coefficients) %>% mutate(model = 'physical.base.demo') %>% 
  rownames_to_column()


#### Arrests - unadjusted
physical.noadjust <- lm(`Poor physical health days raw value` ~  young_adult_rate + lag.ya.rate ,
                      data = health.dat)

results.physical.noadjust <- as.data.frame(summary(physical.noadjust)$coefficients) %>% mutate(model = 'physical.noadjust') %>% 
  rownames_to_column()

#### Arrests with state and year FE
physical.state.year <- lm(`Poor physical health days raw value` ~ young_adult_rate + lag.ya.rate  + as.factor(year) + state ,
                        data = health.dat)

results.state.year <- as.data.frame(summary(physical.state.year)$coefficients) %>% mutate(model = 'physical.state.year') %>% 
  rownames_to_column()



#### Arrest current year
physical.arrest <- lm(`Poor physical health days raw value` ~ as.factor(year) + state + young_adult_rate,
                    data = health.dat)

results.physical.arrest <- as.data.frame(summary(physical.arrest)$coefficients) %>% mutate(model = 'physical.arrest') %>% 
  rownames_to_column()



#### Arrests previous year
physical.arrest.lag <- lm(`Poor physical health days raw value` ~ as.factor(year) + state + young_adult_rate + lag.ya.rate,
                    data = health.dat)

results.physical.arrest.lag <- as.data.frame(summary(physical.arrest.lag)$coefficients) %>% mutate(model = 'physical.arrest.lag') %>% 
  rownames_to_column()

#### Arrests current and previous plus demographics

physical.arrest.demo <- lm(`Poor physical health days raw value` ~ as.factor(year) + state +  young_adult_rate + lag.ya.rate +
                         `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                         `% below 18 years of age raw value` + `% Females raw value` + 
                         `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                         `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`,                                  
                       data = health.dat)

results.physical.arrest.demo <- as.data.frame(summary(physical.arrest.demo)$coefficients) %>% mutate(model = 'physical.arrest.demo') %>% 
  rownames_to_column()



#### Structural factors

physical.structural <- lm(`Poor physical health days raw value` ~ as.factor(year) + state + 
                          `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                          `% below 18 years of age raw value` + `% Females raw value` + 
                          `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                          `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                          young_adult_rate + lag.ya.rate + 
                          `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                          `Children in poverty raw value` + `Children in single-parent households raw value`,
                  data = health.dat)

results.physical.structural <- as.data.frame(summary(physical.structural)$coefficients) %>% mutate(model = 'physical.structural') %>% 
  rownames_to_column()


#### Other health factors 
physical.hfactors <- lm(`Poor physical health days raw value` ~ as.factor(year) + state + 
                          `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                          `% below 18 years of age raw value` + `% Females raw value` + 
                          `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                          `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                          young_adult_rate + lag.ya.rate +  `Adult obesity raw value` +   `Diabetes prevalence raw value` + `Low birthweight raw value` +                        
                         `Premature death raw value` + `Primary care physicians raw value` +              
                      `Sexually transmitted infections raw value` + `Uninsured adults raw value`,  
                        data = health.dat)

results.physical.hfactors <- as.data.frame(summary(physical.hfactors)$coefficients) %>% mutate(model = 'physical.hfactors') %>% 
  rownames_to_column()


#### Structural + Other health factors 
physical.structural.hfactors <- lm(`Poor physical health days raw value` ~ as.factor(year) + state + 
                        `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                        `% below 18 years of age raw value` + `% Females raw value` + 
                        `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                        `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                        young_adult_rate + lag.ya.rate + 
                        `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                        `Children in poverty raw value` + `Children in single-parent households raw value` +
                        `Adult obesity raw value` +   `Diabetes prevalence raw value` + `Low birthweight raw value`  +                       
                      `Premature death raw value` + `Primary care physicians raw value` +            
                        `Sexually transmitted infections raw value` + `Uninsured adults raw value`,  
                      data = health.dat)

results.physical.structural.hfactors <- as.data.frame(summary(physical.structural.hfactors)$coefficients) %>% mutate(model = 'physical.structural.hfactors') %>% 
  rownames_to_column()

#### Structural + health access 
physical.structural.hfactors <- lm(`Poor physical health days raw value` ~ as.factor(year) + state + 
                                   `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                                   `% below 18 years of age raw value` + `% Females raw value` + 
                                   `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                                   `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                                   young_adult_rate + lag.ya.rate + 
                                   `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                                   `Children in poverty raw value` + `Children in single-parent households raw value` +
                                  `Primary care physicians raw value` +  `Uninsured adults raw value`,  
                                 data = health.dat)

results.physical.structural.hfactors <- as.data.frame(summary(physical.structural.hfactors)$coefficients) %>% mutate(model = 'physical.structural.hfactors') %>% 
  rownames_to_column()

                   
# Linear Mixed Model ----
library(lme4)
lmm <- lmer(`Poor physical health days raw value` ~ as.factor(year) + 
              `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
              `% below 18 years of age raw value` + `% Females raw value` + 
              `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
              `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
              young_adult_rate + lag.ya.rate + 
              `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
              `Children in poverty raw value` + `Children in single-parent households raw value` + 
              (1 | state), 
            data = health.dat2,
            REML = FALSE)
summary(lmm)


# combining models ----
df_list.physical <- list(results.physical.base, results.physical.base.demo, results.physical.arrest, results.physical.arrest.demo, 
                    results.physical.arrest.lag, results.physical.hfactors, results.physical.structural, results.physical.structural.hfactors)

all.mods.physical <- df_list.physical %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods.physical, here::here("rda","health_results_physical.rds"))


write.csv(all.mods.physical, here::here("Output","health_results_physical.csv"))

                  

