

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
health.dat2 <- filter(health.dat, is.na(`Poor or fair health raw value`)==0)

# Checking distribution of outcome variable and predictors ----
hist(health.dat2$`Poor or fair health raw value`, breaks=100)
qqp(health.dat2$`Poor or fair health raw value`, "norm")
qqp(health.dat2$`Poor or fair health raw value`, "lnorm")


# Running Models: Poor or fair Health    ----

#### Base
poor.fair.base <- lm(`Poor or fair health raw value` ~ as.factor(year) + state,
                    data = health.dat)

results.poor.fair.base <- as.data.frame(summary(poor.fair.base)$coefficients) %>% mutate(model = 'poor.fair.base') %>% 
  rownames_to_column()


#### Demographics
poor.fair.base.demo <- lm(`Poor or fair health raw value` ~ as.factor(year) + state +  
                    `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                    `% below 18 years of age raw value` + `% Females raw value` + 
                    `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                    `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`,                                  
                  data = health.dat)

results.poor.fair.base.demo <- as.data.frame(summary(poor.fair.base.demo)$coefficients) %>% mutate(model = 'poor.fair.base.demo') %>% 
  rownames_to_column()



#### Arrests - unadjusted
poor.fair.noadjust <- lm(`Poor or fair health raw value` ~  young_adult_rate + lag.ya.rate ,
                        data = health.dat)

results.noadjust <- as.data.frame(summary(poor.fair.noadjust)$coefficients) %>% mutate(model = 'poor.fair.noadjust') %>% 
  rownames_to_column()

#### Arrests with state and year FE
poor.fair.state.year <- lm(`Poor or fair health raw value` ~ young_adult_rate + lag.ya.rate  + as.factor(year) + state ,
                          data = health.dat)

results.state.year <- as.data.frame(summary(poor.fair.state.year)$coefficients) %>% mutate(model = 'poor.fair.state.year') %>% 
  rownames_to_column()



#### Arrest current year
poor.fair.arrest <- lm(`Poor or fair health raw value` ~ as.factor(year) + state + young_adult_rate,
                    data = health.dat)

results.poor.fair.arrest <- as.data.frame(summary(poor.fair.arrest)$coefficients) %>% mutate(model = 'poor.fair.arrest') %>% 
  rownames_to_column()



#### Arrests previous year
poor.fair.arrest.lag <- lm(`Poor or fair health raw value` ~ as.factor(year) + state + young_adult_rate + lag.ya.rate,
                    data = health.dat)

results.poor.fair.arrest.lag <- as.data.frame(summary(poor.fair.arrest.lag)$coefficients) %>% mutate(model = 'poor.fair.arrest.lag') %>% 
  rownames_to_column()

#### Arrests current and previous plus demographics

poor.fair.arrest.demo <- lm(`Poor or fair health raw value` ~ as.factor(year) + state +  young_adult_rate + lag.ya.rate +
                         `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                         `% below 18 years of age raw value` + `% Females raw value` + 
                         `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                         `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`,                                  
                       data = health.dat)

results.poor.fair.arrest.demo <- as.data.frame(summary(poor.fair.arrest.demo)$coefficients) %>% mutate(model = 'poor.fair.arrest.demo') %>% 
  rownames_to_column()



#### Structural factors

poor.fair.structural <- lm(`Poor or fair health raw value` ~ as.factor(year) + state + 
                          `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                          `% below 18 years of age raw value` + `% Females raw value` + 
                          `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                          `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                          young_adult_rate + lag.ya.rate + 
                          `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                          `Children in poverty raw value` + `Children in single-parent households raw value`,
                  data = health.dat)

results.poor.fair.structural <- as.data.frame(summary(poor.fair.structural)$coefficients) %>% mutate(model = 'poor.fair.structural') %>% 
  rownames_to_column()


#### Other health factors 
poor.fair.hfactors <- lm(`Poor or fair health raw value` ~ as.factor(year) + state + 
                          `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                          `% below 18 years of age raw value` + `% Females raw value` + 
                          `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                          `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                          young_adult_rate + lag.ya.rate +  `Adult obesity raw value` +   `Diabetes prevalence raw value` + `Low birthweight raw value` +                        
                         `Premature death raw value` + `Primary care physicians raw value` +              
                      `Sexually transmitted infections raw value` + `Uninsured adults raw value`,  
                        data = health.dat)

results.poor.fair.hfactors <- as.data.frame(summary(poor.fair.hfactors)$coefficients) %>% mutate(model = 'poor.fair.hfactors') %>% 
  rownames_to_column()


#### Structural + Other health factors 
poor.fair.structural.hfactors <- lm(`Poor or fair health raw value` ~ as.factor(year) + state + 
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

results.poor.fair.structural.hfactors <- as.data.frame(summary(poor.fair.structural.hfactors)$coefficients) %>% mutate(model = 'poor.fair.structural.hfactors') %>% 
  rownames_to_column()

#### Structural + health access 
poor.fair.structural.hfactors <- lm(`Poor or fair health raw value` ~ as.factor(year) + state + 
                                   `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                                   `% below 18 years of age raw value` + `% Females raw value` + 
                                   `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                                   `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                                   young_adult_rate + lag.ya.rate + 
                                   `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                                   `Children in poverty raw value` + `Children in single-parent households raw value` +
                                  `Primary care physicians raw value` +  `Uninsured adults raw value`,  
                                 data = health.dat)

results.poor.fair.structural.hfactors <- as.data.frame(summary(poor.fair.structural.hfactors)$coefficients) %>% mutate(model = 'poor.fair.structural.hfactors') %>% 
  rownames_to_column()

                   
# Linear Mixed Model ----
library(lme4)
lmm <- lmer(`Poor or fair health raw value` ~ as.factor(year) + 
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
df_list.poor.fair <- list(results.poor.fair.base, results.poor.fair.base.demo, results.poor.fair.arrest, results.poor.fair.arrest.demo, 
                    results.poor.fair.arrest.lag, results.poor.fair.hfactors, results.poor.fair.structural, results.poor.fair.structural.hfactors)

all.mods.poor.fair <- df_list.poor.fair %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods.poor.fair, here::here("rda","health_results_poor_fair.rds"))


write.csv(all.mods.poor.fair, here::here("Output","health_results_poor_fair.csv"))

                  

