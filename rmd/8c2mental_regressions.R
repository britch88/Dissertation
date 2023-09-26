

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(ggcorrplot)
options(scipen = 100)


# read in data ----
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")


# Outcome 1: Poor Mental Health Days   ----

#### Base
mental.base <- lm(`Poor mental health days raw value` ~ as.factor(year) + state,
                    data = health.dat)

results.mental.base <- as.data.frame(summary(mental.base)$coefficients) %>% mutate(model = 'mental.base') %>% 
  rownames_to_column()


#### Demographics
mental.base.demo <- lm(`Poor mental health days raw value` ~ as.factor(year) + state +  
                    `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                    `% below 18 years of age raw value` + `% Females raw value` + 
                    `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                    `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`,                                  
                  data = health.dat)

results.mental.base.demo <- as.data.frame(summary(mental.base.demo)$coefficients) %>% mutate(model = 'mental.base.demo') %>% 
  rownames_to_column()

#### Arrests - unadjusted
mental.noadjust <- lm(`Poor mental health days raw value` ~  young_adult_rate + lag.ya.rate ,
                    data = health.dat)

results.mental.noadjust <- as.data.frame(summary(mental.noadjust)$coefficients) %>% mutate(model = 'mental.noadjust') %>% 
  rownames_to_column()

#### Arrests with state and year FE
mental.state.year <- lm(`Poor mental health days raw value` ~ young_adult_rate + lag.ya.rate  + as.factor(year) + state ,
                      data = health.dat)

results.state.year <- as.data.frame(summary(mental.state.year)$coefficients) %>% mutate(model = 'mental.state.year') %>% 
  rownames_to_column()


#### Arrest current year
mental.arrest <- lm(`Poor mental health days raw value` ~ as.factor(year) + state + young_adult_rate,
                    data = health.dat)

results.mental.arrest <- as.data.frame(summary(mental.arrest)$coefficients) %>% mutate(model = 'mental.arrest') %>% 
  rownames_to_column()



#### Arrests previous year
mental.arrest.lag <- lm(`Poor mental health days raw value` ~ as.factor(year) + state + young_adult_rate + lag.ya.rate,
                    data = health.dat)

results.mental.arrest.lag <- as.data.frame(summary(mental.arrest.lag)$coefficients) %>% mutate(model = 'mental.arrest.lag') %>% 
  rownames_to_column()

#### Arrests current and previous plus demographics

mental.arrest.demo <- lm(`Poor mental health days raw value` ~ as.factor(year) + state +  young_adult_rate + lag.ya.rate +
                         `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                         `% below 18 years of age raw value` + `% Females raw value` + 
                         `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                         `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value`,                                  
                       data = health.dat)

results.mental.arrest.demo <- as.data.frame(summary(mental.arrest.demo)$coefficients) %>% mutate(model = 'mental.arrest.demo') %>% 
  rownames_to_column()



#### Structural factors

mental.structural <- lm(`Poor mental health days raw value` ~ as.factor(year) + state + 
                          `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                          `% below 18 years of age raw value` + `% Females raw value` + 
                          `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                          `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                          young_adult_rate + lag.ya.rate + 
                          `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                          `Children in poverty raw value` + `Children in single-parent households raw value`,
                  data = health.dat)

results.mental.structural <- as.data.frame(summary(mental.structural)$coefficients) %>% mutate(model = 'mental.structural') %>% 
  rownames_to_column()


#### Other health factors 
mental.hfactors <- lm(`Poor mental health days raw value` ~ as.factor(year) + state + 
                          `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                          `% below 18 years of age raw value` + `% Females raw value` + 
                          `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                          `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                          young_adult_rate + lag.ya.rate +  `Adult obesity raw value` +   `Diabetes prevalence raw value` + `Low birthweight raw value` +                        
                         `Premature death raw value` + `Primary care physicians raw value` +              
                      `Sexually transmitted infections raw value` + `Uninsured adults raw value`,  
                        data = health.dat)

results.mental.hfactors <- as.data.frame(summary(mental.hfactors)$coefficients) %>% mutate(model = 'mental.hfactors') %>% 
  rownames_to_column()


#### Structural + Other health factors 
mental.structural.hfactors <- lm(`Poor mental health days raw value` ~ as.factor(year) + state + 
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

results.mental.structural.hfactors <- as.data.frame(summary(mental.structural.hfactors)$coefficients) %>% mutate(model = 'mental.structural.hfactors') %>% 
  rownames_to_column()

#### Structural + health access 
mental.structural.hfactors <- lm(`Poor mental health days raw value` ~ as.factor(year) + state + 
                                   `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                                   `% below 18 years of age raw value` + `% Females raw value` + 
                                   `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                                   `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
                                   young_adult_rate + lag.ya.rate + 
                                   `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                                   `Children in poverty raw value` + `Children in single-parent households raw value` +
                                  `Primary care physicians raw value` +  `Uninsured adults raw value`,  
                                 data = health.dat)

results.mental.structural.hfactors <- as.data.frame(summary(mental.structural.hfactors)$coefficients) %>% mutate(model = 'mental.structural.hfactors') %>% 
  rownames_to_column()


# Linear Mixed Model ----
library(lme4)
lmm <- lmer(`Poor mental health days raw value` ~ as.factor(year) + 
              `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
              `% below 18 years of age raw value` + `% Females raw value` + 
              `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
              `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` + 
              young_adult_rate + lag.ya.rate + 
              `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
              `Children in poverty raw value` + `Children in single-parent households raw value` + 
              (1 | state), 
            data = health.dat,
            REML = FALSE)
summary(lmm)
                   
                  
# combining models ----
df_list.mental <- list(results.mental.base, results.mental.base.demo, results.mental.arrest, results.mental.arrest.demo, 
                    results.mental.arrest.lag, results.mental.hfactors, results.mental.structural, results.mental.structural.hfactors)

all.mods.mental <- df_list.mental %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods.mental, here::here("rda","health_results_mental.rds"))


write.csv(all.mods.mental, here::here("Output","health_results_mental.csv"))

                  

