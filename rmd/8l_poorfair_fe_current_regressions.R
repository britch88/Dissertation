

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
library(lme4)
#install.packages("lfe")
library(lfe)
options(scipen = 100)


# read in data ----
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")
health.dat2 <- filter(health.dat, is.na(`Poor or fair health raw value`)==0)



#### Arrests with county and time FE



twfe1 <- felm(`Poor or fair health raw value` ~  young_adult_rate  | fips_clean  | 0 | fips_clean, health.dat2)
summary(twfe1)



results.twfe.arrests <- as.data.frame(summary(twfe1)$coefficients) %>% mutate(model = 'TWFE arrests') %>% 
  rownames_to_column()

#### Arrests current and previous plus demographics
twfe2 <- felm(`Poor or fair health raw value` ~  young_adult_rate  +
                `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                `% below 18 years of age raw value` + `% Females raw value` + 
                `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` 
              | fips_clean  | 0 | fips_clean, health.dat2)
summary(twfe2)


results.twfe.demo <- as.data.frame(summary(twfe2)$coefficients) %>% mutate(model = 'TWFE demographics') %>% 
  rownames_to_column()



#### Structural factors
twfe3 <- felm(`Poor or fair health raw value` ~  young_adult_rate  +
                `% 65 and older raw value` +  `% American Indian and Alaskan Native raw value` +
                `% below 18 years of age raw value` + `% Females raw value` + 
                `% Native Hawaiian/Other Pacific Islander raw value` +  `% Non-Hispanic African American raw value` +
                `% Hispanic raw value` +  `% Asian raw value` + `% not proficient in English raw value`+ `% Rural raw value` +
                
                `Unemployment raw value` + `Some college raw value` + `Median household income raw value` + 
                `Children in poverty raw value` + `Children in single-parent households raw value`
              | fips_clean | 0 | fips_clean, health.dat2)
summary(twfe3)


results.twfe.full <- as.data.frame(summary(twfe3)$coefficients) %>% mutate(model = 'TWFE full') %>% 
  rownames_to_column()



# combining models ----
df_list <- list(results.twfe.arrests, results.twfe.demo, results.twfe.full)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods, here::here("rda","fe_poorfair_current"))


write.csv(all.mods, here::here("Output","fe_poorfair_current.csv"))

                  

