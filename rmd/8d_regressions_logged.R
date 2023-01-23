

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(ggcorrplot)


# read in data ----
health2010 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2010.rds")
health2019 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2019.rds")


# merge two data sets ----
health.combo1 <- merge(health2010,health2019, 
                       by= c("fips_clean", "rate2000","rate2010","rate2019","n_young_adults2000",
                             "n_young_adults2010", "n_young_adults2019"), 
                       suffixes = c("2010","2019")) %>% janitor::clean_names()

# 2000 models ----

#### just demography 
reg2000mod.demography  <- lm(rate2000 ~  
  population_raw_value +
  percent_below_18_years_of_age_raw_value+
  percent_65_and_older_raw_value +
  percent_american_indian_and_alaskan_native_raw_value +
  percent_native_hawaiian_other_pacific_islander_raw_value +
  percent_not_proficient_in_english_raw_value +
  percent_females_raw_value +
  percent_rural_raw_value, 
  data =health.combo1 )
results.2000.demography <- as.data.frame(summary(reg2000mod.demography)$coefficients) %>% mutate(model = 'demography2000') %>% 
  rownames_to_column()




#### Health outcomes
reg2000mod.outcomes    <- lm(rate2000 ~  
                    low_birthweight_raw_value2010 +
                    premature_death_raw_value2010 +
                      population_raw_value +
                      percent_below_18_years_of_age_raw_value+
                      percent_65_and_older_raw_value +
                      percent_american_indian_and_alaskan_native_raw_value +
                      percent_native_hawaiian_other_pacific_islander_raw_value +
                      percent_not_proficient_in_english_raw_value +
                      percent_females_raw_value +
                      percent_rural_raw_value, 
                    data = health.combo1  )
results.2000.outcomes <- as.data.frame(summary(reg2000mod.outcomes)$coefficients) %>% mutate(model = 'outcomes2000') %>% 
  rownames_to_column()



#### Health behaviors

reg2000mod.behaviors <- lm(rate2000 ~ 
                    adult_smoking_raw_value2019 + 
                    teen_births_raw_value2010 +
                    teen_births_raw_value2019 +
                    chlamydia_rate_raw_value +
                    sexually_transmitted_infections_raw_value +
                    access_to_exercise_opportunities_raw_value +
                      population_raw_value +
                      percent_below_18_years_of_age_raw_value+
                      percent_65_and_older_raw_value +
                      percent_american_indian_and_alaskan_native_raw_value +
                      percent_native_hawaiian_other_pacific_islander_raw_value +
                      percent_not_proficient_in_english_raw_value +
                      percent_females_raw_value +
                      percent_rural_raw_value, 
                    data =health.combo1 )
results.2000.behaviors <- as.data.frame(summary(reg2000mod.behaviors)$coefficients) %>% mutate(model = 'behaviors2000') %>% 
  rownames_to_column()




#### Clinical access
reg2000mod.access <- lm(rate2000 ~ 
                    ratio_of_population_to_primary_care_physicians +
                    ratio_of_population_to_primary_care_physicians +
                    uninsured_adults_raw_value2019 +
                    ratio_of_population_to_dentists +
                    ratio_of_population_to_primary_care_providers_other_than_physicians +
                      population_raw_value +
                      percent_below_18_years_of_age_raw_value+
                      percent_65_and_older_raw_value +
                      percent_american_indian_and_alaskan_native_raw_value +
                      percent_native_hawaiian_other_pacific_islander_raw_value +
                      percent_not_proficient_in_english_raw_value +
                      percent_females_raw_value +
                      percent_rural_raw_value, 
                    data =health.combo1 )
results.2000.access <- as.data.frame(summary(reg2000mod.access)$coefficients) %>% mutate(model = 'access2000') %>% 
  rownames_to_column()




#### SES

reg2000mod.ses <- lm(rate2000 ~ 
                    high_school_graduation_raw_value2010  +
                    high_school_graduation_raw_value2019  +
                    single_parent_households_raw_value +
                    children_in_single_parent_households_raw_value +
                    children_eligible_for_free_or_reduced_price_lunch_raw_value +
                    income_inequality_raw_value2010 + 
                      population_raw_value +
                      percent_below_18_years_of_age_raw_value+
                      percent_65_and_older_raw_value +
                      percent_american_indian_and_alaskan_native_raw_value +
                      percent_native_hawaiian_other_pacific_islander_raw_value +
                      percent_not_proficient_in_english_raw_value +
                      percent_females_raw_value +
                      percent_rural_raw_value, 
                    data =health.combo1 )
results.2000.ses <- as.data.frame(summary(reg2000mod.ses)$coefficients) %>% mutate(model = 'ses2000') %>% 
  rownames_to_column()




#### Environment
reg2000mod.environment <- lm(rate2000 ~ 
                    air_pollution_particulate_matter_raw_value +
                    long_commute_driving_alone_raw_value +
                    homeownership_raw_value +
                    percentage_of_households_with_overcrowding +
                    severe_housing_problems_raw_value +
                      population_raw_value +
                      percent_below_18_years_of_age_raw_value+
                      percent_65_and_older_raw_value +
                      percent_american_indian_and_alaskan_native_raw_value +
                      percent_native_hawaiian_other_pacific_islander_raw_value +
                      percent_not_proficient_in_english_raw_value +
                      percent_females_raw_value +
                      percent_rural_raw_value, 
                    data =health.combo1 )
results.2000.environment <- as.data.frame(summary(reg2000mod.environment)$coefficients) %>% mutate(model = 'environment2000') %>% 
  rownames_to_column()




#### Full
reg2000mod.full <- lm(rate2000 ~ 
                    air_pollution_particulate_matter_raw_value +
                    high_school_graduation_raw_value2010  +
                    high_school_graduation_raw_value2019  +
                    adult_smoking_raw_value2019 + 
                    ratio_of_population_to_primary_care_physicians +
                    ratio_of_population_to_primary_care_physicians +
                    single_parent_households_raw_value +
                    long_commute_driving_alone_raw_value +
                    uninsured_adults_raw_value2019 +
                    teen_births_raw_value2010 +
                    teen_births_raw_value2019 +
                    chlamydia_rate_raw_value +
                    low_birthweight_raw_value2010 +
                    children_in_single_parent_households_raw_value +
                    sexually_transmitted_infections_raw_value +
                    access_to_exercise_opportunities_raw_value +
                    children_eligible_for_free_or_reduced_price_lunch_raw_value +
                    income_inequality_raw_value2010 + 
                    homeownership_raw_value +
                    percentage_of_households_with_overcrowding +
                    severe_housing_problems_raw_value +
                    ratio_of_population_to_dentists +
                    ratio_of_population_to_primary_care_providers_other_than_physicians +
                    premature_death_raw_value2010 + # 2010 value
                      population_raw_value +
                      percent_below_18_years_of_age_raw_value+
                      percent_65_and_older_raw_value +
                      percent_american_indian_and_alaskan_native_raw_value +
                      percent_native_hawaiian_other_pacific_islander_raw_value +
                      percent_not_proficient_in_english_raw_value +
                      percent_females_raw_value +
                      percent_rural_raw_value, 
                    data =health.combo1 )
results.2000.full <- as.data.frame(summary(reg2000mod.full)$coefficients) %>% mutate(model = 'full2000') %>% 
  rownames_to_column()




# 2010 models ----
#### just demography 
reg2010mod.demography  <- lm(log(rate2010) ~  
                               population_raw_value +
                               percent_below_18_years_of_age_raw_value+
                               percent_65_and_older_raw_value +
                               percent_american_indian_and_alaskan_native_raw_value +
                               percent_native_hawaiian_other_pacific_islander_raw_value +
                               percent_not_proficient_in_english_raw_value +
                               percent_females_raw_value +
                               percent_rural_raw_value, 
                             data =health.combo1 )
results.2010.demography <- as.data.frame(summary(reg2010mod.demography)$coefficients) %>% mutate(model = 'demography2010') %>% 
  rownames_to_column()




#### Health outcomes
reg2010mod.outcomes    <- lm(log(rate2010) ~  
                               low_birthweight_raw_value2010 +
                               premature_death_raw_value2010 +
                               population_raw_value +
                               percent_below_18_years_of_age_raw_value+
                               percent_65_and_older_raw_value +
                               percent_american_indian_and_alaskan_native_raw_value +
                               percent_native_hawaiian_other_pacific_islander_raw_value +
                               percent_not_proficient_in_english_raw_value +
                               percent_females_raw_value +
                               percent_rural_raw_value, 
                             data = health.combo1  )
results.2010.outcomes <- as.data.frame(summary(reg2010mod.outcomes)$coefficients) %>% mutate(model = 'outcomes2010') %>% 
  rownames_to_column()



#### Health behaviors

reg2010mod.behaviors <- lm(log(rate2010) ~ 
                             adult_smoking_raw_value2019 + 
                             teen_births_raw_value2010 +
                             teen_births_raw_value2019 +
                             chlamydia_rate_raw_value +
                             sexually_transmitted_infections_raw_value +
                             access_to_exercise_opportunities_raw_value +
                             population_raw_value +
                             percent_below_18_years_of_age_raw_value+
                             percent_65_and_older_raw_value +
                             percent_american_indian_and_alaskan_native_raw_value +
                             percent_native_hawaiian_other_pacific_islander_raw_value +
                             percent_not_proficient_in_english_raw_value +
                             percent_females_raw_value +
                             percent_rural_raw_value, 
                           data =health.combo1 )
results.2010.behaviors <- as.data.frame(summary(reg2010mod.behaviors)$coefficients) %>% mutate(model = 'behaviors2010') %>% 
  rownames_to_column()




#### Clinical access
reg2010mod.access <- lm(log(rate2010) ~ 
                          ratio_of_population_to_primary_care_physicians +
                          ratio_of_population_to_primary_care_physicians +
                          uninsured_adults_raw_value2019 +
                          ratio_of_population_to_dentists +
                          ratio_of_population_to_primary_care_providers_other_than_physicians +
                          population_raw_value +
                          percent_below_18_years_of_age_raw_value+
                          percent_65_and_older_raw_value +
                          percent_american_indian_and_alaskan_native_raw_value +
                          percent_native_hawaiian_other_pacific_islander_raw_value +
                          percent_not_proficient_in_english_raw_value +
                          percent_females_raw_value +
                          percent_rural_raw_value, 
                        data =health.combo1 )
results.2010.access <- as.data.frame(summary(reg2010mod.access)$coefficients) %>% mutate(model = 'access2010') %>% 
  rownames_to_column()




#### SES

reg2010mod.ses <- lm(log(rate2010) ~ 
                       high_school_graduation_raw_value2010  +
                       high_school_graduation_raw_value2019  +
                       single_parent_households_raw_value +
                       children_in_single_parent_households_raw_value +
                       children_eligible_for_free_or_reduced_price_lunch_raw_value +
                       income_inequality_raw_value2010 + 
                       population_raw_value +
                       percent_below_18_years_of_age_raw_value+
                       percent_65_and_older_raw_value +
                       percent_american_indian_and_alaskan_native_raw_value +
                       percent_native_hawaiian_other_pacific_islander_raw_value +
                       percent_not_proficient_in_english_raw_value +
                       percent_females_raw_value +
                       percent_rural_raw_value, 
                     data =health.combo1 )
results.2010.ses <- as.data.frame(summary(reg2010mod.ses)$coefficients) %>% mutate(model = 'ses2010') %>% 
  rownames_to_column()




#### Environment
reg2010mod.environment <- lm(log(rate2010) ~ 
                               air_pollution_particulate_matter_raw_value +
                               long_commute_driving_alone_raw_value +
                               homeownership_raw_value +
                               percentage_of_households_with_overcrowding +
                               severe_housing_problems_raw_value +
                               population_raw_value +
                               percent_below_18_years_of_age_raw_value+
                               percent_65_and_older_raw_value +
                               percent_american_indian_and_alaskan_native_raw_value +
                               percent_native_hawaiian_other_pacific_islander_raw_value +
                               percent_not_proficient_in_english_raw_value +
                               percent_females_raw_value +
                               percent_rural_raw_value, 
                             data =health.combo1 )
results.2010.environment <- as.data.frame(summary(reg2010mod.environment)$coefficients) %>% mutate(model = 'environment2010') %>% 
  rownames_to_column()



#### Full
reg2010mod.full <- lm(log(rate2010) ~ 
                        air_pollution_particulate_matter_raw_value +
                        high_school_graduation_raw_value2010  +
                        high_school_graduation_raw_value2019  +
                        adult_smoking_raw_value2019 + 
                        ratio_of_population_to_primary_care_physicians +
                        ratio_of_population_to_primary_care_physicians +
                        single_parent_households_raw_value +
                        long_commute_driving_alone_raw_value +
                        uninsured_adults_raw_value2019 +
                        teen_births_raw_value2010 +
                        teen_births_raw_value2019 +
                        chlamydia_rate_raw_value +
                        low_birthweight_raw_value2010 +
                        children_in_single_parent_households_raw_value +
                        sexually_transmitted_infections_raw_value +
                        access_to_exercise_opportunities_raw_value +
                        children_eligible_for_free_or_reduced_price_lunch_raw_value +
                        income_inequality_raw_value2010 + 
                        homeownership_raw_value +
                        percentage_of_households_with_overcrowding +
                        severe_housing_problems_raw_value +
                        ratio_of_population_to_dentists +
                        ratio_of_population_to_primary_care_providers_other_than_physicians +
                        premature_death_raw_value2010 + # 2010 value
                        population_raw_value +
                        percent_below_18_years_of_age_raw_value+
                        percent_65_and_older_raw_value +
                        percent_american_indian_and_alaskan_native_raw_value +
                        percent_native_hawaiian_other_pacific_islander_raw_value +
                        percent_not_proficient_in_english_raw_value +
                        percent_females_raw_value +
                        percent_rural_raw_value, 
                      data =health.combo1 )
results.2010.full <- as.data.frame(summary(reg2010mod.full)$coefficients) %>% mutate(model = 'full2010') %>% 
  rownames_to_column()









# 2019 models ----                  
#### just demography 
reg2019mod.demography  <- lm(log(rate2019) ~  
                               population_raw_value +
                               percent_below_18_years_of_age_raw_value+
                               percent_65_and_older_raw_value +
                               percent_american_indian_and_alaskan_native_raw_value +
                               percent_native_hawaiian_other_pacific_islander_raw_value +
                               percent_not_proficient_in_english_raw_value +
                               percent_females_raw_value +
                               percent_rural_raw_value, 
                             data =health.combo1 )
results.2019.demography <- as.data.frame(summary(reg2019mod.demography)$coefficients) %>% mutate(model = 'demography2019') %>% 
  rownames_to_column()




#### Health outcomes
reg2019mod.outcomes    <- lm(log(rate2019) ~  
                               low_birthweight_raw_value2010 +
                               premature_death_raw_value2010 +
                               population_raw_value +
                               percent_below_18_years_of_age_raw_value+
                               percent_65_and_older_raw_value +
                               percent_american_indian_and_alaskan_native_raw_value +
                               percent_native_hawaiian_other_pacific_islander_raw_value +
                               percent_not_proficient_in_english_raw_value +
                               percent_females_raw_value +
                               percent_rural_raw_value, 
                             data = health.combo1  )
results.2019.outcomes <- as.data.frame(summary(reg2019mod.outcomes)$coefficients) %>% mutate(model = 'outcomes2019') %>% 
  rownames_to_column()



#### Health behaviors

reg2019mod.behaviors <- lm(log(rate2019) ~ 
                             adult_smoking_raw_value2019 + 
                             teen_births_raw_value2010 +
                             teen_births_raw_value2019 +
                             chlamydia_rate_raw_value +
                             sexually_transmitted_infections_raw_value +
                             access_to_exercise_opportunities_raw_value +
                             population_raw_value +
                             percent_below_18_years_of_age_raw_value+
                             percent_65_and_older_raw_value +
                             percent_american_indian_and_alaskan_native_raw_value +
                             percent_native_hawaiian_other_pacific_islander_raw_value +
                             percent_not_proficient_in_english_raw_value +
                             percent_females_raw_value +
                             percent_rural_raw_value, 
                           data =health.combo1 )
results.2019.behaviors <- as.data.frame(summary(reg2019mod.behaviors)$coefficients) %>% mutate(model = 'behaviors2019') %>% 
  rownames_to_column()




#### Clinical access
reg2019mod.access <- lm(log(rate2019) ~ 
                          ratio_of_population_to_primary_care_physicians +
                          ratio_of_population_to_primary_care_physicians +
                          uninsured_adults_raw_value2019 +
                          ratio_of_population_to_dentists +
                          ratio_of_population_to_primary_care_providers_other_than_physicians +
                          population_raw_value +
                          percent_below_18_years_of_age_raw_value+
                          percent_65_and_older_raw_value +
                          percent_american_indian_and_alaskan_native_raw_value +
                          percent_native_hawaiian_other_pacific_islander_raw_value +
                          percent_not_proficient_in_english_raw_value +
                          percent_females_raw_value +
                          percent_rural_raw_value, 
                        data =health.combo1 )
results.2019.access <- as.data.frame(summary(reg2019mod.access)$coefficients) %>% mutate(model = 'access2019') %>% 
  rownames_to_column()




#### SES

reg2019mod.ses <- lm(log(rate2019) ~ 
                       high_school_graduation_raw_value2010  +
                       high_school_graduation_raw_value2019  +
                       single_parent_households_raw_value +
                       children_in_single_parent_households_raw_value +
                       children_eligible_for_free_or_reduced_price_lunch_raw_value +
                       income_inequality_raw_value2010 + 
                       population_raw_value +
                       percent_below_18_years_of_age_raw_value+
                       percent_65_and_older_raw_value +
                       percent_american_indian_and_alaskan_native_raw_value +
                       percent_native_hawaiian_other_pacific_islander_raw_value +
                       percent_not_proficient_in_english_raw_value +
                       percent_females_raw_value +
                       percent_rural_raw_value, 
                     data =health.combo1 )
results.2019.ses <- as.data.frame(summary(reg2019mod.ses)$coefficients) %>% mutate(model = 'ses2019') %>% 
  rownames_to_column()



#### Environment
reg2019mod.environment <- lm(log(rate2019) ~ 
                               air_pollution_particulate_matter_raw_value +
                               long_commute_driving_alone_raw_value +
                               homeownership_raw_value +
                               percentage_of_households_with_overcrowding +
                               severe_housing_problems_raw_value +
                               population_raw_value +
                               percent_below_18_years_of_age_raw_value+
                               percent_65_and_older_raw_value +
                               percent_american_indian_and_alaskan_native_raw_value +
                               percent_native_hawaiian_other_pacific_islander_raw_value +
                               percent_not_proficient_in_english_raw_value +
                               percent_females_raw_value +
                               percent_rural_raw_value, 
                             data =health.combo1 )
results.2019.environment <- as.data.frame(summary(reg2019mod.environment)$coefficients) %>% mutate(model = 'environment2019') %>% 
  rownames_to_column()


#### Full
reg2019mod.full <- lm(log(rate2019) ~ 
                        air_pollution_particulate_matter_raw_value +
                        high_school_graduation_raw_value2010  +
                        high_school_graduation_raw_value2019  +
                        adult_smoking_raw_value2019 + 
                        ratio_of_population_to_primary_care_physicians +
                        ratio_of_population_to_primary_care_physicians +
                        single_parent_households_raw_value +
                        long_commute_driving_alone_raw_value +
                        uninsured_adults_raw_value2019 +
                        teen_births_raw_value2010 +
                        teen_births_raw_value2019 +
                        chlamydia_rate_raw_value +
                        low_birthweight_raw_value2010 +
                        children_in_single_parent_households_raw_value +
                        sexually_transmitted_infections_raw_value +
                        access_to_exercise_opportunities_raw_value +
                        children_eligible_for_free_or_reduced_price_lunch_raw_value +
                        income_inequality_raw_value2010 + 
                        homeownership_raw_value +
                        percentage_of_households_with_overcrowding +
                        severe_housing_problems_raw_value +
                        ratio_of_population_to_dentists +
                        ratio_of_population_to_primary_care_providers_other_than_physicians +
                        premature_death_raw_value2010 + # 2010 value
                        population_raw_value +
                        percent_below_18_years_of_age_raw_value+
                        percent_65_and_older_raw_value +
                        percent_american_indian_and_alaskan_native_raw_value +
                        percent_native_hawaiian_other_pacific_islander_raw_value +
                        percent_not_proficient_in_english_raw_value +
                        percent_females_raw_value +
                        percent_rural_raw_value, 
                      data =health.combo1 )
results.2019.full <- as.data.frame(summary(reg2019mod.full)$coefficients) %>% mutate(model = 'full2019') %>% 
  rownames_to_column()



res <- resid(reg2019mod.full)
res

plot(fitted(reg2019mod.full), res)

# add a horizontal line at 0 
abline(0,0)


                  
# combining models ----
df_list2000 <- list(results.2000.demography, results.2000.outcomes, results.2000.behaviors, 
                    results.2000.access, results.2000.ses, results.2000.environment, results.2000.full)

df_list2010 <- list(results.2010.demography, results.2010.outcomes, results.2010.behaviors, 
                    results.2010.access, results.2010.ses, results.2010.environment, results.2010.full)


df_list2019 <- list(results.2019.demography, results.2019.outcomes, results.2019.behaviors, 
                    results.2019.access, results.2019.ses, results.2019.environment, results.2019.full)


all.mods2000 <- df_list2000 %>% reduce(full_join, by='rowname')
all.mods2010 <- df_list2010 %>% reduce(full_join, by='rowname')
all.mods2019 <- df_list2019 %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods2000, here::here("rda","health2000resultslog.rds"))
saveRDS(all.mods2010, here::here("rda","health2010resultslog.rds"))
saveRDS(all.mods2019, here::here("rda","health2019resultslog.rds"))


write.csv(all.mods2000, here::here("Output","health2000resultslog.csv"))
write.csv(all.mods2010, here::here("Output","health2010resultslog.csv"))
write.csv(all.mods2019, here::here("Output","health2019resultslog.csv"))

                  

