library(tidyverse)
library(guf)  # for "right" function

#### Read in Data sets
countycomplete2017 <- readRDS(here::here("rda","countycomplete2017.rds"))
rentburden2017 <- readRDS(here::here("rda","rentburden2017.rds"))
incarceration2017 <- readRDS(here::here("rda","incarceration2017.rds"))
voting2016 <- readRDS(here::here("rda","voting2016clean.rds"))
volunteer2017 <- readRDS(here::here("rda","volunteer2017clean.rds"))
cbp2017 <- readRDS(here::here("rda","cbp2017clean.rds"))
mortality2017 <- readRDS(here::here("rda","mortality2017clean.rds"))
preschool2017 <- readRDS(here::here("rda","preschool2017.rds"))
rwj2017 <- readRDS(here::here("rda","rwj2017.rds"))
analysis.geo <- as.data.frame(readRDS(here::here("rda","analysis.geo.rds")))




#### Standardize merge variables
rentburden2017 <- rentburden2017 %>%  mutate(fips_clean = right(id,5))
incarceration2017 <- incarceration2017 %>% mutate(state = tolower(state))
voting2016 <- voting2016 %>% mutate(state = tolower(state))
volunteer2017 <- volunteer2017 %>% mutate(state_abb = state) %>% select(-state)
mortality2017 <- mortality2017 %>% mutate(fips_clean = `County Code`) 
preschool2017 <- preschool2017 %>%  mutate(fips_clean = right(id,5))
rwj2017 <- rwj2017 %>%  mutate(fips_clean = FIPS) %>% select(-State)
countycomplete2017 <- countycomplete2017 %>% select(-state)

#### Merge to geo file 
df_list <- list(rentburden2017, cbp2017, rwj2017, preschool2017, mortality2017, countycomplete2017)
combo.dat1 <- df_list %>% reduce(full_join, by='fips_clean')

combo.dat2 <- merge(analysis.geo, combo.dat1, by = "fips_clean", all= TRUE)

combo.dat3 <- merge(combo.dat2, incarceration2017, by = "state", all= TRUE)

combo.dat4 <- merge(combo.dat3, voting2016, by = "state", all = TRUE)

combo.dat5 <- merge(combo.dat4, volunteer2017, by = "state_abb", all = TRUE)



# Checking for missing data
summary(combo.dat5)
visdat::vis_dat(combo.dat5)
visdat::vis_miss(combo.dat5)
100 * (apply(apply(combo.dat5, 2, is.na), 2, sum)/nrow(combo.dat5))

check.miss <-filter(combo.dat5, is.na(nbanks)==1)
as.data.frame(table(check.miss$state_abb, useNA = "always"))


dat_na_rm <- na.omit(dat)
100 * (nrow(dat_na_rm)/nrow(dat))


#dat <- na.omit(dat)  #consider deleting missing values


#### Discretize continuous variables by quartiles
analysis.indicators <- analysis.dat.combo %>% 
  mutate(bankrate = nbanks/pop2017,
         
         bankrate.na = as.numeric(is.na(bankrate)),
         bankrate.ltp25 = ifelse(as.numeric(is.na(bankrate)) == 1,0, ifelse(
           bankrate < quantile(bankrate, .25, na.rm = TRUE),1,0)),
         bankrate.gtp75 = ifelse(is.na(bankrate) == 0, bankrate > quantile(bankrate, .75, na.rm = TRUE),0),
         bankrate.p25top75 = ifelse(is.na(bankrate) == 0, bankrate <= quantile(bankrate, .75, na.rm = TRUE) 
                                    & bankrate >= quantile(bankrate,.25, na.rm = TRUE),0),
         
         poverty_2017.ltp25 = poverty_2017 < quantile(poverty_2017, .25, na.rm = TRUE),
         poverty_2017.gtp75 = poverty_2017 > quantile(poverty_2017,.75, na.rm = TRUE),
         poverty_2017.p25top75 = poverty_2017 <= quantile(poverty_2017, .75, na.rm = TRUE) 
         & poverty_2017 >= quantile(poverty_2017,.25, na.rm = TRUE),
         
         unemployment_rate_2017.ltp25 = unemployment_rate_2017 < quantile(unemployment_rate_2017, .25, na.rm = TRUE),
         unemployment_rate_2017.gtp75 = unemployment_rate_2017 > quantile(unemployment_rate_2017, .75, na.rm = TRUE),
         unemployment_rate_2017.p25top75 =  unemployment_rate_2017 <= quantile( unemployment_rate_2017, .75, na.rm = TRUE) 
         &  unemployment_rate_2017 >= quantile( unemployment_rate_2017,.25, na.rm = TRUE),
         
         median_household_income_2017.ltp25 = median_household_income_2017 < quantile(median_household_income_2017, .25, na.rm = TRUE),
         median_household_income_2017.gtp75 = median_household_income_2017 > quantile(median_household_income_2017, .75, na.rm = TRUE),
         median_household_income_2017.p25top75 = median_household_income_2017 <= quantile(median_household_income_2017, .75, na.rm = TRUE) 
         & median_household_income_2017 >= quantile(median_household_income_2017,.25, na.rm = TRUE),
         
         some_college_2017.ltp25 = some_college_2017 < quantile(some_college_2017, .25, na.rm = TRUE),
         some_college_2017.gtp75 = some_college_2017 > quantile(some_college_2017, .75, na.rm = TRUE),
         some_college_2017.p25top75 = some_college_2017 <= quantile(some_college_2017, .75, na.rm = TRUE) 
         & some_college_2017 >= quantile(some_college_2017,.25, na.rm = TRUE),
         
         hs_grad_2017.ltp25 = hs_grad_2017 < quantile(hs_grad_2017, .25, na.rm = TRUE),
         hs_grad_2017.gtp75 = hs_grad_2017 > quantile(hs_grad_2017, .75, na.rm = TRUE),
         hs_grad_2017.p25top75 = hs_grad_2017 <= quantile(hs_grad_2017, .75, na.rm = TRUE) 
         & hs_grad_2017 >= quantile(hs_grad_2017,.25, na.rm = TRUE),
         
         broadband_2017.ltp25 = broadband_2017 < quantile(broadband_2017, .25, na.rm = TRUE),
         broadband_2017.gtp75 = broadband_2017 > quantile(broadband_2017, .75, na.rm = TRUE),
         broadband_2017.p25top75 = broadband_2017 <= quantile(broadband_2017, .75, na.rm = TRUE) 
         & broadband_2017 >= quantile(broadband_2017,.25, na.rm = TRUE))





# Save final variable importance analysis file
analysis.vi <- analysis.dat.combo2
saveRDS(analysis.vi, here::here("rda","analysis.vi.rds"))

