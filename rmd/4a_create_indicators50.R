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

# Remove counties that aren't included in trajectory analysis and removing variables that aren't needed
combo.dat6 <- combo.dat5 %>% 
  filter(is.na(Grp1)==0) %>% 
  select(-METDIVFP,-CSAFP,-CBSAFP, -`Segregation index`, -in.t, -County.y, -County.x, -id.y,-`County Code`,-geo_name,
         -in.s,-id.x,-LSAD,-CLASSFP, -MTFCC,-FUNCSTAT,-ALAND, -AWATER, -INTPTLAT, -INTPTLON, -fips, -FIPS, -geometry,
         -total3and4, -COUNTYFP, -COUNTYNS, -STATEFP,-Name,-NAME,-NAMELSAD,-`Geographic Area Name`,-GEOID, -total.households,
         -total.rentburden, -Deaths, -prek) %>% 
  rename(segregation_white_nonwhite = `Segregation Index`,
         deaths_age_adj_rate2017 = `Age Adjusted Rate`)



# Checking for missing data
#summary(combo.dat6)
#visdat::vis_dat(combo.dat5)
#visdat::vis_miss(combo.dat5)
check.miss <- as.data.frame(100 * (apply(apply(combo.dat6, 2, is.na), 2, sum)/nrow(combo.dat6)))

#check.miss <-filter(combo.dat5, is.na(nbanks)==1)
#as.data.frame(table(check.miss$state_abb, useNA = "always"))


#dat_na_rm <- na.omit(dat)
#100 * (nrow(dat_na_rm)/nrow(dat))


#dat <- na.omit(dat)  #consider deleting missing values


#### Discretize continuous variables by quartiles
combo.dat7 <- combo.dat6 %>% 
  mutate(bankrate = nbanks/pop2017,
         
         bankrate2017.na = as.numeric(is.na(bankrate)),
         bankrate2017.gtp50 = ifelse(is.na(bankrate) == 0 & bankrate > quantile(bankrate, .50, na.rm = TRUE),1,0),
 
         poverty_2017.ltp50 = poverty_2017 < quantile(poverty_2017,.50, na.rm = TRUE),
         
         unemployment_rate_2017.ltp50 = unemployment_rate_2017 < quantile(unemployment_rate_2017, .50, na.rm = TRUE),

         median_household_income_2017.gtp50 = median_household_income_2017 > quantile(median_household_income_2017, .50, na.rm = TRUE),

         some_college_2017.gtp50 = some_college_2017 > quantile(some_college_2017, .50, na.rm = TRUE),

         hs_grad_2017.gtp50 = hs_grad_2017 > quantile(hs_grad_2017, .50, na.rm = TRUE),

         broadband_2017.gtp50 = broadband_2017 > quantile(broadband_2017, .50, na.rm = TRUE),

         disconnected.2017.na = as.numeric(is.na(`% Disconnected Youth`)),
         disconnected.2017.ltp50 = ifelse(is.na(`% Disconnected Youth`) == 0 & `% Disconnected Youth` < quantile(`% Disconnected Youth`, .50, na.rm = TRUE),1,0),

        food.2017.na = as.numeric(is.na(food)),
        food.2017.gtp50 = ifelse(is.na(food) == 0 & food > quantile(food, .50, na.rm = TRUE),1,0),

        violent.2017.na = as.numeric(is.na(`Violent Crime Rate`)),
        violent.2017.ltp50 = ifelse(is.na(`Violent Crime Rate`) == 0 & `Violent Crime Rate` < quantile(`Violent Crime Rate`, .50, na.rm = TRUE),1,0),

        segregation.2017.na = as.numeric(is.na(segregation_white_nonwhite)),
        segregation.2017.ltp50 = ifelse(is.na(segregation_white_nonwhite) == 0 & segregation_white_nonwhite < quantile(segregation_white_nonwhite, .50, na.rm = TRUE),1,0),

      pcp_2017.gtp50 = `PCP Rate` > quantile(`PCP Rate`, .50, na.rm = TRUE),

      lbw_2017.ltp50 = `% LBW` < quantile(`% LBW`, .50, na.rm = TRUE),

      
      income_ratio_2017.ltp50 = `Income Ratio` < quantile(`Income Ratio`, .50, na.rm = TRUE),

      
      uninsured_2017.ltp50 = `% Uninsured` < quantile(`% Uninsured`, .50, na.rm = TRUE),

      
      under18_2017.ltp25 = `% < 18` < quantile(`% < 18`, .25, na.rm = TRUE),
      under18_2017.gtp75 = `% < 18` > quantile(`% < 18`, .75, na.rm = TRUE),
      under18_2017.p25top75 = `% < 18` <= quantile(`% < 18`, .75, na.rm = TRUE) 
      & `% < 18` >= quantile(`% < 18`,.25, na.rm = TRUE),
      
      
      over64_2017.ltp25 = `% 65 and over` < quantile(`% 65 and over`, .25, na.rm = TRUE),
      over64_2017.gtp75 = `% 65 and over` > quantile(`% 65 and over`, .75, na.rm = TRUE),
      over64_2017.p25top75 = `% 65 and over` <= quantile(`% 65 and over`, .75, na.rm = TRUE) 
      & `% 65 and over` >= quantile(`% 65 and over`,.25, na.rm = TRUE),
      
      
      black_2017.ltp25 = `% African American` < quantile(`% African American`, .25, na.rm = TRUE),
      black_2017.gtp75 = `% African American` > quantile(`% African American`, .75, na.rm = TRUE),
      black_2017.p25top75 = `% African American` <= quantile(`% African American`, .75, na.rm = TRUE) 
      & `% African American` >= quantile(`% African American`,.25, na.rm = TRUE),
      
      
      native_2017.ltp25 = `% American Indian/Alaskan Native` < quantile(`% American Indian/Alaskan Native`, .25, na.rm = TRUE),
      native_2017.gtp75 = `% American Indian/Alaskan Native` > quantile(`% American Indian/Alaskan Native`, .75, na.rm = TRUE),
      native_2017.p25top75 = `% American Indian/Alaskan Native` <= quantile(`% American Indian/Alaskan Native`, .75, na.rm = TRUE) 
      & `% American Indian/Alaskan Native` >= quantile(`% American Indian/Alaskan Native`,.25, na.rm = TRUE),
      
      
      pacific_islander_2017.ltp25 = `% Native Hawaiian/Other Pacific Islander` < quantile(`% Native Hawaiian/Other Pacific Islander`, .25, na.rm = TRUE),
      pacific_islander_2017.gtp75 = `% Native Hawaiian/Other Pacific Islander` > quantile(`% Native Hawaiian/Other Pacific Islander`, .75, na.rm = TRUE),
      pacific_islander_2017.p25top75 = `% Native Hawaiian/Other Pacific Islander` <= quantile(`% Native Hawaiian/Other Pacific Islander`, .75, na.rm = TRUE) 
      & `% Native Hawaiian/Other Pacific Islander` >= quantile(`% Native Hawaiian/Other Pacific Islander`,.25, na.rm = TRUE),
      
      
      hispanic_2017.ltp25 = `% Hispanic` < quantile(`% Hispanic`, .25, na.rm = TRUE),
      hispanic_2017.gtp75 = `% Hispanic` > quantile(`% Hispanic`, .75, na.rm = TRUE),
      hispanic_2017.p25top75 = `% Hispanic` <= quantile(`% Hispanic`, .75, na.rm = TRUE) 
      & `% Hispanic` >= quantile(`% Hispanic`,.25, na.rm = TRUE),
      
      
      white_2017.ltp25 = `% Non-Hispanic White` < quantile(`% Non-Hispanic White`, .25, na.rm = TRUE),
      white_2017.gtp75 = `% Non-Hispanic White` > quantile(`% Non-Hispanic White`, .75, na.rm = TRUE),
      white_2017.p25top75 = `% Non-Hispanic White` <= quantile(`% Non-Hispanic White`, .75, na.rm = TRUE) 
      & `% Non-Hispanic White` >= quantile(`% Non-Hispanic White`,.25, na.rm = TRUE),
      
      
      lim_eng_2017.ltp25 = `% Not Proficient in English` < quantile(`% Not Proficient in English`, .25, na.rm = TRUE),
      lim_eng_2017.gtp75 = `% Not Proficient in English` > quantile(`% Not Proficient in English`, .75, na.rm = TRUE),
      lim_eng_2017.p25top75 = `% Not Proficient in English` <= quantile(`% Not Proficient in English`, .75, na.rm = TRUE) 
      & `% Not Proficient in English` >= quantile(`% Not Proficient in English`,.25, na.rm = TRUE),
      
      
      female_2017.ltp25 = `% Female` < quantile(`% Female`, .25, na.rm = TRUE),
      female_2017.gtp75 = `% Female` > quantile(`% Female`, .75, na.rm = TRUE),
      female_2017.p25top75 = `% Female` <= quantile(`% Female`, .75, na.rm = TRUE) 
      & `% Female` >= quantile(`% Female`,.25, na.rm = TRUE),
      
      
      rural_2017.ltp25 = `% Rural` < quantile(`% Rural`, .25, na.rm = TRUE),
      rural_2017.gtp75 = `% Rural` > quantile(`% Rural`, .75, na.rm = TRUE),
      rural_2017.p25top75 = `% Rural` <= quantile(`% Rural`, .75, na.rm = TRUE) 
      & `% Rural` >= quantile(`% Rural`,.25, na.rm = TRUE),
      
      
      prek_2017.gtp50 = prekpct > quantile(prekpct, .50, na.rm = TRUE),

      
      rentburden_2017.ltp50 = rentburden_pct < quantile(rentburden_pct, .50, na.rm = TRUE),

      deaths_2017.ltp50 = ifelse(is.na(deaths_age_adj_rate2017)==1 | deaths_age_adj_rate2017 < quantile(deaths_age_adj_rate2017, .50, na.rm = TRUE),1,0),

      incarcerated_2017.ltp50 = incarcerated_rate < quantile(incarcerated_rate, .50, na.rm = TRUE),

      voting_2017.gtp50 = voting_pct > quantile(voting_pct, .50, na.rm = TRUE),

      volunteer_2017.gtp50 = volunteer_pct > quantile(volunteer_pct, .50, na.rm = TRUE)) %>% 

  fastDummies::dummy_cols(select_columns = c("region", "country_division"))

# Convert logical values to numeric binary variables
cols <- sapply(combo.dat7, is.logical)
combo.dat7[,cols] <- lapply(combo.dat7[,cols], as.numeric)
    

# Descriptive statistics for data set with indicators
library(skimr)
skim(combo.dat7)

check.miss <- as.data.frame(100 * (apply(apply(combo.dat7, 2, is.na), 2, sum)/nrow(combo.dat7)))

        
# Save final variable importance analysis file
saveRDS(combo.dat7, here::here("rda","analysis_vi_2017_50th.rds"))

