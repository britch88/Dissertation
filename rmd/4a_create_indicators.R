library(tidyverse)

#### Read in Data sets
countycomplete <- readRDS()


#Merge County predictors with other county-level file
# reading in geo file
analysis.geo1 <- as.data.frame(readRDS(here::here("rda","analysis.geo.rds")))

#put all data frames into list
#df_list <- list(analysis.geo1, dat2, dat2.cbp2019)
#<- df_list %>% reduce(full_join, by='fips_clean')

merge1 <- merge(analysis.geo1, dat2, by = "fips_clean", all = TRUE)

#merge all data frames in list
analysis.dat.combo <- merge(merge1, dat2.cbp2019, by = "fips_clean", all = TRUE)



# Checking for missing data
summary(analysis.dat.combo)
visdat::vis_dat(analysis.dat.combo)
visdat::vis_miss(analysis.dat.combo)
100 * (apply(apply(analysis.dat.combo, 2, is.na), 2, sum)/nrow(analysis.dat.combo))

check.miss <-filter(analysis.dat.combo, is.na(nbanks)==1)
as.data.frame(table(check.miss$state_abb, useNA = "always"))


dat_na_rm <- na.omit(dat)
100 * (nrow(dat_na_rm)/nrow(dat))
```

For demonstration purposes, we omit all the missing observations from the data.

```{r}
dat <- na.omit(dat)
```


# Decide how to discretize continuous variables
```{r}
analysis.dat.combo2 <- analysis.dat.combo %>% 
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


```



# Save final variable importance analysis file
```{r}
analysis.vi <- analysis.dat.combo2
saveRDS(analysis.vi, here::here("rda","analysis.vi.rds"))
```

