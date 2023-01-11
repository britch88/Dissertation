library(tidyverse)

# Data needed:

##arrest rate data for 2000 and 2019
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")
rate2000 <- filter(arrestdat, 1999 <= year & year <= 2001) %>% 
  group_by(fips_clean, state) %>% 
  summarise(rate2000 = mean(young_adult_rate)) %>% 
  ungroup()

rate2019 <- filter(arrestdat, 2017 <= year & year <= 2019) %>% 
  group_by(fips_clean, state) %>% 
  summarise(rate2019 = mean(young_adult_rate)) %>% 
  ungroup()

rate2000t2019 <- merge(rate2000,rate2019, by=c("fips_clean","state"))

county.change <- mutate(rate2000t2019, 
                        county.change = rate2019 - rate2000) %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico'))



## metropolitan status 2013
metstat2013 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/metstat2013.rds")


## data in 2017 file: % non-white 2017, income ratio 2017
analysis_vi_2017 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/analysis_vi_2017.rds")

income.nonwhite2017 <- analysis_vi_2017 %>% 
  mutate(nonwhite2017 = 100 - `% Non-Hispanic White`,
         income.ratio2017 = `Income Ratio`) %>% 
  select(fips_clean,nonwhite2017, income.ratio2017)


## income inequality 2000
income.ratio2000 <- readRDS( "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/income.ratio2000")



## % non-white 2000 + % in correctional facility 2000
dat2000whitecorrections <- readRDS( "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/white_corrections2000.rds")


## Correctional facility rate 2020
correctionspct2020 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/correctionspct2020.rds")



## % single headed 2000
familytype2000 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/familytype2000.rds")



## % single headed 2019
familytype2019 <-readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/familytype2019.rds")





## Poverty status 2000
povrate2000 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/povrate2000.rds")


### Poverty status 2019
povrate2019 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/povrate2019.rds")

#merge all data frames in list
df_list <- list(county.change,
                metstat2013,
                income.nonwhite2017,
                income.ratio2000,
                dat2000whitecorrections,
                correctionspct2020,
                familytype2000, 
                familytype2019, 
                povrate2000,
                povrate2019)

dataprep1 <- df_list %>% reduce(full_join, by='fips_clean')


# create a few more variables
dataprep2 <- dataprep1 %>% 
  mutate(nonwhite2000 = 100 - white.pct,
         corrections.pct2000 = corrections.pct) %>% 
  mutate(income.ratio2000 = as.numeric(income.ratio2000),
         povrate2019 = as.numeric(povrate2019),
         povrate.change = povrate2019 - povrate2000,
         income.ratio.change = income.ratio2017 - income.ratio2000,
         corrections.change = corrections.pct2020 - corrections.pct2000,
         nonwhite.change = nonwhite2017 - nonwhite2000,
         single.headed.change = single.headed2019 - single.headed2000)

summary(dataprep2)



##### Removing missing fips codes, territories, alaska, and hawaii
dataprep3 <- dataprep2 %>%
  filter(!fips_clean %in% c("0","00000","01000")) %>%  
  #filter(str_detect(fips_clean, '721'|'780'|'720'|'600'|'579')  == 0)
  filter(!grepl('721', fips_clean)) %>% 
  filter(!grepl('780', fips_clean)) %>% 
  filter(!grepl('720', fips_clean)) %>% 
  filter(!grepl('691', fips_clean)) %>% 
  filter(!grepl('600', fips_clean)) %>% 
  filter(!grepl('660', fips_clean)) %>% 
  filter(!grepl('579', fips_clean)) %>% 
  filter(rate2000 >0) %>% 
  filter(rate2019 >0) %>% 
  filter(!grepl('alaska',state)) %>% #alaska
  filter(!grepl('hawaii',state)) %>% #hawaii
  filter(is.na(income.ratio2017)==0) %>% 
  filter(is.na(corrections.pct2020)==0)


# After removing counties with missing rates for either 2000 or 2019 and removing HI and AK sample is reduced to 2,591
# and after removing missings from predictors down to 2,572

# Check for any remaining missings
sapply(dataprep3, function(x) sum(is.na(x)))

#saveRDS(decomp.analysisx3,"/data/share/xproject/Training/Practice/henderson/Dissertation/rda/decomposition_analysis.rds")

########################### Additional refinements to data

##### Remove counties that have less than 200 young adults or less than 1 arrest, in both timepoints
# Read in arrest rate data with independent vars
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")


# Grab ratio young to older adults
age_break2000 <- arrestdat %>% 
  ungroup() %>% 
  filter(year == 2000) %>% 
  group_by(fips_clean,year) %>% 
  mutate(proportion.young.adult2000 = n_young_adults/n_all_adults * 100,
         year = as.factor(year),
         n_young_adults2000 = n_young_adults,
         c_young_adult_tot2000 = c_young_adult_tot) %>% 
  ungroup() %>% 
  select(fips_clean,  proportion.young.adult2000,  n_young_adults2000, c_young_adult_tot2000)  

age_break2019 <- arrestdat %>% 
  ungroup() %>% 
  filter(year ==2019) %>% 
  group_by(fips_clean,year) %>% 
  mutate(proportion.young.adult2019 = n_young_adults/n_all_adults * 100,
         year = as.factor(year),
         n_young_adults2019 = n_young_adults,
         c_young_adult_tot2019 = c_young_adult_tot) %>% 
  ungroup() %>% 
  select(fips_clean,  proportion.young.adult2019, n_young_adults2019, c_young_adult_tot2019)  

age.break <- merge(age_break2000,age_break2019, by="fips_clean") %>% 
  filter(fips_clean != "0") %>% 
  mutate(young.adult.pop.change = proportion.young.adult2019 - proportion.young.adult2000)

summary(age.break)


# Merge decomposition data set and arrest data to select subset of eligible counties
### based on Light and Harris, counties with 200+ young adults and at least 1 arrest
dataprep4 <- merge(age.break, dataprep3, by= "fips_clean") %>%
  filter(n_young_adults2000 >200 & n_young_adults2019 > 200) %>%  # n = 2,446
  filter(c_young_adult_tot2000 > 0 & c_young_adult_tot2019 > 0)  #n = 2,339

summary(dataprep4)
# down to 2,325

# reading in and cleaning county adjacency file to identify counties with no neighbors
### data from: https://www.nber.org/research/data/county-adjacency
county_adjacency2010 <- read_csv("/data/share/xproject/Training/Practice/henderson/Dissertation/raw/Census Data/county_adjacency2010.csv")
county.x <- county_adjacency2010 %>% 
  mutate(fips_clean = fipsneighbor)

### removing neighbors that don't have arrests or that have less than 200 young adults
county.x2 <- merge(dataprep4, county.x, by= "fips_clean", all.x = TRUE, all.y = FALSE)

county.x3 <- county.x2 %>% 
  group_by(fipscounty) %>% 
  summarise(n.neighbors = n()) %>% 
  rename('fips_clean' = 'fipscounty') 

dataprep5 <- merge(dataprep4,county.x3, by="fips_clean", all.x=TRUE, all.y = FALSE) %>% 
  filter(n.neighbors > 3) # 2,182 observations


### Checking contents
summary(dataprep5)
state.check <- as.data.frame(table(dataprep5$state, useNA = "always"))
n_distinct(dataprep5$state)

## HI, AK, FL, IL, WI excluded

### save file
saveRDS(dataprep5,"/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_analysis_file.rds")


