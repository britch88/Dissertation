

# Data needed:

## main arrest rate data
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")
rate2000 <- filter(arrestdat, 1999 <= year & year <= 2001) %>% 
  group_by(fips_clean, state, Name) %>% 
  summarise(rate2000 = mean(young_adult_rate)) %>% 
  ungroup()

rate2019 <- filter(arrestdat, 2017 <= year & year <= 2019) %>% 
  group_by(fips_clean, state, Name) %>% 
  summarise(rate2019 = mean(young_adult_rate)) %>% 
  ungroup()

rate2000t2019 <- merge(rate2000,rate2019, by=c("fips_clean","Name","state"))

county.change <- mutate(rate2000t2019, 
                        county.change = rate2019 - rate2000,
                        county.dev = county.change - national.change) %>% 
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
correctionspct2020.rds <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/correctionspct2020.rds")



## % single headed 2000
familytype2000 <- readRDS(family.type2000, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/familytype2000.rds")



## % single headed 2019
familytype2019 <-readRDS(family.type2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/familytype2019.rds")





## Poverty status 2000
povrate2000 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/povrate2000.rds")


### Poverty status 2019
povrate2019 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/povrate2019.rds")



