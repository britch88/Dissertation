# read in data
library(readr)
library(tidyverse)
library(skimr)
library(janitor)
library(stringr)


# Read in 2010 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2010.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2010 = read.csv("raw/RWJ Health Data/analytic_data2010.csv", skip = 3, header = F)
colnames(data2010)= headers


data2010x2 = data2010[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2010))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2010)


summarydat2010ck <- as.data.frame(skim(data2010x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)


# Read in 2011 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2011.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2011 = read.csv("raw/RWJ Health Data/analytic_data2011.csv", skip = 3, header = F)
colnames(data2011)= headers


data2011x2 = data2011[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2011))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2011)


summarydat2011ck <- as.data.frame(skim(data2011x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



# Read in 2012 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2012.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2012 = read.csv("raw/RWJ Health Data/analytic_data2012.csv", skip = 3, header = F)
colnames(data2012)= headers


data2012x2 = data2012[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2012))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2012)


summarydat2012ck <- as.data.frame(skim(data2012x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



# Read in 2013 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2013.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2013 = read.csv("raw/RWJ Health Data/analytic_data2013.csv", skip = 3, header = F)
colnames(data2013)= headers


data2013x2 = data2013[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2013))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2013)


summarydat2013ck <- as.data.frame(skim(data2013x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



# Read in 2014 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2014.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2014 = read.csv("raw/RWJ Health Data/analytic_data2014.csv", skip = 3, header = F)
colnames(data2014)= headers


data2014x2 = data2014[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2014))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2014)


summarydat2014ck <- as.data.frame(skim(data2014x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)


# Read in 2015 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2015.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2015 = read.csv("raw/RWJ Health Data/analytic_data2015.csv", skip = 3, header = F)
colnames(data2015)= headers


data2015x2 = data2015[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2015))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2015)


summarydat2015ck <- as.data.frame(skim(data2015x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



# Read in 2016 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2016.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2016 = read.csv("raw/RWJ Health Data/analytic_data2016.csv", skip = 3, header = F)
colnames(data2016)= headers


data2016x2 = data2016[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2016))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2016)


summarydat2016ck <- as.data.frame(skim(data2016x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



# Read in 2017 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2017.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2017 = read.csv("raw/RWJ Health Data/analytic_data2017.csv", skip = 3, header = F)
colnames(data2017)= headers


data2017x2 = data2017[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2017))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2017)


summarydat2017ck <- as.data.frame(skim(data2017x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



# Read in 2018 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2018_0.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2018 = read.csv("raw/RWJ Health Data/analytic_data2018_0.csv", skip = 3, header = F)
colnames(data2018)= headers


data2018x2 = data2018[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2018))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2018)


summarydat2018ck <- as.data.frame(skim(data2018x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



# Read in 2019 health data ----
headers2019 = read.csv("raw/RWJ Health Data/analytic_data2019.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2019 = read.csv("raw/RWJ Health Data/analytic_data2019.csv", skip = 3, header = F)
colnames(data2019)= headers2019


data2019x2 = data2019[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2019))] %>% 
  mutate(fips_clean = "5-digit FIPS Code",
         year = 2019)


summarydat2019ck <- as.data.frame(skim(data2019x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



### check missings for 2010 
check2010 <- as.data.frame(sapply(data2010x2, function(x) sum(is.na(x))))


### check missings for 2011 
check2011 <- as.data.frame(sapply(data2011x2, function(x) sum(is.na(x))))


### check missings for 2012 
check2012 <- as.data.frame(sapply(data2012x2, function(x) sum(is.na(x))))


### check missings for 2013 
check2013 <- as.data.frame(sapply(data2013x2, function(x) sum(is.na(x))))


### check missings for 2014 
check2014 <- as.data.frame(sapply(data2014x2, function(x) sum(is.na(x))))


### check missings for 2015 
check2015 <- as.data.frame(sapply(data2015x2, function(x) sum(is.na(x))))


### check missings for 2016 
check2016 <- as.data.frame(sapply(data2016x2, function(x) sum(is.na(x))))


### check missings for 2017 
check2017 <- as.data.frame(sapply(data2017x2, function(x) sum(is.na(x))))


### check missings for 2018 
check2018 <- as.data.frame(sapply(data2018x2, function(x) sum(is.na(x))))


### check missings for 2019 
check2019 <- as.data.frame(sapply(data2019x2, function(x) sum(is.na(x))))


# Prep arrest data for all years ----
arrdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")

dataprep1.2009_2019 <- filter(arrdat, 2009 <= year & year <= 2019) %>% 
  #group_by(fips_clean, state) %>% 
  #summarise(rate2019 = mean(young_adult_rate)) %>% 
  #ungroup() %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico'))

    ### 34,863 county years


  ## Removing missing fips codes, territories, counties with few arrests or few young adults --
  dataprep2.2009_2019 <- dataprep1.2009_2019 %>%
    filter(!fips_clean %in% c("0","00000","01000")) %>%  
    filter(!grepl('721', fips_clean)) %>% 
    filter(!grepl('780', fips_clean)) %>% 
    filter(!grepl('720', fips_clean)) %>% 
    filter(!grepl('691', fips_clean)) %>% 
    filter(!grepl('600', fips_clean)) %>% 
    filter(!grepl('660', fips_clean)) %>% 
    filter(young_adult_rate >0) 

      ### After removing missing or 0 rates, sample reduced to 30,774 county years
  

  # remove counties with less than 200 young adults
  arrdat.ya.pop <- arrdat %>% 
    filter(2009 <= year & year <=2019 & n_young_adults >= 200) %>% 
    ungroup() %>% 
    select(fips_clean,year, n_young_adults) 

  dataprep3.2009_2019 <- merge(dataprep2.2009_2019,arrdat.ya.pop, by= c("fips_clean", "year"), all.x = F, all.y = F)
  # sample at 29,404 county years



# # Prep arrest data for 2019 (three year average ----
# 
# arrdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")
# 
# dataprep1.2019 <- filter(arrdat, 2017 <= year & year <= 2019) %>% 
#   group_by(fips_clean, state) %>% 
#   summarise(rate2019 = mean(young_adult_rate)) %>% 
#   ungroup() %>% 
#   filter(!state %in% c('canal zone','guam', 'puerto rico'))
# 
#   #### Removing missing fips codes, territories, counties with few arrests or few young adults --
#   dataprep2.2019 <- dataprep1.2019 %>%
#   filter(!fips_clean %in% c("0","00000","01000")) %>%  
#   filter(!grepl('721', fips_clean)) %>% 
#   filter(!grepl('780', fips_clean)) %>% 
#   filter(!grepl('720', fips_clean)) %>% 
#   filter(!grepl('691', fips_clean)) %>% 
#   filter(!grepl('600', fips_clean)) %>% 
#   filter(!grepl('660', fips_clean)) %>% 
#   filter(rate2019 >0) 
# 
#   # After removing missing or 0 rates, sample reduced to 2,853
# 
#   # remove counties with less than 200 young adults
#   arrdat2019 <- arrdat %>% 
#     filter(year == 2019 & n_young_adults >= 200) %>% 
#     ungroup() %>% 
#     select(fips_clean, n_young_adults) 
# 
#   dataprep3.2019 <- merge(dataprep2.2019,arrdat2019, by= "fips_clean", all.x = F, all.y = F)
#   # sample at 2,709
# 
#   
# # Prep arrest data for 2019 (three year average ---
#   
# arrdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")
#   
#   dataprep1.2019single <- filter(arrdat, year = 2019) %>% 
#     group_by(fips_clean, state) %>% 
#     summarise(rate2019 = mean(young_adult_rate)) %>% 
#     ungroup() %>% 
#     filter(!state %in% c('canal zone','guam', 'puerto rico'))
#   
#   #### Removing missing fips codes, territories, counties with few arrests or few young adults --
#   dataprep2.2019single <- dataprep1.2019single %>%
#     filter(!fips_clean %in% c("0","00000","01000")) %>%  
#     filter(!grepl('721', fips_clean)) %>% 
#     filter(!grepl('780', fips_clean)) %>% 
#     filter(!grepl('720', fips_clean)) %>% 
#     filter(!grepl('691', fips_clean)) %>% 
#     filter(!grepl('600', fips_clean)) %>% 
#     filter(!grepl('660', fips_clean)) %>% 
#     filter(rate2019 >0) 
#   
#   # After removing missing or 0 rates, sample reduced to 2,853
#   
#   # remove counties with less than 200 young adults
#   arrdat2019single <- arrdat %>% 
#     filter(year == 2019 & n_young_adults >= 200) %>% 
#     ungroup() %>% 
#     select(fips_clean, n_young_adults) 
#   
#   dataprep3.2019single <- merge(dataprep2.2019single,arrdat2019single, by= "fips_clean", all.x = F, all.y = F)
#   # sample at 2,709
#   
# 
#   # Prep arrest data for 2018 ---
#   
#   arrdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")
#   
#   dataprep1.2018 <- filter(arrdat, year = 2018) %>% 
#     group_by(fips_clean, state) %>% 
#     summarise(rate2018 = mean(young_adult_rate)) %>% 
#     ungroup() %>% 
#     filter(!state %in% c('canal zone','guam', 'puerto rico'))
#   
#   #### Removing missing fips codes, territories, counties with few arrests or few young adults --
#   dataprep2.2018 <- dataprep1.2018 %>%
#     filter(!fips_clean %in% c("0","00000","01000")) %>%  
#     filter(!grepl('721', fips_clean)) %>% 
#     filter(!grepl('780', fips_clean)) %>% 
#     filter(!grepl('720', fips_clean)) %>% 
#     filter(!grepl('691', fips_clean)) %>% 
#     filter(!grepl('600', fips_clean)) %>% 
#     filter(!grepl('660', fips_clean)) %>% 
#     filter(rate2018 >0) 
#   
#   # After removing missing or 0 rates, sample reduced to 2,853
#   
#   # remove counties with less than 200 young adults
#   arrdat2019 <- arrdat %>% 
#     filter(year == 2019 & n_young_adults >= 200) %>% 
#     ungroup() %>% 
#     select(fips_clean, n_young_adults) 
#   
#   dataprep3.2019 <- merge(dataprep2.2019,arrdat2019, by= "fips_clean", all.x = F, all.y = F)
#   # sample at 2,709
#   
#   
# 
# ###### Repeat for 2000 ---
# dataprep1.2000 <- filter(arrdat, 1999 <= year & year <= 2001) %>% 
#   group_by(fips_clean, state) %>% 
#   summarise(rate2000 = mean(young_adult_rate)) %>% 
#   ungroup() %>% 
#   filter(!state %in% c('canal zone','guam', 'puerto rico'))
# 
# 
# dataprep2.2000 <- dataprep1.2000 %>%
#   filter(!fips_clean %in% c("0","00000","01000")) %>%  
#   filter(!grepl('721', fips_clean)) %>% 
#   filter(!grepl('780', fips_clean)) %>% 
#   filter(!grepl('720', fips_clean)) %>% 
#   filter(!grepl('691', fips_clean)) %>% 
#   filter(!grepl('600', fips_clean)) %>% 
#   filter(!grepl('660', fips_clean)) %>% 
#   filter(rate2000 >0) 
# 
# arrdat2000 <- arrdat %>% 
#   filter(year == 2000 & n_young_adults >= 200) %>% 
#   ungroup() %>% 
#   select(fips_clean, n_young_adults) 
# 
# 
# 
# 
# dataprep3.2000 <- merge(dataprep2.2000,arrdat2000, by= "fips_clean", all.x = F, all.y = F)
# # sample at 2,501
# 
# 
# sapply(dataprep3.2000, function(x) sum(is.na(x)))
# 
# 
# 
# ###### Repeat for 2010 ---
# dataprep1.2010 <- filter(arrdat, 2009 <= year & year <= 2011) %>% 
#   group_by(fips_clean, state) %>% 
#   summarise(rate2010 = mean(young_adult_rate)) %>% 
#   ungroup() %>% 
#   filter(!state %in% c('canal zone','guam', 'puerto rico'))
# 
# 
# dataprep2.2010 <- dataprep1.2010 %>%
#   filter(!fips_clean %in% c("0","00000","01000")) %>%  
#   filter(!grepl('721', fips_clean)) %>% 
#   filter(!grepl('780', fips_clean)) %>% 
#   filter(!grepl('720', fips_clean)) %>% 
#   filter(!grepl('691', fips_clean)) %>% 
#   filter(!grepl('600', fips_clean)) %>% 
#   filter(!grepl('660', fips_clean)) %>% 
#   filter(rate2010 >0) 
# 
# arrdat2010 <- arrdat %>% 
#   filter(year == 2010 & n_young_adults >= 200) %>% 
#   ungroup() %>% 
#   select(fips_clean, n_young_adults) 
# 
# 
# 
# 
# dataprep3.2010 <- merge(dataprep2.2010,arrdat2010, by= "fips_clean", all.x = F, all.y = F)
# # sample at 2,708
# sapply(dataprep3.2010, function(x) sum(is.na(x)))
# 
# 
# 
# # Merge all arrest data sets UPDATE not needed for cumulative file----
# arrest.2000.2010 <- merge(dataprep3.2000,dataprep3.2010, by = "fips_clean", all = TRUE, suffixes = c("2000","2010"))
# arrest.2000.2010.2019 <- merge(arrest.2000.2010,dataprep3.2019, by = "fips_clean", all= TRUE, suffixes = c("","2019")) %>% 
#   select(-state2010, -state2000) %>% 
#   rename('n_young_adults2019' = "n_young_adults")

#arrest.single.2000.2010 <- merge(dataprep3.2000single,dataprep3.2010single, by = "fips_clean", all = TRUE, suffixes = c("2000","2010"))
#arrest.single.2000.2010.2019 <- merge(arrest.single.2000.2010,dataprep3.2019single, by = "fips_clean", all= TRUE, suffixes = c("","2019")) %>% 
  #select(-state2010, -state2000) %>% 
 # rename('n_young_adults2019' = "n_young_adults")

# Merge arrest data and health data ----
  
  # Check that data sets contain same variables
  checkcols<- compare_df_cols(data2011x2,data2012x2,data2013x2,
                              data2014x2,data2015x2,data2016x2,data2017x2,
                              data2018x2,data2019x2)
  
  
  # Variables to keep from health data
  keeplist <- c('% 65 and older raw value',  # variables to include
                '% American Indian and Alaskan Native raw value',
                '% below 18 years of age raw value',
                '% Females raw value',
                '% Native Hawaiian/Other Pacific Islander raw value',
                '% not proficient in English raw value',
                '% Rural raw value',
                'year',
                #'Adult smoking raw value',
                'Adult obesity raw value',
                'Children in poverty raw value',
                'Children in single-parent households raw value',
                'Diabetes prevalence raw value',
                #'Did not get needed health care raw value',
                #'Excessive drinking raw value',
                #'High school graduation raw value',
                #'HIV prevalence raw value',
                #'Homicides raw value',
                'Low birthweight raw value',
                'Median household income raw value',
                #'Mental health providers raw value',
                'Poor mental health days raw value',
                'Poor or fair health raw value',
                'Poor physical health days raw value',
                'Population raw value',
                'Premature death raw value',
                'Primary care physicians raw value',
                'Preventable hospital stays raw value',
                #'Ratio of population to mental health providers',
                #'Ratio of population to primary care physicians',
                'Sexually transmitted infections raw value',
                'Some college raw value',
                'Unemployment raw value',
                'Uninsured adults raw value',
                '5-digit FIPS Code')


  # Stack health data sets, keeping data for years that have most of needed variables
  health.data.2011_2019 <- rbind(select(data2011x2, keeplist),
                                 select(data2012x2, keeplist),
                                 select(data2013x2, keeplist),
                                 select(data2014x2, keeplist),
                                 select(data2015x2, keeplist),
                                 select(data2016x2, keeplist),
                                 select(data2017x2, keeplist),
                                 select(data2018x2, keeplist),
                                 select(data2019x2, keeplist)) %>% 
    mutate(fips_clean = str_pad(`5-digit FIPS Code`,5,pad="0"))


  
      
  ### check missings for combined health data 
  checkcombo <- as.data.frame(sapply(health.data.2011_2019, function(x) sum(is.na(x))))
  

  # Now for the actual merge between the arrest and health data...

# health.dat2010 <- merge(dataprep3.2009_2019,data2010x2, by.x ="fips_clean", by.y="5-digit FIPS Code", all.x = TRUE, all.y = FALSE )
# health.dat2019 <- merge(arrest.single.2000.2010.2019,data2019x2, by.x ="fips_clean", by.y="5-digit FIPS Code", all.x = TRUE, all.y = FALSE )
# sapply(health.dat2010, function(x) sum(is.na(x)))
# sapply(health.dat2019, function(x) sum(is.na(x)))
  
  health.arrest.2010_2019 <-  merge(dataprep3.2009_2019,health.data.2011_2019, by = c("fips_clean", "year"), all.x = FALSE, all.y = FALSE )
  # 24,042 records
 
glimpse(health.arrest.2010_2019)
str(health.arrest.2010_2019)
possible_values <- unique(health.arrest.2010_2019$fips_clean)
#2,773 unique counties


# Save data set ----
  saveRDS(health.arrest.2010_2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2010_2019.rds")
#saveRDS(health.dat2010, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2010.rds")
#saveRDS(health.dat2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2019.rds")

               