# read in data
library(readr)
library(tidyverse)


# Read in 2010 health data ----
headers = read.csv("raw/RWJ Health Data/analytic_data2010.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2010 = read.csv("raw/RWJ Health Data/analytic_data2010.csv", skip = 3, header = F)
colnames(data2010)= headers


data2010x2 = data2010[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2010))] %>% 
  mutate(fips_clean = "5-digit FIPS Code")


summarydat2010ck <- as.data.frame(skim(data2010x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)


# Read in 2019 health data ----
headers2019 = read.csv("raw/RWJ Health Data/analytic_data2019.csv", skip = 0, header = F, nrows = 1, as.is = T)
data2019 = read.csv("raw/RWJ Health Data/analytic_data2019.csv", skip = 3, header = F)
colnames(data2019)= headers2019


data2019x2 = data2019[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2019))] %>% 
  mutate(fips_clean = "5-digit FIPS Code")


summarydat2019ck <- as.data.frame(skim(data2019x2)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)




### check missings for 2019 
check2019 <- as.data.frame(sapply(data2019x2, function(x) sum(is.na(x))))



### check missings for 2010 
check2010 <- as.data.frame(sapply(data2010x2, function(x) sum(is.na(x))))


 # Read in arrest data and select 2019 ----

arrdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")

dataprep1.2019 <- filter(arrdat, 2017 <= year & year <= 2019) %>% 
  group_by(fips_clean, state) %>% 
  summarise(rate2019 = mean(young_adult_rate)) %>% 
  ungroup() %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico'))

  
##### Removing missing fips codes, territories, counties with few arrests or few young adults ----
dataprep2.2019 <- dataprep1.2019 %>%
  filter(!fips_clean %in% c("0","00000","01000")) %>%  
  filter(!grepl('721', fips_clean)) %>% 
  filter(!grepl('780', fips_clean)) %>% 
  filter(!grepl('720', fips_clean)) %>% 
  filter(!grepl('691', fips_clean)) %>% 
  filter(!grepl('600', fips_clean)) %>% 
  filter(!grepl('660', fips_clean)) %>% 
  filter(rate2019 >0) 

# After removing missing or 0 rates, sample reduced to 2,853

# remove counties with less than 200 young adults
arrdat2019 <- arrdat %>% 
  filter(year == 2019 & n_young_adults >= 200) %>% 
  ungroup() %>% 
  select(fips_clean, n_young_adults) 

dataprep3.2019 <- merge(dataprep2.2019,arrdat2019, by= "fips_clean", all.x = F, all.y = F)
# sample at 2,709



###### Repeat for 2000 ----
dataprep1.2000 <- filter(arrdat, 1999 <= year & year <= 2001) %>% 
  group_by(fips_clean, state) %>% 
  summarise(rate2000 = mean(young_adult_rate)) %>% 
  ungroup() %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico'))


dataprep2.2000 <- dataprep1.2000 %>%
  filter(!fips_clean %in% c("0","00000","01000")) %>%  
  filter(!grepl('721', fips_clean)) %>% 
  filter(!grepl('780', fips_clean)) %>% 
  filter(!grepl('720', fips_clean)) %>% 
  filter(!grepl('691', fips_clean)) %>% 
  filter(!grepl('600', fips_clean)) %>% 
  filter(!grepl('660', fips_clean)) %>% 
  filter(rate2000 >0) 

arrdat2000 <- arrdat %>% 
  filter(year == 2000 & n_young_adults >= 200) %>% 
  ungroup() %>% 
  select(fips_clean, n_young_adults) 




dataprep3.2000 <- merge(dataprep2.2000,arrdat2000, by= "fips_clean", all.x = F, all.y = F)
# sample at 2,501


sapply(dataprep3.2000, function(x) sum(is.na(x)))



###### Repeat for 2010 ----
dataprep1.2010 <- filter(arrdat, 2009 <= year & year <= 2011) %>% 
  group_by(fips_clean, state) %>% 
  summarise(rate2010 = mean(young_adult_rate)) %>% 
  ungroup() %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico'))


dataprep2.2010 <- dataprep1.2010 %>%
  filter(!fips_clean %in% c("0","00000","01000")) %>%  
  filter(!grepl('721', fips_clean)) %>% 
  filter(!grepl('780', fips_clean)) %>% 
  filter(!grepl('720', fips_clean)) %>% 
  filter(!grepl('691', fips_clean)) %>% 
  filter(!grepl('600', fips_clean)) %>% 
  filter(!grepl('660', fips_clean)) %>% 
  filter(rate2010 >0) 

arrdat2010 <- arrdat %>% 
  filter(year == 2010 & n_young_adults >= 200) %>% 
  ungroup() %>% 
  select(fips_clean, n_young_adults) 




dataprep3.2010 <- merge(dataprep2.2010,arrdat2010, by= "fips_clean", all.x = F, all.y = F)
# sample at 2,708
sapply(dataprep3.2010, function(x) sum(is.na(x)))



# Merge all arrest data sets ----
arrest.2000.2010 <- merge(dataprep3.2000,dataprep3.2010, by = "fips_clean", all = TRUE, suffixes = c("2000","2010"))
arrest.2000.2010.2019 <- merge(arrest.2000.2010,dataprep3.2019, by = "fips_clean", all= TRUE, suffixes = c("","2019")) %>% 
  select(-state2010, -state2000) %>% 
  rename('n_young_adults2019' = "n_young_adults")



# Merge arrest data and health data ----

health.dat2010 <- merge(arrest.2000.2010.2019,data2010x2, by.x ="fips_clean", by.y="5-digit FIPS Code", all.x = TRUE, all.y = FALSE )
health.dat2019 <- merge(arrest.2000.2010.2019,data2019x2, by.x ="fips_clean", by.y="5-digit FIPS Code", all.x = TRUE, all.y = FALSE )
sapply(health.dat2010, function(x) sum(is.na(x)))
sapply(health.dat2019, function(x) sum(is.na(x)))




# Save data set ----
saveRDS(health.dat2010, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2010.rds")
saveRDS(health.dat2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2019.rds")

               