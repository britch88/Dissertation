library(tidyverse)

#weighted_trajectory_results <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/weighted_trajectory_results.rds")
dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/gbtm4g2p.rds")

long_dat <- dat %>% 
  pivot_longer(contains("year"), 
               names_to = "Year",
               names_prefix = "year",
               values_to = "Arrest rate") 


group.dat <- long_dat %>% 
  group_by(Year, cluster) %>%
  summarise(rate = mean(`Arrest rate`))


averages <- group.dat %>% 
  group_by(cluster) %>%
  summarise(mean = mean(rate),
            max = max(rate),
            min = min(rate),
            std = sd(rate))


peak <- group.dat %>%
 group_by(cluster) %>%
 top_n(1, rate) 


valley <- group.dat %>%
  group_by(cluster) %>%
  top_n(-1, rate)


allstats <- group.dat %>% 
  summarise(max = max(rate),
            min = min(rate),
            average = mean(rate),
            std = sd(rate))


allpeak <- group.dat %>%
  top_n(1, rate) 


alllow <- group.dat %>%
  top_n(-1, rate) 


# Descriptive table for predictors ----
pdat <-readRDS(here::here("rda","logit_analysis_file.rds"))

options(scipen=100)

sdat1 <- skimr::skim(filter(pdat, cluster ==1)) %>% 
  select(skim_variable, n_missing, numeric.mean,numeric.sd) %>% 
  filter(!grepl("p75|p25|state|country",skim_variable)) %>% 
  rename('mean1' = 'numeric.mean',
         'sd1' = 'numeric.sd',
         'missing1' = 'n_missing',
         'variable' = 'skim_variable')


sdat2 <- skimr::skim(filter(pdat, cluster ==2)) %>% 
  select(skim_variable, n_missing, numeric.mean,numeric.sd) %>% 
  filter(!grepl("p75|p25|state|country",skim_variable)) %>% 
  rename('mean2' = 'numeric.mean',
         'sd2' = 'numeric.sd',
         'missing2' = 'n_missing',
         'variable' = 'skim_variable')


sdat3 <- skimr::skim(filter(pdat, cluster ==3)) %>% 
  select(skim_variable, n_missing, numeric.mean,numeric.sd) %>% 
  filter(!grepl("p75|p25|state|country",skim_variable)) %>% 
  rename('mean3' = 'numeric.mean',
         'sd3' = 'numeric.sd',
         'missing3' = 'n_missing',
         'variable' = 'skim_variable')


sdat4 <- skimr::skim(filter(pdat, cluster ==4)) %>% 
  select(skim_variable, n_missing, numeric.mean,numeric.sd) %>% 
  filter(!grepl("p75|p25|state|country",skim_variable)) %>% 
  rename('mean4' = 'numeric.mean',
         'sd4' = 'numeric.sd',
         'missing4' = 'n_missing',
         'variable' = 'skim_variable')


dflist <- list(sdat1,sdat2,sdat3,sdat4)
skim.all <- dflist %>%  reduce(full_join, by='variable')









