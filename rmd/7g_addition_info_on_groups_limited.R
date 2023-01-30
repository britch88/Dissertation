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

