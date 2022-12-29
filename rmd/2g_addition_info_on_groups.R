library(tidyverse)

weighted_trajectory_results <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/weighted_trajectory_results.rds")
tempds <- weighted_trajectory_results 
tempds3 <- tempds %>% 
  mutate(year = x + 1973) %>% 
  group_by(Group) %>%
  summarise(max = max(w_mean),
            min = min(w_mean),
            average = mean(w_mean),
            std = sd(w_mean))

peak <- tempds %>%
 group_by(Group) %>%
 top_n(1, w_mean) %>% 
 mutate(year = x + 1973)

valley <- tempds %>%
  group_by(Group) %>%
  top_n(-1, w_mean) %>% 
  mutate(year = x + 1973)


allstats <- tempds %>% 
  mutate(year = x + 1973) %>% 
  summarise(max = max(w_mean),
            min = min(w_mean),
            average = mean(w_mean),
            std = sd(w_mean))

allpeak <- tempds %>%
  top_n(1, w_mean) %>% 
  mutate(year = x + 1973)

alllow <- tempds %>%
  top_n(-1, w_mean) %>% 
  mutate(year = x + 1973)

