library(readxl)
library(tidyverse)

voting2016 <- read_excel("raw/CountyPredictors/Community Measures/table04a_voting 2016.xlsx", 
                                   skip = 4)

positions <- c(1,5)

voting2016.2 <- voting2016 %>% 
  select(all_of(positions)) %>% 
  rename(state = 1,
       voting_pct = 2) %>% 
  filter(is.na(voting_pct) ==0, state != "UNITED STATES")

saveRDS(voting2016.2, here::here("rda","voting2016clean.rds"))
