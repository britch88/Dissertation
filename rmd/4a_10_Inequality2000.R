
library(tidyverse)
library(readr)
income_inequality_2000_clean <- read_csv("raw/Additional Predictors for Decomposition/income inequality 2000 clean.csv")

income.ratio2000 <- income_inequality_2000_clean %>%  
  mutate(income.ratio2000 = ratio80t20) %>% 
  select(fips_clean, income.ratio2000)
                                                            


saveRDS(income.ratio2000, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/income.ratio2000.rds")
