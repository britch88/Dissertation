library(readr)
incarceration2017 <- read_csv("raw/CountyPredictors/Community Measures/cpus1718at02.csv", 
                         skip = 11)

positions <- c(2, 12)

incarceration2017.2 <- incarceration2017 %>% 
  select(positions) %>% 
  rename(state = 1,
         incarcerated_rate = 2) %>% 
  filter(is.na(state)==0)

saveRDS(incarceration2017.2, here::here("rda","incarceration2017.rds"))
