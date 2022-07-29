library(readr)

#mortality2016 <- read_delim("/data/share/xproject/Training/Practice/henderson/Dissertation/raw/CountyPredictors/Mortality/Compressed Mortality, 1999-2016.txt")

mortality2017 <- read_delim("/data/share/xproject/Training/Practice/henderson/Dissertation/raw/CountyPredictors/Mortality/Mortality2017.txt")
mortality2017.2 <- mortality2017 %>% filter(is.na(County)==0) %>% 
  select(County, Deaths, `County Code`,`Age Adjusted Rate`)


saveRDS(mortality2017.2, here::here("rda","mortality2017clean.rds"))
