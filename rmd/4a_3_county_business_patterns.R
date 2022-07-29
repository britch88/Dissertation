library(tidyverse)

# County Business Patterns Data: 2017
#https://www.census.gov/data/datasets/2017/econ/cbp/2017-cbp.html
#technical documentation: https://www.census.gov/programs-surveys/cbp/technical-documentation/methodology.html

##### Reading in data and selecting naics codes corresponding to commercial banks, savings institutions, and credit unions

dat.cbp2017 <- read_csv(here::here("raw/CountyPredictors/County Business Patterns", "cbp17co.txt"))
dat.banks2017 <- dat.cbp2017 %>% 
  filter(naics %in% c("522110","522120" ,"522130")) %>% 
  select( fipstate, fipscty, est) %>% 
  mutate(fips_clean = paste(fipstate, fipscty, sep = "")) %>% 
  group_by(fips_clean) %>% 
  summarise(nbanks = sum(est))


dat.food2017 <- dat.cbp2017 %>% 
  filter(naics %in% c("445110","445230")) %>% 
  select( fipstate, fipscty, est) %>% 
  mutate(fips_clean = paste(fipstate, fipscty, sep = "")) %>% 
  group_by(fips_clean) %>% 
  summarise(food = sum(est))


cbp2017merge <- merge(dat.food2017, dat.banks2017, by = c('fips_clean'), all = TRUE)




saveRDS(cbp2017merge, here::here("rda","cbp2017clean.rds"))

