library(readr)
poverty_2019_clean <- read_csv("raw/Additional Predictors for Decomposition/poverty 2019 clean.csv")


povrate2019 <- poverty_2019_clean %>% 
  mutate(fips_clean = paste0(`State FIPS Code`,`County FIPS Code`),
         povrate2019 = `Poverty Percent, All Ages`) %>% 
  select(fips_clean, povrate2019)


saveRDS(povrate2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/povrate2019.rds")
