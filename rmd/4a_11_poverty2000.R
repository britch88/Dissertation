library(readxl)
poverty_2000 <- read_excel("raw/Additional Predictors for Decomposition/poverty 2000.xlsx")




povrate2000 <- poverty_2000 %>% 
  mutate(fips_clean = paste0(state_fip,str_pad(county_fip, 3, pad = "0")),
         povrate2000 = poverty_percent) %>% 
  select(fips_clean, povrate2000)


saveRDS(povrate2000, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/povrate2000.rds")

