library(readxl)
corrections2020 <- read_excel("raw/Additional Predictors for Decomposition/corrections2020.xlsx")

corrections2020v2 <- corrections2020 %>% 
  mutate(corrections.pct2020 = correctionalpop2020/Total *100,
         fips_clean = substr(`GIS Join Match Code`,3,7)) %>% 
  select(fips_clean, corrections.pct2020)


  
saveRDS(corrections2020v2, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/correctionspct2020.rds")
