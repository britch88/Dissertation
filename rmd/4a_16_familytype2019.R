library(readxl)
family_type_2019 <- read_excel("raw/Additional Predictors for Decomposition/family type 2019.xlsx")




family <- family_type_2019 %>% 
  mutate(single.headed2019 = `Other family_KIDS`/(`Other family_KIDS` + Married_couple_KIDS + Married_NO_KIDS + Other_family_NO_KIDS)*100,
         fips_clean = substr(`GIS Join Match Code`,3,7))

family.type2019 <- family %>% select(fips_clean, single.headed2019)


saveRDS(family.type2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/familytype2019")
