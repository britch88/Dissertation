library(readxl)
family_type_2019 <- read_excel("raw/Additional Predictors for Decomposition/family type 2019.xlsx")




family <- family_type_2019 %>% 
  mutate(single.headed2019 = `Other family_KIDS`/(`Other family_KIDS` + Married_couple_KIDS + Married_NO_KIDS + Other_family_NO_KIDS)*100,
         statecode = str_pad(`State Code`,2,pad ="0"),
         countycode = str_pad(`County Code`,3,pad="0"),
         fips_clean = paste0(statecode,countycode))

family.type2019 <- family %>% select(fips_clean, single.headed2019)


saveRDS(family.type2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/familytype2019.rds")
