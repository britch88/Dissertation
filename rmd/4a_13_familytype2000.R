library(tidyverse)
library(readxl)
family_type_2000 <- read_excel("raw/Additional Predictors for Decomposition/family type 2000.xlsx")


family <- family_type_2000 %>% 
  mutate(single.headed2000 = `not married with kids`/(`not married with kids` + `married with kids` + `not married no kids` + `married no kids`)*100,
         statecode = str_pad(STATEA,2,pad ="0"),
         countycode = str_pad(COUNTYA,3,pad="0"),
         fips_clean = paste0(statecode,countycode))

family.type2000 <- family %>% select(fips_clean, single.headed2000)

saveRDS(family.type2000, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/familytype2000.rds")
