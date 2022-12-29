library(readxl)
library(tidyverse)
ruralurbancodes2013 <- read_excel("raw/Additional Predictors for Decomposition/ruralurbancodes2013.xls")

metstat01 <- mutate(ruralurbancodes2013,
                    fips_clean = FIPS,
                    metstatcat = ifelse(RUCC_2013 ==1,1,
                                        ifelse(RUCC_2013 == 2,2,
                                               ifelse(RUCC_2013 ==3,3,
                                                      ifelse(RUCC_2013 > 3,4,
                                                             ifelse(is.na(RUCC_2013) ==1,NA,NA))))))

table(ruralurbancodes2013$Description, ruralurbancodes2013$RUCC_2013, useNA = "always")
table(metstat01$Description, metstat01$metstatcat, useNA = "always") 

metstat02 <- metstat01 %>% 
  filter(is.na(metstatcat)==0) %>% 
  mutate(large.metro = ifelse(metstatcat==1,1,0),
         medium.metro = ifelse(metstatcat==2,1,0),
         small.metro = ifelse(metstatcat==3,1,0),
         non.metro = ifelse(metstatcat==4,1,0),
         metstatcat = factor(metstatcat, levels = c(1,2,3,4), labels = c("Large Metro","Medium Metro", "Small Metro", "Non-metro")))
         
         
table(metstat02$metstatcat, metstat02$large.metro ,useNA = "always") 
table(metstat02$metstatcat, metstat02$small.metro ,useNA = "always") 
table(metstat02$metstatcat, metstat02$medium.metro ,useNA = "always") 
table(metstat02$metstatcat, metstat02$non.metro ,useNA = "always") 

metstatout <- select(metstat02,
                     fips_clean,
                     metstatcat,
                     large.metro,
                     small.metro,
                     medium.metro,
                     non.metro)

saveRDS(metstatout, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/metstat2013.rds")
