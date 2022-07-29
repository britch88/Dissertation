library(readr)

preschool2017 <- read_csv("raw/CountyPredictors/School Enrollment/ACSDT5Y2017.B14003_data_with_overlays_2022-07-23T222256.csv", 
                                                                    skip = 1)

preschool2017.2 <- preschool2017 %>% 
  select(-contains("Margin of Error")) %>% 
  select(contains("3 and 4"),id,"Geographic Area Name") 
  

preschool2017.final <- preschool2017.2 %>% 
  group_by(id, `Geographic Area Name`) %>% 
  summarise(prek = sum(`Estimate!!Total!!Male!!Enrolled in public school!!3 and 4 years`, 
                       `Estimate!!Total!!Male!!Enrolled in private school!!3 and 4 years`,
                       `Estimate!!Total!!Female!!Enrolled in private school!!3 and 4 years`,
                       `Estimate!!Total!!Female!!Enrolled in public school!!3 and 4 years`),
            
            total3and4 = sum(prek, 
                             `Estimate!!Total!!Male!!Not enrolled in school!!3 and 4 years`, 
                             `Estimate!!Total!!Female!!Not enrolled in school!!3 and 4 years`),
            
            prekpct = prek/total3and4 *100)

# Save data set
saveRDS(preschool2017.final, here::here("rda","preschool2017.rds"))
