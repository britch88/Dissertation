library(readxl)
volunteering2017 <- read_excel("raw/CountyPredictors/Community Measures/volunteering2017sda.xlsx", 
                                  sheet = "Data_clean", range = "B1:C52")

saveRDS(volunteering2017, here::here("rda","volunteer2017clean.rds"))
