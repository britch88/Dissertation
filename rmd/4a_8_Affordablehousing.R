library(readr)
mortgage2017 <- read_csv("raw/CountyPredictors/Affordable Housing/ACSST5Y2017.S2506_data_with_overlays_2022-07-29T132955.csv", 
                                                                   skip = 1)
mortgage2017.2<- mortgage2017 %>% 
  select(1,2,3,211:252) %>% 
  select(-contains("Margin of Error!")) %>% 
  select(-5,-6,-9,-10,-13,-14,-17,-18,-21,-22) %>% 
  mutate(total_households_mortgage = sum(.[[3]])) %>% 
  rename_all(~stringr::str_replace(.,"Estimate!!Percent owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage!!MONTHLY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS!!",""))
         
mortgage2017.3 <- mortgage2017.2 %>% 
  mutate(rentburdenlt20 = as.numeric(.[[3]]) * as.numeric(.[[4]])* as.numeric(.[[5]]) *.0001,
         rentburden20t34 = as.numeric(.[[3]]) * as.numeric(.[[6]])* as.numeric(.[[7]]) *.0001,
         rentburden35t49 = as.numeric(.[[3]]) * as.numeric(.[[8]])* as.numeric(.[[9]]) *.0001,
         rentburden50t74 = as.numeric(.[[3]]) * as.numeric(.[[10]])* as.numeric(.[[11]]) *.0001,
         rentburden75plus = as.numeric(.[[3]]) * as.numeric(.[[12]])* as.numeric(.[[13]]) *.0001,
         rentburdentotal = rentburdenlt20 + rentburden20t34 + rentburden35t49 + rentburden50t74 + rentburden75plus,
         rentburdenrate2017mortgage = rentburdentotal/as.numeric(.[[3]])*100)
         
         
##### no mortgage

nomortgage2017 <-read_csv("raw/CountyPredictors/Affordable Housing/ACSST5Y2017.S2507_data_with_overlays_2022-07-29T142700.csv", 
                              skip = 1)

nomortgage2017.2<- nomortgage2017 %>% 
  select(1,2,3,183:223) %>% 
  select(-contains("Margin of Error!")) %>% 
  select(-5,-6,-9,-10,-13,-14,-17,-18,-21,-22) %>%
  rename_all(~stringr::str_replace(.,"Estimate!!Percent owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!MONTHLY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS!!",""))


nomortgage2017.3 <- nomortgage2017.2 %>% 
  mutate(rentburdenlt20 = as.numeric(.[[3]]) * as.numeric(.[[4]])* as.numeric(.[[5]]) *.0001,
         rentburden20t34 = as.numeric(.[[3]]) * as.numeric(.[[6]])* as.numeric(.[[7]]) *.0001,
         rentburden35t49 = as.numeric(.[[3]]) * as.numeric(.[[8]])* as.numeric(.[[9]]) *.0001,
         rentburden50t74 = as.numeric(.[[3]]) * as.numeric(.[[10]])* as.numeric(.[[11]]) *.0001,
         rentburden75plus = as.numeric(.[[3]]) * as.numeric(.[[12]])* as.numeric(.[[13]]) *.0001,
         rentburden.nomortgage = rentburdenlt20 + rentburden20t34 + rentburden35t49 + rentburden50t74 + rentburden75plus)


##### renters

rent2017 <- read_csv("raw/CountyPredictors/Affordable Housing/ACSDT5Y2017.B25070_data_with_overlays_2022-07-29T143213.csv", 
                                                                    skip = 1)


rent2017.2 <- rent2017 %>% 
  select(-contains("Margin of Error!")) %>%
  mutate(rentburden.rent = .[[7]] +.[[8]]  + .[[9]] + .[[10]])


##### combine 3 files

burden.rent.combo <- rent2017.2 %>% 
  select(rentburden.rent, `Geographic Area Name`, id, `Estimate!!Total`) %>% 
  rename(total.rent = `Estimate!!Total`,
         geo_name = `Geographic Area Name`)

  
burden.mortgage.combo <- mortgage2017.3 %>% 
  select(id, 
         `Geographic Area Name`, 
         `Estimate!!Owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage`,
         rentburdentotal) %>% 
  rename( rentburden.mortgage = rentburdentotal,
          geo_name = `Geographic Area Name`,
          total.mortgage = `Estimate!!Owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage`)


burden.nomortgage.combo <- nomortgage2017.3 %>% 
  select(id,
         `Geographic Area Name`,
         `Estimate!!Owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage`,
         rentburden.nomortgage) %>% 
  rename(total.nomortgage =  `Estimate!!Owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage`,
         geo_name = `Geographic Area Name` )



#merge all data frames together
df_list <- list(burden.mortgage.combo, burden.nomortgage.combo, burden.rent.combo)  

burden.combo <- df_list %>% reduce(full_join, by= c('id','geo_name')) %>% 
  mutate(total.households = total.nomortgage + total.mortgage + total.rent,
         sum.tot = sum(total.households),
         total.rentburden = rentburden.mortgage + rentburden.nomortgage + rentburden.rent,
         rentburden_pct = total.rentburden/total.households * 100)


rentburden2017 <- burden.combo %>% 
  select(id, geo_name, total.households, total.rentburden, rentburden_pct)

saveRDS(rentburden2017, here::here("rda","rentburden2017.rds"))

