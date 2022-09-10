library(randomForest)
library(datasets)
library(caret)


datrf <- dat %>% select(group, rentburden_pct , prekpct  ,
                        some_college_2017 , hs_grad_2017 , broadband_2017 , median_household_income_2017 ,
                        poverty_2017 , unemployment_rate_2017 , incarcerated_rate , voting_pct , volunteer_pct ) %>% 
  mutate(group = as.factor(group))

check.miss <- as.data.frame(100 * (apply(apply(datrf, 2, is.na), 2, sum)/nrow(datrf)))
datrf2 <- drop_na(datrf)




rf <- randomForest(group~., data=datrf2, proximity=TRUE) 


print(rf)


hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T,
           n.var = 11,
           main = "Top 11 - Variable Importance")
importance(rf)
MeanDecreaseGini


####### Repeat it all using same sample as logit variable importance

v2datrf <- dat %>% select(group, bankrate2017.na,bankrate2017.gtp75,   bankrate2017.p25top75,    
  poverty_2017.ltp25,  poverty_2017.p25top75,unemployment_rate_2017.ltp25,  unemployment_rate_2017.p25top75,
  median_household_income_2017.gtp75, median_household_income_2017.p25top75,some_college_2017.gtp75, some_college_2017.p25top75,
  hs_grad_2017.gtp75,  hs_grad_2017.p25top75, broadband_2017.gtp75,broadband_2017.p25top75, disconnected.2017.na,
  disconnected.2017.ltp25,    disconnected.2017.p25top75, food.2017.na,   food.2017.gtp75,food.2017.p25top75,   
  violent.2017.na,violent.2017.ltp25,violent.2017.p25top75, segregation.2017.na, segregation.2017.ltp25,   
  segregation.2017.p25top75, pcp_2017.gtp75, pcp_2017.p25top75, lbw_2017.ltp25, lbw_2017.p25top75,   
  income_ratio_2017.ltp25,  income_ratio_2017.p25top75, uninsured_2017.ltp25,uninsured_2017.p25top75,  
  under18_2017.ltp25,   under18_2017.gtp75,  over64_2017.ltp25,    over64_2017.gtp75,    
  black_2017.ltp25,black_2017.gtp75, native_2017.ltp25,    native_2017.gtp75, pacific_islander_2017.ltp25,    
  pacific_islander_2017.gtp75, hispanic_2017.ltp25,  hispanic_2017.gtp75,    white_2017.ltp25,white_2017.gtp75,
  lim_eng_2017.ltp25,   lim_eng_2017.gtp75, female_2017.ltp25,    female_2017.gtp75,  rural_2017.ltp25,rural_2017.gtp75,
  prek_2017.gtp75, prek_2017.p25top75,  rentburden_2017.ltp25,   rentburden_2017.p25top75,  deaths_2017.lt10,deaths_2017.ltp25,   
  deaths_2017.p25top75,  incarcerated_2017.ltp25,  incarcerated_2017.p25top75,  voting_2017.gtp75,   voting_2017.p25top75, 
  volunteer_2017.gtp75,volunteer_2017.p25top75, region_Midwest,  region_Northeast,  region_other,   region_West) %>% 
  mutate(group = as.factor(group))

check.miss <- as.data.frame(100 * (apply(apply(v2datrf, 2, is.na), 2, sum)/nrow(v2datrf)))
v2datrf2 <- drop_na(v2datrf)


v2rf <- randomForest(group~., data=v2datrf2, proximity=TRUE) 


print(v2rf)


hist(treesize(v2rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(v2rf,
           sort = T,
           n.var = 50,
           main = "Top 20 - Variable Importance")
importance(v2rf)


