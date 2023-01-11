
### spatial error model

#2.4.5 Spatial Error Model
#Now we will run the Spatial Error Model that was suggested earlier in our analysis by the Moran’s Correlation 
#and LaGrange Multiplier Tests. The Spatial Error Model does not include lagged dependent or independent variables, 
#but instead includes a function of our unexplained error and that of our neighbors. In this analysis, higher 
#than expected residual values suggest a missing explanatory variable that is spatially correlated. 
#In this analysis, Lambda is the error multiplier.

sp.err.model <- spatialreg::errorsarlm(rate ~ 1 + Year + metstatcat + income.ratio + pov.rate +
                                         single.headed + corrections.pct + nonwhite, data=reg.dat2, cont.neighb)
summary(sp.err.model, Nagelkerke = TRUE)


# Derive the residuals from the regression. Need to handle those missed values
seResiduals <- rep(0, length(reg.dat2$rate))
resIndex <- sp.err.model$residuals %>% names() %>% as.integer();
seResiduals[resIndex] <- sp.err.model$residuals

test1 <-  sp.err.model$residuals %>% names()

# Test if there is spatial autocorrelation in the regression residuals (errors).
cont.neighb %>%
  spdep::moran.test(seResiduals, ., zero.policy = TRUE) 

# Add residuals to analysis data set
reg.dat2$residuals <- sp.err.model$resid 

## baseline model: year
model1 <-spatialreg::errorsarlm(rate ~ 1 + Year, data=reg.dat2,cont.neighb) 
summary(model1, Nagelkerke = TRUE)


## year and met category 
model2 <-spatialreg::errorsarlm(rate ~ 1 + Year + metstatcat + metstatcat:Year, data=reg.dat2,cont.neighb) 
summary(model2, Nagelkerke = TRUE)



## year and state
model3 <-spatialreg::errorsarlm(rate ~ 1 + Year + state + state:Year, data=reg.dat2,cont.neighb) 
summary(model3, Nagelkerke = TRUE)



## Full model
model4 <-spatialreg::errorsarlm(rate ~ 1 + Year + state +  state:Year + metstatcat + metstatcat:Year + 
                                  income.ratio + pov.rate + single.headed + corrections.pct + nonwhite, 
                                data=reg.dat2,
                                cont.neighb) 
summary(model4, Nagelkerke = TRUE)


################## Saving results the long and painful way

##### model 1
m1beta <-as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year, data=reg.dat2,cont.neighb))$coefficients)

m1se <- as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year, data=reg.dat2,cont.neighb))$rest.se)

m1beta <- rownames_to_column(m1beta, "Outcome")
m1beta2 <- m1beta %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome)) %>% 
  mutate(Outcome3 = gsub("\\s+","",Outcome2)) %>% 
  rename('m1mean' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year, data = reg.dat2, cont.neighb))$coefficients") %>% 
  select(m1mean, Outcome3) %>% 
  rename("outcome" = "Outcome3")

m1se <- rownames_to_column(m1se, "Outcome") 
m1se2 <- m1se %>%  
  mutate(Outcome0 = gsub("lambda", "", Outcome)) %>% 
  mutate(Outcome1 = str_replace(Outcome0, "WX", "")) %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome1)) %>%  
  mutate(Outcome3 = gsub("I ", "", Outcome2)) %>% 
  mutate(Outcome4 = gsub("\\s+","",Outcome3)) %>% 
  mutate(Outcome5 = sub(".","",Outcome4)) %>% 
  rename('m1se' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year, data = reg.dat2, cont.neighb))$rest.se") %>% 
  select(m1se, Outcome5) %>% 
  rename("outcome" = "Outcome5")


m1res <- merge(m1beta2,m1se2, by="outcome", all = TRUE)




##### model 2
m2beta <-as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year + metstatcat + metstatcat:Year, data=reg.dat2,cont.neighb))$coefficients)

m2se <- as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year + metstatcat + metstatcat:Year, data=reg.dat2,cont.neighb))$rest.se)

m2beta <- rownames_to_column(m2beta, "Outcome")
m2beta2 <- m2beta %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome)) %>% 
  mutate(Outcome3 = gsub("\\s+","",Outcome2)) %>% 
  rename('m2mean' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year + metstatcat + metstatcat:Year, data = reg.dat2, cont.neighb))$coefficients") %>% 
  select(m2mean, Outcome3) %>% 
  rename("outcome" = "Outcome3")

m2se <- rownames_to_column(m2se, "Outcome") 
m2se2 <- m2se %>%  
  mutate(Outcome0 = gsub("lambda", "", Outcome)) %>% 
  mutate(Outcome1 = str_replace(Outcome0, "WX", "")) %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome1)) %>%  
  mutate(Outcome3 = gsub("I ", "", Outcome2)) %>% 
  mutate(Outcome4 = gsub("\\s+","",Outcome3)) %>% 
  mutate(Outcome5 = sub(".","",Outcome4)) %>% 
  rename('m2se' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year + metstatcat + metstatcat:Year, data = reg.dat2, cont.neighb))$rest.se") %>% 
  select(m2se, Outcome5) %>% 
  rename("outcome" = "Outcome5")


m2res <- merge(m2beta2,m2se2, by="outcome", all = TRUE)






### model 3
m3beta <-as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year + state + state:Year, data=reg.dat2,cont.neighb))$coefficients)

m3se <- as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year + state + state:Year, data=reg.dat2,cont.neighb))$rest.se)

m3beta <- rownames_to_column(m3beta, "Outcome")
m3beta2 <- m3beta %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome)) %>% 
  mutate(Outcome3 = gsub("\\s+","",Outcome2)) %>% 
  rename('m3mean' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year + state + state:Year, data = reg.dat2, cont.neighb))$coefficients") %>% 
  select(m3mean, Outcome3) %>% 
  rename("outcome" = "Outcome3")

m3se <- rownames_to_column(m3se, "Outcome") 
m3se2 <- m3se %>%  
  mutate(Outcome0 = gsub("lambda", "", Outcome)) %>% 
  mutate(Outcome1 = str_replace(Outcome0, "WX", "")) %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome1)) %>%  
  mutate(Outcome3 = gsub("I ", "", Outcome2)) %>% 
  mutate(Outcome4 = gsub("\\s+","",Outcome3)) %>% 
  mutate(Outcome5 = sub(".","",Outcome4)) %>% 
  rename('m3se' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year + state + state:Year, data = reg.dat2, cont.neighb))$rest.se") %>% 
  select(m3se, Outcome5) %>% 
  rename("outcome" = "Outcome5")


m3res <- merge(m3beta2,m3se2, by="outcome", all = TRUE)




### model 4
m4beta <-as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year + state +  state:Year + metstatcat + metstatcat:Year + 
                                                        income.ratio + pov.rate + single.headed + corrections.pct + nonwhite, 
                                                      data=reg.dat2,
                                                      cont.neighb))$coefficients)

m4se <- as.data.frame(summary(spatialreg::errorsarlm(rate ~ 1 + Year + state +  state:Year + metstatcat + metstatcat:Year + 
                                                       income.ratio + pov.rate + single.headed + corrections.pct + nonwhite, 
                                                     data=reg.dat2,
                                                     cont.neighb))$rest.se)

m4beta <- rownames_to_column(m4beta, "Outcome")
m4beta2 <- m4beta %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome)) %>% 
  mutate(Outcome3 = gsub("\\s+","",Outcome2)) %>% 
  rename('m4mean' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year + state + state:Year + metstatcat + metstatcat:Year + income.ratio + pov.rate + single.headed + corrections.pct + nonwhite, data = reg.dat2, cont.neighb))$coefficients") %>% 
  select(m4mean, Outcome3) %>% 
  rename("outcome" = "Outcome3")

m4se <- rownames_to_column(m4se, "Outcome") 
m4se2 <- m4se %>%  
  mutate(Outcome0 = gsub("lambda", "", Outcome)) %>% 
  mutate(Outcome1 = str_replace(Outcome0, "WX", "")) %>% 
  mutate(Outcome2 = gsub('[[:punct:] ]+',' ',Outcome1)) %>%  
  mutate(Outcome3 = gsub("I ", "", Outcome2)) %>% 
  mutate(Outcome4 = gsub("\\s+","",Outcome3)) %>% 
  mutate(Outcome5 = sub(".","",Outcome4)) %>% 
  rename('m4se' = "summary(spatialreg::errorsarlm(rate ~ 1 + Year + state + state:Year + metstatcat + metstatcat:Year + income.ratio + pov.rate + single.headed + corrections.pct + nonwhite, data = reg.dat2, cont.neighb))$rest.se") %>% 
  select(m4se, Outcome5) %>% 
  rename("outcome" = "Outcome5")


m4res <- merge(m4beta2,m4se2, by="outcome", all = TRUE)



#### attempt to combine results for all models...
df_list <- list(m1res, m2res, m3res,m4res)
all.results <- df_list %>% reduce(full_join, by='outcome')

