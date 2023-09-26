options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
library(lme4)
#install.packages("lfe")
library(lfe)
library(plm)


# read in data ----
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_dat230919.rds")
health.dat2 <- filter(health.dat, is.na(`Poor mental health days raw value`)==0)


# Model 1: Random effects, lag rate and county random effects
m1 <- plm(mental.days ~ z.lag.ya.rate, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m1)


results.m1 <- as.data.frame(summary(m1)$coefficients) %>% mutate(model = 'm1') %>% 
  rownames_to_column()



# Model 2: Add homicides and % Black
m2 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m2)
results.m2 <- as.data.frame(summary(m2)$coefficients) %>% mutate(model = 'm2') %>% 
  rownames_to_column()



# Model 3: Add Disadvantage, scale
m3 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + disadvantage.scale, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m3)
results.m3 <- as.data.frame(summary(m3)$coefficients) %>% mutate(model = 'm3') %>% 
  rownames_to_column()


# Model 4: Add Disadvantage, score
m4 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + disadvantage.score, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m4)
results.m4 <- as.data.frame(summary(m4)$coefficients) %>% mutate(model = 'm4') %>% 
  rownames_to_column()


# Model 5: Add Disadvantage, individual measures
m5 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
            z.median.income + z.single.parent + z.poverty + z.unemployment + z.some.college.rev
            , data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m5)
results.m5 <- as.data.frame(summary(m5)$coefficients) %>% mutate(model = 'm5') %>% 
  rownames_to_column()


# Model 6: Based on Chris's articles, using score variable
m6 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + disadvantage.scorev2 + z.median.income 
          , data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m6)
results.m6 <- as.data.frame(summary(m6)$coefficients) %>% mutate(model = 'm6') %>% 
  rownames_to_column()


# Model 7: Based on Chris's articles, using score variable
m7 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + disadvantage.scorev2 + z.median.income 
          , data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m7)
results.m7 <- as.data.frame(summary(m7)$coefficients) %>% mutate(model = 'm7') %>% 
  rownames_to_column()


# Model 8: Based on Chris's articles, using score variable
m8 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.median.income + disadvantage.scorev3
          , data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m8)
results.m8 <- as.data.frame(summary(m8)$coefficients) %>% mutate(model = 'm8') %>% 
  rownames_to_column()


# Model 9: Based on Chris's articles, using score variable
m9 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate  + disadvantage.scorev2
          , data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m9)
results.m9 <- as.data.frame(summary(m9)$coefficients) %>% mutate(model = 'm9') %>% 
  rownames_to_column()

# Model 10: Add homicides and % Black
m10 <- plm(mental.days ~ z.lag.ya.rate + z.homicides , data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m10)
results.m10 <- as.data.frame(summary(m10)$coefficients) %>% mutate(model = '10') %>% 
  rownames_to_column()


# Model 11: mental.days vs. ya rate
m11 <- plm(mental.days ~ z.lag.ya.rate, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m11)




plot(mental.days ~ disadvantage.score,
     data = health.dat2,
     main = "Upper Arm Length vs. Body Height of Women (Age 20-25)",
     xlab = "Body height (cm)",
     ylab = "Upper arm length (cm)")


# trying stargazer
library(stargazer)
stargazer(m1,m10,m2,m9,m7,m8,type = "text", dep.var.labels = c("Poor Mental Health Days"), 
          out = ("/data/share/xproject/Training/Practice/henderson/Dissertation/output/table1.txt"), star.cutoffs = c(0.05, 0.01, 0.001))


# combining model results ----
df_list <- list(results.m1, results.m2, results.m3, results.m4, results.m5, results.m6, results.m7, results.m8)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods, here::here("rda","mental_RE_20230924.rds"))


write.csv(all.mods, here::here("Output","mental_RE_20230924.csv"))



