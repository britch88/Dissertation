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
health.dat2 <- filter(health.dat, is.na(`Poor physical health days raw value`)==0)


# Model 1: Random effects, lag rate and county random effects
m1 <- plm(physical.days ~ z.lag.ya.rate, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m1)


results.m1 <- as.data.frame(summary(m1)$coefficients) %>% mutate(model = 'm1') %>% 
  rownames_to_column()



# Model 2: Add homicides and % Black
m2 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m2)
results.m2 <- as.data.frame(summary(m2)$coefficients) %>% mutate(model = 'm2') %>% 
  rownames_to_column()



# Model 3: Add Disadvantage, scale
m3 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + disadvantage.scale, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m3)
results.m3 <- as.data.frame(summary(m3)$coefficients) %>% mutate(model = 'm3') %>% 
  rownames_to_column()


# Model 4: Add Disadvantage, scale
m4 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + disadvantage.score, data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m4)
results.m4 <- as.data.frame(summary(m4)$coefficients) %>% mutate(model = 'm4') %>% 
  rownames_to_column()


# Model 5: Add Disadvantage, individual measures
m5 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
            z.median.income + z.single.parent + z.poverty + z.unemployment + z.some.college
          , data=health.dat2, index=c("fips_clean", "year"), model="random")  #random model
summary(m5)
results.m5 <- as.data.frame(summary(m5)$coefficients) %>% mutate(model = 'm5') %>% 
  rownames_to_column()


# combining model results ----
df_list <- list(results.m1, results.m2, results.m3, results.m4, results.m5)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods, here::here("rda","physical_RE_202309.rds"))


write.csv(all.mods, here::here("Output","physical_RE_202309.csv"))



