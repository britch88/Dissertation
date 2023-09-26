
options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
#install.packages("lfe")
library(lfe)

# Read in data
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_dat230919.rds")
health.dat2 <- filter(health.dat, is.na(`Poor or fair health raw value`)==0)

disadvantage <- filter(health.dat2,
                       disadv.edu + disadv.2parent + disadv.income + disadv.pov + disadv.emp > 3)

advantage <- filter(health.dat2,
                    adv.edu + adv.2parent + adv.income + adv.pov + adv.emp > 3)




# Advantaged    ----


adv1 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
                  disadvantage.score, 
                data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv1)

results.adv1 <- as.data.frame(summary(adv1)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


adv2 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
             disadvantage.score, 
           data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv2)

results.adv2 <- as.data.frame(summary(adv2)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()

# Disadvantaged ----
dis1 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
             disadvantage.score, 
           data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis1)

results.dis1 <- as.data.frame(summary(dis1)$coefficients) %>% mutate(model = 'Disadvantaged') %>% 
  rownames_to_column()


dis2 <- plm(physical.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
             disadvantage.score, 
           data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis2)

results.dis2 <- as.data.frame(summary(dis2)$coefficients) %>% mutate(model = 'Disadvantaged') %>% 
  rownames_to_column()




# combining models ----
df_list<- list(results.adv, results.dis)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods, here::here("rda","subgroup_poorfair230919.rds"))


write.csv(all.mods, here::here("Output","subgroup_poorfair230919.csv"))
