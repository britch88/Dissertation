
options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
#install.packages("lfe")
library(lfe)
library(stargazer)

# Read in data
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_dat230919.rds")
health.dat2 <- filter(health.dat, is.na(`Poor mental health days raw value`)==0)

disadvantage <- filter(health.dat2,
                       disadv.edu + disadv.2parent + disadv.income + disadv.pov + disadv.emp > 3)

advantage <- filter(health.dat2,
                    adv.edu + adv.2parent + adv.income + adv.pov + adv.emp > 3)




# Advantaged    ----


adv1 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
                  disadvantage.score, 
                data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv1)

results.adv1 <- as.data.frame(summary(adv1)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


adv2 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
              disadvantage.scorev2, 
            data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv2)

results.adv2 <- as.data.frame(summary(adv2)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


adv3 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
              disadvantage.scorev3, 
            data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv3)

results.adv3 <- as.data.frame(summary(adv3)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


adv4 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate  
              , 
            data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv4)

results.adv3 <- as.data.frame(summary(adv3)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


adv5 <- plm(mental.days ~ z.lag.ya.rate   
            , 
            data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv5)


adv6 <- plm(mental.days ~ z.lag.ya.rate + z.homicides   
            , 
            data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv6)



# Disadvantaged ----
dis1 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
             disadvantage.score, 
           data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis1)

results.dis1 <- as.data.frame(summary(dis1)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


dis2 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
              disadvantage.scorev2, 
            data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis2)

results.dis2 <- as.data.frame(summary(dis2)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


dis3 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
              disadvantage.scorev3, 
            data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis3)

results.dis3 <- as.data.frame(summary(dis3)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()


dis4 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate , 
            data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis4)

dis5 <- plm(mental.days ~ z.lag.ya.rate   , 
            data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis5)

dis6 <- plm(mental.days ~ z.lag.ya.rate + z.homicides  , 
            data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis6)


# combining models ----
df_list<- list(results.adv1, results.adv2, results.adv3, results.dis1, results.dis2, results.dis3)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# View results with stargazer
stargazer(adv1,adv2,adv3,dis1,dis2,dis3, type = "text", dep.var.labels = c("Poor Mental Health Days - Advantaged vs Disadvantaged"), 
          out = ("/data/share/xproject/Training/Practice/henderson/Dissertation/output/mental_subgroup.txt"), star.cutoffs = c(0.05, 0.01, 0.001)) 


# Save and export all models ----
saveRDS(all.mods, here::here("rda","subgroup_mental230919.rds"))


write.csv(all.mods, here::here("Output","subgroup_mental230919.csv"))
