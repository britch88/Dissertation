
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
health.dat2 <- filter(health.dat, is.na(`Poor mental health days raw value`)==0)

disadvantage <- filter(health.dat2,
                       disadv.edu + disadv.2parent + disadv.income + disadv.pov + disadv.emp > 3)

advantage <- filter(health.dat2,
                    adv.edu + adv.2parent + adv.income + adv.pov + adv.emp > 3)




# Advantaged    ----


adv <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
                  disadvantage.scale, 
                data=advantage, index=c("fips_clean", "year"), model="random")  #random model

summary(adv)

results.adv <- as.data.frame(summary(adv)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()



# Disadvantaged ----
dis <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.black + z.black*z.lag.ya.rate + 
             disadvantage.scale, 
           data=disadvantage, index=c("fips_clean", "year"), model="random")  #random model

summary(dis)

results.dis <- as.data.frame(summary(dis)$coefficients) %>% mutate(model = 'Advantaged') %>% 
  rownames_to_column()




# combining models ----
df_list<- list(results.adv, results.dis)

all.mods <- df_list %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods, here::here("rda","subgroup_mental230919.rds"))


write.csv(all.mods, here::here("Output","subgroup_mental230919.csv"))
