
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
options(scipen = 100)


# read in data ----
#health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")
health.dat3 <- filter(health.dat2, is.na(`Poor mental health days raw value`)==0)


#### Arrests in prior year with county FE
m1 <- lm(`Poor mental health days raw value` ~   z.lag.ya.rate + fips_clean -1 , health.dat2)

summary(m1)

results.m1 <- as.data.frame(summary(m1)$coefficients) %>% mutate(model = 'm1') %>% 
  rownames_to_column() %>% filter(!str_detect(rowname, "fips_clean"))


#### Arrests in prior year with county FE + homicides
m2 <- lm(`Poor mental health days raw value` ~   z.lag.ya.rate + z.homicides +fips_clean, health.dat2)

summary(m2)

results.m2 <- as.data.frame(summary(m2)$coefficients) %>% mutate(model = 'm2') %>% 
  rownames_to_column() %>% filter(!str_detect(rowname, "fips_clean"))


#### Arrests in prior year with county FE +  homicides + demographic vars
m3 <- lm(`Poor mental health days raw value` ~   z.lag.ya.rate + z.homicides + z.18minus + z.65plus + 
           z.american.indian + z.asian + z.black + z.hispanic + z.native.hawaiian + z.non.english 
            + z.female +  fips_clean, health.dat2)

summary(m3)


results.m3 <- as.data.frame(summary(m3)$coefficients) %>% mutate(model = 'm3') %>% 
  rownames_to_column() %>% filter(!str_detect(rowname, "fips_clean"))


#### Arrests in prior year with county
m4 <- lm(`Poor mental health days raw value` ~   z.lag.ya.rate + z.homicides + z.18minus + z.65plus + 
           z.american.indian + z.asian + z.black + z.hispanic + z.native.hawaiian + z.non.english 
         + z.female + disadvantage.score + fips_clean, health.dat2)

summary(m4)


results.m4 <- as.data.frame(summary(m4)$coefficients) %>% mutate(model = 'm4') %>% 
  rownames_to_column() %>% filter(!str_detect(rowname, "fips_clean"))

# Compare fixed and random effects ----

fixed <- plm(mental.days ~ z.lag.ya.rate, data=health.dat3, index=c("fips_clean", "year"), model="within")  #fixed model
random <- plm(mental.days ~ z.lag.ya.rate, data=health.dat3, index=c("fips_clean", "year"), model="random")  #random model
phtest(fixed,random) #Hausman test
# Null hypothesis is that random effects model is preferred and given p-value of 0.2061, I fail to reject this hypothesis.
# random effects model is better model for these data
# See https://libguides.princeton.edu/R-Panel
summary(fixed)
summary(random)


results.fixed <- as.data.frame(summary(fixed)$coefficients) %>% mutate(model = 'fixed') %>% 
  rownames_to_column()



results.random <- as.data.frame(summary(random)$coefficients) %>% mutate(model = 'random') %>% 
  rownames_to_column()




# Random effects + homicide
random2 <- plm(mental.days ~ z.lag.ya.rate + z.homicides, data=health.dat3, index=c("fips_clean", "year"), model="random")  #random model
summary(random2)


results.random2 <- as.data.frame(summary(random2)$coefficients) %>% mutate(model = 'random2') %>% 
  rownames_to_column()


# Random effects + homicide + demographic variables
random3 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.18minus + z.65plus + 
                 z.american.indian + z.asian + z.black + z.hispanic + z.native.hawaiian + z.non.english 
               + z.female, 
               data=health.dat3, 
               index=c("fips_clean", "year"), 
               model="random")  #random model
summary(random3)


results.random3 <- as.data.frame(summary(random3)$coefficients) %>% mutate(model = 'random3') %>% 
  rownames_to_column()


# Random effects + homicide + demographic variables + disadvantage score
random4 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.18minus + z.65plus + 
                 z.american.indian + z.asian + z.black + z.hispanic + z.native.hawaiian + z.non.english 
               + z.female + disadvantage.score, 
               data=health.dat3, 
               index=c("fips_clean", "year"), 
               model="random")  #random model
summary(random4)


results.random4 <- as.data.frame(summary(random4)$coefficients) %>% mutate(model = 'random4') %>% 
  rownames_to_column()

# Random effects + homicide + demographic variables + disadvantage scale
random5 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + z.18minus + z.65plus + 
                 z.american.indian + z.asian + z.black + z.hispanic + z.native.hawaiian + z.non.english 
               + z.female + disadvantage.scale, 
               data=health.dat3, 
               index=c("fips_clean", "year"), 
               model="random")  #random model
summary(random5)

results.random5 <- as.data.frame(summary(random5)$coefficients) %>% mutate(model = 'random5') %>% 
  rownames_to_column()

# Random effects + homicide +  disadvantage score
random6 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + disadvantage.score, 
               data=health.dat3, 
               index=c("fips_clean", "year"), 
               model="random")  #random model
summary(random6)


results.random6 <- as.data.frame(summary(random6)$coefficients) %>% mutate(model = 'random6') %>% 
  rownames_to_column()


# Random effects + homicide +  disadvantage scale
random7 <- plm(mental.days ~ z.lag.ya.rate + z.homicides + disadvantage.scale, 
               data=health.dat3, 
               index=c("fips_clean", "year"), 
               model="random")  #random model
summary(random7)


results.random7 <- as.data.frame(summary(random7)$coefficients) %>% mutate(model = 'random7') %>% 
  rownames_to_column()



# Combine results
# combining models ----
df_list.poor.fair <- list(results.m1, results.m2, results.m3, results.m4, results.fixed, 
                          results.random, results.random2, results.random3, results.random4, 
                          results.random5, results.random6, results.random7)

all.mods.poor.fair <- df_list.poor.fair %>% reduce(full_join, by='rowname')


# Save and export all models ----
saveRDS(all.mods.poor.fair, here::here("rda","mental_random.rds"))


write.csv(all.mods.poor.fair, here::here("Output","mental_random.csv"))




