

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(car)
#Anova(lmm)
library(MASS)
options(scipen = 100)



# read in data ----
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")

# keep needed variables for health data set
health.dat2 <- health.dat %>% 
  dplyr::select(fips_clean, year, state, lag.ya.rate, young_adult_rate)


# Get average rates of 2017 - 2019 for structural measures
average.rates <- health.dat %>% 
 # filter(year >= 2017 & year <= 2019) %>% 
  group_by(fips_clean) %>% 
  mutate(avg.some.college = mean(`Some college raw value`, na.rm = TRUE),
         avg.single.parent = mean(`Children in single-parent households raw value`, na.rm = TRUE),
         avg.poverty = mean(`Children in poverty raw value`, na.rm = TRUE),
         avg.income = mean(`Median household income raw value`, na.rm = TRUE),
         avg.unemployment = mean(`Unemployment raw value`, na.rm = TRUE)) %>%
  ungroup()
  #dplyr::select(fips_clean, year, avg.some.college, avg.poverty, avg.single.parent, avg.income, avg.unemployment)


health.dat2 <- average.rates %>% 
  mutate(disadv.edu     = as.numeric(avg.some.college  <= quantile(average.rates$avg.some.college,.25, na.rm=TRUE)),
         disadv.2parent = as.numeric(avg.single.parent <= quantile(average.rates$avg.single.parent,.75, na.rm=TRUE)),
         disadv.income  = as.numeric(avg.income        <= quantile(average.rates$avg.income,.25, na.rm=TRUE)),
         disadv.pov     = as.numeric(avg.poverty       <= quantile(average.rates$avg.poverty,.75, na.rm=TRUE)),
         disadv.emp     = as.numeric(avg.unemployment  <= quantile(average.rates$avg.unemployment,.75, na.rm=TRUE)),
         
         adv.edu     = as.numeric(avg.some.college  <= quantile(average.rates$avg.some.college,.75, na.rm=TRUE)),
         adv.2parent = as.numeric(avg.single.parent <= quantile(average.rates$avg.single.parent,.25, na.rm=TRUE)),
         adv.income  = as.numeric(avg.income        <= quantile(average.rates$avg.income,.75, na.rm=TRUE)),
         adv.pov     = as.numeric(avg.poverty       <= quantile(average.rates$avg.poverty,.25, na.rm=TRUE)),
         adv.emp     = as.numeric(avg.unemployment  <= quantile(average.rates$avg.unemployment,.25, na.rm=TRUE)))

disadvantage <- filter(health.dat2,
                            disadv.edu + disadv.2parent + disadv.income + disadv.pov + disadv.emp > 3)

advantage <- filter(health.dat2,
                           adv.edu + adv.2parent + adv.income + adv.pov + adv.emp > 3)

#limit to only counties that were in disadvantage group for all years
check.n.disadvantage <- disadvantage %>% 
  group_by(fips_clean) %>% 
  mutate(n=n())

library(janitor)
janitor::tabyl(check.n.disadvantage$n)

disadvantage2 <- check.n.disadvantage %>% 
  filter(n==9)

#limit to only counties that were in advantage group for all years
check.n.advantage <- advantage %>% 
  group_by(fips_clean) %>% 
  mutate(n=n())

janitor::tabyl(check.n.advantage$n)

advantage2 <- check.n.advantage %>% 
  filter(n==9)

# Doing some descriptive checks
check.d <- disadvantage2 %>% 
  mutate(disadvantage.indicators = (disadv.edu + disadv.2parent + disadv.income + disadv.pov + disadv.emp))

janitor::tabyl(check.d$disadvantage.indicators)

check.a <- advantage2 %>% 
  mutate(advantage.indicators = (adv.edu + adv.2parent + adv.income + adv.pov + adv.emp))

janitor::tabyl(check.a$advantage.indicators)


# summing up indicators (divide by 9 for county totals)
sum.a <- advantage2 %>% 
  ungroup() %>% 
  summarise(sum.edu = sum(adv.edu),
            sum.pov = sum(adv.pov),
            sum.par = sum(adv.2parent),
            sum.emp = sum(adv.emp),
            sum.inc = sum(adv.income))

sum.d <- disadvantage2 %>% 
  ungroup() %>% 
  summarise(sum.edu = sum(disadv.edu),
            sum.pov = sum(disadv.pov),
            sum.par = sum(disadv.2parent),
            sum.emp = sum(disadv.emp),
            sum.inc = sum(disadv.income))
