

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

health.dat2 <- health.dat %>% 
  mutate(disadv.edu     = as.numeric(`Some college raw value` <= quantile(health.dat$`Some college raw value`,.25, na.rm=TRUE)),
         disadv.2parent = as.numeric(`Children in single-parent households raw value` <= quantile(health.dat$`Children in single-parent households raw value`,.75, na.rm=TRUE)),
         disadv.income = as.numeric(`Median household income raw value` <= quantile(health.dat$`Median household income raw value`,.25, na.rm=TRUE)),
         disadv.pov = as.numeric(`Children in poverty raw value` <= quantile(health.dat$`Children in poverty raw value`,.75, na.rm=TRUE)),
         disadv.emp = as.numeric(`Unemployment raw value` <= quantile(health.dat$`Unemployment raw value`,.75, na.rm=TRUE)),
         
         adv.edu     = as.numeric(`Some college raw value` <= quantile(health.dat$`Some college raw value`,.75, na.rm=TRUE)),
         adv.2parent = as.numeric(`Children in single-parent households raw value` <= quantile(health.dat$`Children in single-parent households raw value`,.25, na.rm=TRUE)),
         adv.income = as.numeric(`Median household income raw value` <= quantile(health.dat$`Median household income raw value`,.75, na.rm=TRUE)),
         adv.pov = as.numeric(`Children in poverty raw value` <= quantile(health.dat$`Children in poverty raw value`,.25, na.rm=TRUE)),
         adv.emp = as.numeric(`Unemployment raw value` <= quantile(health.dat$`Unemployment raw value`,.25, na.rm=TRUE)))

disadvantage <- filter(health.dat2,
                            disadv.edu + disadv.2parent + disadv.income + disadv.pov + disadv.emp > 3)

advantage <- filter(health.dat2,
                           adv.edu + adv.2parent + adv.income + adv.pov + adv.emp > 3)

