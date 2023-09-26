# read in packages
library(readr)
library(tidyverse)
library(skimr)
library(janitor)
library(stringr)
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")
library("factoextra")
#install.packages("factoextra")



# read in data
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")

# Z-score all variables
health.dat2 <- health.dat %>% 
  mutate(homicides = case_when(is.na(`Homicides raw value`) ~ 9,
                               TRUE ~ `Homicides raw value`),
    
          z.unemployment = scale(`Unemployment raw value`),
         z.some.college.rev = scale(`Some college raw value`)*(-1),
         z.uninsured = scale(`Uninsured adults raw value`),
         z.median.income.rev = scale(`Median household income raw value`)*(-1),
         z.median.income = scale(`Median household income raw value`),
         z.poverty = scale(`Children in poverty raw value`),
         z.single.parent = scale(`Children in single-parent households raw value`),
         z.lag.ya.rate = scale(lag.ya.rate),
         z.homicides = scale(homicides),
         z.65plus = scale(`% 65 and older raw value`),
         z.american.indian = scale(`% American Indian and Alaskan Native raw value`),
         z.18minus = scale(`% below 18 years of age raw value`),
         z.female = scale(`% Females raw value`),
         z.native.hawaiian = scale(`% Native Hawaiian/Other Pacific Islander raw value`),
         z.black = scale(`% Non-Hispanic African American raw value`),
         z.hispanic = scale(`% Hispanic raw value`),
         z.asian = scale(`% Asian raw value`),
         z.non.english = scale(`% not proficient in English raw value`),
         z.rural = scale(`% Rural raw value`),
         z.hs.grad.rev = scale(`High school graduation raw value`)*(-1),
         
         disadv.edu     = as.numeric(`Some college raw value` <= quantile(health.dat$`Some college raw value`,.25, na.rm=TRUE)),
         disadv.2parent = as.numeric(`Children in single-parent households raw value` <= quantile(health.dat$`Children in single-parent households raw value`,.75, na.rm=TRUE)),
         disadv.income = as.numeric(`Median household income raw value` <= quantile(health.dat$`Median household income raw value`,.25, na.rm=TRUE)),
         disadv.pov = as.numeric(`Children in poverty raw value` <= quantile(health.dat$`Children in poverty raw value`,.75, na.rm=TRUE)),
         disadv.emp = as.numeric(`Unemployment raw value` <= quantile(health.dat$`Unemployment raw value`,.75, na.rm=TRUE)),
         
         adv.edu     = as.numeric(`Some college raw value` <= quantile(health.dat$`Some college raw value`,.75, na.rm=TRUE)),
         adv.2parent = as.numeric(`Children in single-parent households raw value` <= quantile(health.dat$`Children in single-parent households raw value`,.25, na.rm=TRUE)),
         adv.income = as.numeric(`Median household income raw value` <= quantile(health.dat$`Median household income raw value`,.75, na.rm=TRUE)),
         adv.pov = as.numeric(`Children in poverty raw value` <= quantile(health.dat$`Children in poverty raw value`,.25, na.rm=TRUE)),
         adv.emp = as.numeric(`Unemployment raw value` <= quantile(health.dat$`Unemployment raw value`,.25, na.rm=TRUE)),
         
         disadvantage.score = (z.unemployment + z.some.college.rev + z.median.income.rev + z.poverty + z.single.parent )/5, 
         disadvantage.scorev2 =  (z.unemployment + z.hs.grad.rev +z.poverty + z.single.parent )/4,
         disadvantage.scorev3 =  (z.unemployment + z.hs.grad.rev +z.poverty + z.single.parent + z.black )/5,
         

         disadvantage.scale = (disadv.edu + disadv.2parent + disadv.income + disadv.pov + disadv.emp),
         
         mental.days = `Poor mental health days raw value`,
         physical.days = `Poor physical health days raw value`,
         poor.fair = `Poor or fair health raw value`)


# Save data
saveRDS(health.dat2, here::here("rda","health_dat230919.rds"))


write.csv(health.dat2, here::here("rda","health_dat230919.csv"))


# NOTES ----
# citation for creating composite variable
# Song, Mi-Kyung et al. “Composite variables: when and how.” Nursing research vol. 62,1 (2013): 45-9. doi:10.1097/NNR.0b013e3182741948

  
         
