# title: "Regression"
# author: "Brit Henderson"
# date: "12/26/2022"

options(scipen=100)

# directories ----
readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"


# packages ----
#library(spaMM)
#library(xlsx)
library(lme4)
library(nlme)
library(tidyverse)
library(Hmisc)
library(GGally)
library(cowplot)
library(reshape2)
library(tigris)
library(spatialreg)
library(rgdal)
library(spdplyr)
library(stringr)
library(gstat)
library(sp)
library(sf)
library(spdep)
library(tmap)
#library(INLA)
#library(R2OpenBUGS)
#library(rjags)
#library(MCMCvis)
#library(coda)

# Read in spatial analysis file
desc1 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_analysis_file.rds")


# simple OLS models regressing change in rate on predictors in 2000 ----
mod0 <- lm(county.change ~ 1, data=desc1)
summary(mod0)
mod_beta0 <- as.data.frame(summary(mod0)$coefficients) %>% mutate(model = 0)

mod1 <- lm(county.change ~ 1 + rate2000, data=desc1)
summary(mod1)$coefficients
mod_beta1 <- as.data.frame(summary(mod1)$coefficients)  %>% mutate(model = 1 )


mod2 <- lm(county.change ~ 1 + state, data=desc1)
summary(mod2)
mod_beta2 <-as.data.frame(summary(mod2)$coefficients) %>% mutate(model = 2)



mod3 <- lm(county.change ~ 1 + rate2000 + n_young_adults2000 + proportion.young.adult2000 +
             metstatcat, data = desc1)

summary(mod3)
mod_beta3 <- as.data.frame(summary(mod3)$coefficients) %>% mutate(model = 3)


mod4 <- lm(county.change ~ 1 + rate2000 + n_young_adults2000 + proportion.young.adult2000 +
             metstatcat + state, data = desc1)

summary(mod4)
mod_beta4 <- as.data.frame(summary(mod4)$coefficients) %>% mutate(model = 4)



mod5 <- lm(county.change ~ 1 +  +  rate2000 + n_young_adults2000 + proportion.young.adult2000 + metstatcat  + 
             income.ratio2000 + povrate2000  + single.headed2000 + nonwhite2000 + corrections.pct2000 + state, 
           data=desc1)
summary(mod5)
mod_beta5 <- as.data.frame(summary(mod5)$coefficients) %>% mutate(model = 5)



mod6 <- lm(county.change ~ 1 +  rate2000 + n_young_adults2000 + proportion.young.adult2000 + metstatcat  + 
             income.ratio2000 + povrate2000  + single.headed2000 + nonwhite2000 + corrections.pct2000 , 
           data=desc1)
summary(mod6)
mod_beta6 <- as.data.frame(summary(mod6)$coefficients) %>% mutate(model =6 )



# random intercept model
mod7 <- lme(county.change ~ 1 +  rate2000 + n_young_adults2000 + proportion.young.adult2000 + metstatcat  + 
  income.ratio2000 + povrate2000  + single.headed2000 + nonwhite2000 + corrections.pct2000 , data=desc1, method = "ML", random = ~1|state)
summary(mod7)
#mod_beta7 <- as.data.frame(summary(mod7)$coefficients) %>% mutate(model = 7)
mod_beta7 <-summary(mod7)$coefficients



#### Fitting unconditional state means
mod8  <-lmer(county.change ~ 1 + (1|state), data=desc1) 
summary(mod8)
mod_beta8 <- as.data.frame(summary(mod8)$coefficients) %>% mutate(`Pr(>|t|)` = NA,
                                                                  model = 8)

#extract coefficients
coefs <- data.frame(coef(summary(mod8)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
anova(mod8)

# Extract random effects
VarCorr(mod8)
RandomEffects <- as.data.frame(VarCorr(mod8))

# Compute Intra-class correlation (ICC)
ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between
#From the unconditional means model, the ICC was calculated, 
#which indicated that of the total variance in negative affect, 
#19% is attributable to between-state variation whereas 
#81% is attributable to within-state county variation.This means there is a good 
#portion of within-state variance to model using predictors.

## Multi-level model with predictors
mod9  <-lmer(county.change ~ 1 +  rate2000 + n_young_adults2000 + proportion.young.adult2000 + metstatcat  + 
               income.ratio2000 + povrate2000  + single.headed2000 + nonwhite2000 + corrections.pct2000 +
                     (1|state), data=desc1) 

summary(mod9)
mod_beta9 <- as.data.frame(summary(mod9)$coefficients) %>% mutate(`Pr(>|t|)` = NA,
                                                                  model = 9)

# stack model results
all.results1 <- rbind(mod_beta0,mod_beta1,mod_beta2,mod_beta3,mod_beta4,mod_beta5,mod_beta6, mod_beta8,mod_beta9) %>% 
  tibble::rownames_to_column("Variable")



#### save results
saveRDS(all.results1,"/data/share/xproject/Training/Practice/henderson/Dissertation/rda/linear_model_results.rds")

writexl::write_xlsx(all.results1, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/linear_model_results.xlsx")


