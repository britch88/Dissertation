# title: "Regression"
# author: "Brit Henderson"
# date: "12/26/2022"


readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"

#install inla package
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
#install.packages('gstat')
#remotes::install_version("INLA", version="22.05.03",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#install.packages("rjags")





# packages
#library(spaMM)
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
library(INLA)
library(R2OpenBUGS)
#library(rjags)
library(MCMCvis)
library(coda)

# Read in arrest rate data with independent vars
decomposition_analysis <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/decomposition_analysis.rds")


# Descriptive stats for change in rates
hist(decomposition_analysis$county.change, breaks=800)
abline(v = mean(decomposition_analysis$county.change), col = 'red')
sd(decomposition_analysis$county.change)
g = decomposition_analysis$county.change


h <- hist(g, breaks = 800, #density = 10,
          col = "lightgray", xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "red", lwd = 2)

######### Restructure data by year
data2000 <- decomposition_analysis %>% 
  select(fips_clean, Name, state, rate2000, metstatcat, large.metro, small.metro,
         medium.metro, non.metro, income.ratio2000,corrections.pct2000,
         single.headed2000, povrate2000,nonwhite2000) %>% 
  rename('rate' = 'rate2000',
         'income.ratio' = 'income.ratio2000',
         'corrections.pct' = 'corrections.pct2000',
         'single.headed' = 'single.headed2000',
         'povrate' = 'povrate2000',
         'nonwhite' = 'nonwhite2000') %>% 
  mutate(year = 2000,
         y.2019 = 'no')


data2019 <- decomposition_analysis %>% 
  select(fips_clean, Name, state, rate2019, metstatcat, large.metro, small.metro,
         medium.metro, non.metro, income.ratio2017,corrections.pct2020,
         single.headed2019, povrate2019,nonwhite2017) %>% 
  rename('rate' = 'rate2019',
         'income.ratio' = 'income.ratio2017',
         'corrections.pct' = 'corrections.pct2020',
         'single.headed' = 'single.headed2019',
         'povrate' = 'povrate2019',
         'nonwhite' = 'nonwhite2017') %>% 
  mutate(year = 2019,
         y.2019 = 'yes')

reg.dat <- rbind(data2000, data2019) %>%  
  mutate(y.2019 = factor(y.2019, levels = c('no','yes'), labels = c("Year 2000", "Year 2019")),
         incomeratio = as.numeric(income.ratio),
         pov.rate = as.numeric(povrate),
         Year = as.factor(year)) %>% 
  select(-income.ratio, -povrate, -year, -y.2019) %>% 
  rename(`income.ratio` = `incomeratio`)

summary(reg.dat)

########## add in shapefiles
# Note shapefiles from 2020 used
shapefile <- counties()
state.lines <- states()
reg.dat$in.t <- 1
shapefile$in.s <- 1
shapefile$fips_clean <- shapefile$GEOID
reg.dat2 <- merge(shapefile, reg.dat, by = "fips_clean", all.x = FALSE, all.y = TRUE)
table(reg.dat2$in.t, reg.dat2$in.s, useNA = "always")


#baseline model

mod.base <- gls(rate ~ 1, data=reg.dat, method = "ML")
summary(mod.base)


# random intercept model
mod.random1 <- lme(rate ~1, data=reg.dat, method = "ML", random = ~1|state)
summary(mod.base)
summary(mod.random1)
anova(mod.base, mod.random1)

# OLS with key variables
ols <- lm(rate ~ 1 + Year + metstatcat + state + income.ratio + pov.rate  + single.headed + nonwhite + corrections.pct, data=reg.dat2)
summary(ols)

#### Fitting unconditional state means
model0_fit  <-lmer(rate ~ 1 + (1|state), data=reg.dat2) 
summary(model0_fit)
#extract coefficients
coefs <- data.frame(coef(summary(model0_fit)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
anova(model0_fit)

# Extract random effects
VarCorr(model0_fit)
RandomEffects <- as.data.frame(VarCorr(model0_fit))

# Compute Intra-class correlation (ICC)
ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between
#From the unconditional means model, the ICC was calculated, 
#which indicated that of the total variance in negative affect, 
#13% is attributable to between-state variation whereas 
#87% is attributable to within-state county variation.This means there is a good 
#portion of within-person variance to model using time-varying predictor.

## Multi-level model with predictors
model1_fit  <-lmer(rate ~ 1 + Year + metstatcat + income.ratio + pov.rate +
                     single.headed + corrections.pct + nonwhite +
                     (1|state), data=reg.dat2) 

summary(model1_fit)

# Testing for spatial correlation in OLS model

neighb.data <- poly2nb(reg.dat2, queen=T)
cont.neighb <- nb2listw(neighb.data,style="W", zero.policy = TRUE)

lm.LMtests(ols, cont.neighb, test="all")



### spatial error model

#2.4.5 Spatial Error Model
#Now we will run the Spatial Error Model that was suggested earlier in our analysis by the Moran’s Correlation 
#and LaGrange Multiplier Tests. The Spatial Error Model does not include lagged dependent or independent variables, 
#but instead includes a function of our unexplained error and that of our neighbors. In this analysis, higher 
#than expected residual values suggest a missing explanatory variable that is spatially correlated. 
#In this analysis, Lambda is the error multiplier.

sp.err.model <- spatialreg::errorsarlm(rate ~ 1 + Year + metstatcat + income.ratio + pov.rate +
                                         single.headed + corrections.pct + nonwhite, data=reg.dat2, cont.neighb)
summary(sp.err.model, Nagelkerke = TRUE)


# Derive the residuals from the regression. Need to handle those missed values
seResiduals <- rep(0, length(reg.dat2$rate))
resIndex <- sp.err.model$residuals %>% names() %>% as.integer();
seResiduals[resIndex] <- sp.err.model$residuals

test1 <-  sp.err.model$residuals %>% names()

# Test if there is spatial autocorrelation in the regression residuals (errors).
cont.neighb %>%
  spdep::moran.test(seResiduals, ., zero.policy = TRUE) 

# Add residuals to analysis data set
reg.dat2$residuals <- sp.err.model$resid 

## baseline model: year
model0 <-lm(rate ~ 1 + Year, data=reg.dat2) 
summary(model0)

## baseline model: year and state random effects
model1 <-lmer(rate ~ 1 + Year + (1|state), data=reg.dat2) 
summary(model1)

# Adding met status X year interaction
model2 <-lmer(rate ~ 1 + Year + metstatcat + metstatcat:Year + (1|state), data=reg.dat2) 
summary(model2)

# Adding state
model3 <-lmer(rate ~ 1 + Year +  state:Year + (1|state), data=reg.dat2) 
summary(model3)


# Full model
model0 <-lmer(rate ~ 1 + Year + metstatcat + income.ratio + pov.rate +
                single.headed + corrections.pct + nonwhite + residuals +
                (1|state), data=reg.dat2) 




