# title: "Regression"
# author: "Brit Henderson"
# date: "12/26/2022"


readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"

#install inla package
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
#install.packages('gstat')
#remotes::install_version("INLA", version="22.05.03",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
install.packages("MCMCvis")





# packages
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
         y.2019 = 0)


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
         y.2019 = 1)

reg.dat <- rbind(data2000, data2019)
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



########## testing out code without INLA package
mod = function(){
  #priors
  b0 ~dnorm(0,.001)
  mu.z ~ dnorm(0,.001)
  
  
  sigma~dunif(0,50)
  tau <- 1(sigma*sigma)
  
  
  varsigma~dunif(0,50)
  tau.g <- 1/(varsigma*varsigma)

  #likelihood
  for(i in 1:length(reg.dat$fips_clean)){
    mu[i] <- b0 + z[state[i]]*year[i]
    rate[i]~dnorm(mu[i], tau)
    rate_pred[i]~dnorm(mu[i], tau)
  }
    
    for(j in 1:50){
      z[j] ~dnorm(mu.z, tau.g)
    }
  }


# write model
model.file = "model.txt"
write.model(mod,model.file)


# no initial values
inits <- NULL

# parameters I want to track
params = c("tau", "z","b0", "rate_pred")

# hyperparameters
# Number of iterations
ni = 1000
#burn in interval
nb = 1000
#number of chains
nc = 3

# compile model
jmod = jags.model(file=model.file, data = reg.dat, 
                  n.chains = nc,
                  inits = inits,
                  n.adapt = 1000)


# iterate through the jmod for the extent of the burn-in
update(jmod, n.iter = nb, by = 1)

#draw samples from the posterior fpr params, given MCMC hyperparameters
post = coda.samples(jmod, params, n.iter = ni, thin = nt)


########## create spatial weights matrix
listw.0 <- poly2nb(reg.dat2)
listw <- nb2mat(listw.0, style = "B") 

# model without spatial

reg.dat2$metstatcat <- relevel(reg.dat2$metstatcat, "Non-metro")


modtest <- inla(rate ~ year + metstatcat, data=reg.dat)

mod.base1 <- inla(rate ~ 1 + year + metstatcat +
  f(state, model = "iid"), 
  data = as.data.frame(reg.dat2),
 # family = "iid",
  control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE), verbose = TRUE)


summary(mod.base1)


boston.iid <- inla(update(boston.form, . ~. + f(ID, model = "iid")),
                   data = as.data.frame(boston.tr),
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                   control.predictor = list(compute = TRUE)
)
summary(boston.iid)

## model with spatial autoregression
#Besag's improper
boston.besag <- inla(update(boston.form, . ~. +
                              f(ID, model = "besag", graph = W.boston)), 
                     data = as.data.frame(boston.tr),
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute = TRUE)
)
boston.tr$BESAG <- tmarg(boston.besag$marginals.fitted.values)


#Besag proper
boston.besagprop <- inla(update(boston.form, . ~. +
                                  f(ID, model = "besagproper", graph = W.boston)), 
                         data = as.data.frame(boston.tr),
                         control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                         control.predictor = list(compute = TRUE)
)
boston.tr$BESAGPROP <- tmarg(boston.besagprop$marginals.fitted.values)


#BYM
boston.bym <- inla(update(boston.form, . ~. +
                            f(ID, model = "bym", graph = W.boston)), 
                   data = as.data.frame(boston.tr),
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                   control.predictor = list(compute = TRUE)
)
boston.tr$BYM <- tmarg(boston.bym$marginals.fitted.values)






