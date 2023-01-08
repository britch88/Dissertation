
# title: "Geographically Weighted Regression"
# author: "Brit Henderson"
# date: "01/08/2023"


readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"


# packages


library(GWmodel)
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
library(spgwr)
#library(INLA)
#library(MCMCvis)
#library(coda)

# Read in arrest rate data with independent vars
decomposition_analysis <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/decomposition_analysis.rds")
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")


# Grab ratio young to older adults
age_break2000 <- arrestdat %>% 
  ungroup() %>% 
  filter(year == 2000) %>% 
  group_by(fips_clean,year) %>% 
  mutate(proportion.young.adult2000 = n_young_adults/n_all_adults * 100,
         year = as.factor(year),
         n_young_adults2000 = n_young_adults,
         c_young_adult_tot2000 = c_young_adult_tot) %>% 
  ungroup() %>% 
  select(fips_clean,  proportion.young.adult2000,  n_young_adults2000, c_young_adult_tot2000)  
 
age_break2019 <- arrestdat %>% 
  ungroup() %>% 
  filter(year ==2019) %>% 
  group_by(fips_clean,year) %>% 
  mutate(proportion.young.adult2019 = n_young_adults/n_all_adults * 100,
         year = as.factor(year),
         n_young_adults2019 = n_young_adults,
         c_young_adult_tot2019 = c_young_adult_tot) %>% 
  ungroup() %>% 
  select(fips_clean,  proportion.young.adult2019, n_young_adults2019, c_young_adult_tot2019)  

age.break <- merge(age_break2000,age_break2019, by="fips_clean") %>% 
  filter(fips_clean != "0") %>% 
  mutate(young.adult.pop.change = proportion.young.adult2019 - proportion.young.adult2000)

summary(age.break)



# Merge decomposition data set and arrest data to select subset of eligible counties
### based on Light and Harris, counties with 200+ young adults and at least 1 arrest
decomp2 <- merge(age.break, decomposition_analysis, by= "fips_clean") %>%
  filter(n_young_adults2000 >200 & n_young_adults2019 > 200) %>%  # n = 2,446
  filter(c_young_adult_tot2000 > 0 & c_young_adult_tot2019 > 0)  #n = 2,339

summary(decomp2)

# Descriptive stats for change in rates
hist(decomp2$county.change, breaks=800)
abline(v = mean(decomp2$county.change), col = 'red')
sd(decomp2$county.change)
g = decomp2$county.change


h <- hist(g, breaks = 800, #density = 10,
          col = "lightgray", xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "red", lwd = 2)

reg.dat <- decomp2 %>%  
  mutate(income.ratio2000 = as.numeric(income.ratio2000),
         povrate2019 = as.numeric(povrate2019),
         povrate.change = povrate2019 - povrate2000,
         income.ratio.chane = income.ratio2017 - income.ratio2000,
         corrections.change = corrections.pct2020 - corrections.pct2000,
         nonwhite.change = nonwhite2017 - nonwhite2000,
         single.headed.change = single.headed2019 - single.headed2000)

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

# reading in and cleaning county adjacency file to identify counties with no neighbors
### data from: https://www.nber.org/research/data/county-adjacency
county_adjacency2010 <- read_csv("raw/Census Data/county_adjacency2010.csv")
county.x <- county_adjacency2010 %>% 
  mutate(fips_clean = fipsneighbor)

### removing neighbors that don't have arrests or that have less than 200 young adults
county.x2 <- merge(reg.dat, county.x, by= "fips_clean", all.x = TRUE, all.y = FALSE)

county.x3 <- county.x2 %>% 
  group_by(fipscounty) %>% 
  summarise(n.neighbors = n()) %>% 
  rename('fips_clean' = 'fipscounty') 

reg.dat3 <- merge(reg.dat2,county.x3, by="fips_clean", all.x=TRUE, all.y = FALSE) %>% 
  filter(n.neighbors > 3) # 2,185 observations



# Estimate an optimal bandwidth
# bwVal <- GWmodel::bw.gwr(county.change ~ 1 +  as.factor(state)  + metstatcat + young.adult.pop.change +
#                            income.ratio2000 + income.ratio2017 + povrate.change  + 
#                            single.headed.change + corrections.change + nonwhite.change , 
#                          data=reg.dat2 %>% sf::as_Spatial() , 
#                           kernel = 'bisquare', 
#                          adaptive = TRUE)

GWRbandwidth <- gwr.sel(county.change ~ 1 +  as.factor(state)  + metstatcat + young.adult.pop.change +
                          income.ratio2000 + income.ratio2017 + povrate.change  + 
                          single.headed.change + corrections.change + nonwhite.change , 
                  data=reg.dat2 , 
                  coords = cbind(as.numeric(reg.dat2$INTPTLAT),as.numeric(reg.dat2$INTPTLON)), 
                  adapt = TRUE,
                  show.error.messages= TRUE,
                  longlat = TRUE)



gwr.model1 <- gwr(county.change ~ metstatcat + 
                   income.ratio2000 + income.ratio2017 + povrate2000 + povrate2019 + 
                   single.headed2000 + single.headed2019 + 
                   corrections.pct2000 + corrections.pct2020 + nonwhite2000 + nonwhite2017, 
                 data=reg.dat2 , 
                 adapt=GWRbandwidth,
                 coords = cbind(as.numeric(reg.dat2$INTPTLAT),as.numeric(reg.dat2$INTPTLON)),
                 hatmatrix=TRUE,
                 se.fit=TRUE)

gwr.model1




gwr.model <- gwr(county.change ~  as.factor(state)  + metstatcat + 
                   income.ratio2000 + income.ratio2017 + povrate2000 + povrate2019 + 
                   single.headed2000 + single.headed2019 + 
                   corrections.pct2000 + corrections.pct2020 + nonwhite2000 + nonwhite2017, 
                 data=reg.dat2 , 
                 adapt=GWRbandwidth,
                 coords = cbind(as.numeric(reg.dat2$INTPTLAT),as.numeric(reg.dat2$INTPTLON)),
                 hatmatrix=TRUE,
                 se.fit=TRUE)


gwr.model
