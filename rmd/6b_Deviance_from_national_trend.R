# title: "Spatial Autocorrelation"
# author: "Brit Henderson"
# date: "12/26/2022"

readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"


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

# Read in arrest data
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")




# Calculate national trends
dat2000 <- filter(arrestdat, year ==1999 | year ==2000 | year ==2001) %>% 
  group_by(year) %>% 
  summarise(natpop2000 = sum(n_young_adults, na.rm = TRUE),
            natarrests2000 = sum(c_young_adult_tot, na.rm = TRUE),
            natrates = natarrests2000/natpop2000 * 1000, na.rm = TRUE) %>% 
  summarise(natrate2000 = sum(natrates)/3)


dat2019 <- filter(arrestdat, year ==2017 | year ==2018 | year ==2019) %>% 
  group_by(year) %>% 
  summarise(natpop2019 = sum(n_young_adults, na.rm = TRUE),
            natarrests2019 = sum(c_young_adult_tot, na.rm = TRUE),
            natrates = natarrests2019/natpop2019 * 1000, na.rm = TRUE) %>% 
  summarise(natrate2019 = sum(natrates)/3)

national.change = dat2019$natrate2019 - dat2000$natrate2000


# Calculate state trends
state2000 <- filter(arrestdat, year ==1999 | year ==2000 | year ==2001) %>% 
  group_by(year, state) %>% 
  summarise(statepop2000 = sum(n_young_adults, na.rm = TRUE),
            statearrests2000 = sum(c_young_adult_tot, na.rm = TRUE),
            staterates = statearrests2000/statepop2000 * 1000, na.rm = TRUE)  %>% 
  ungroup %>% 
  group_by(state) %>% 
  summarise(staterate2000 = sum(staterates)/3) %>% filter(is.na(staterate2000)==0)


state2019 <- filter(arrestdat, year ==2017 | year ==2018 | year ==2019) %>% 
  group_by(year, state) %>% 
  summarise(statepop2019 = sum(n_young_adults, na.rm = TRUE),
            statearrests2019 = sum(c_young_adult_tot, na.rm = TRUE),
            staterates = statearrests2019/statepop2019 * 1000, na.rm = TRUE)  %>% 
  ungroup %>% 
  group_by(state) %>% 
  summarise(staterate2019 = sum(staterates)/3) %>% filter(is.na(staterate2019)==0)

state.change = merge(state2000, state2019, by = c('state')) %>% 
  mutate(state.change = staterate2019 - staterate2000,
         state.dev = state.change - national.change)

statesf <- as(states(), "Spatial") %>% mutate(state = tolower(NAME))
mapstate <- merge(statesf,state.change, by="state", all.x = FALSE, all.y = TRUE)

tm_shape(filter(mapstate,STATEFP != "02" & STATEFP != "15" & STATEFP != "72"
                & STATEFP != "78" & STATEFP < 70))+
  tm_fill(col="state.dev",style = "quantile", n=6, palette = "-RdBu", 
          title = "Deviance \n from natl change \n per 1,000") +
  tm_layout(frame = FALSE,legend.outside = TRUE) +
  tm_borders(lwd=0.5) #+
 # tm_shape(filter(statesf, !NAME %in% c('United States Virgin Islands',
  #                                      'Guam',
 #                                       'Commonwealth of the Northern Mariana Islands',
 #                                       'American Samoa',
 #                                       'Puerto Rico', 'Hawaii', 'Alaska'))) +
 # tm_borders(lwd=.5)

############### Calculate county trendsand graph them
rate2000 <- filter(arrestdat, 1999 <= year & year <= 2001) %>% 
  group_by(fips_clean, state, Name) %>% 
  summarise(rate2000 = mean(young_adult_rate)) %>% 
  ungroup()

rate2019 <- filter(arrestdat, 2017 <= year & year <= 2019) %>% 
  group_by(fips_clean, state, Name) %>% 
  summarise(rate2019 = mean(young_adult_rate)) %>% 
  ungroup()

rate2000t2019 <- merge(rate2000,rate2019, by=c("fips_clean","Name","state"))

county.change <- mutate(rate2000t2019, 
                        county.change = rate2019 - rate2000,
                        county.dev = county.change - national.change) %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico'))



countysf <- as(counties(), "Spatial") %>% mutate(fips_clean = GEOID)
mapcounty <- merge(countysf,county.change, by="fips_clean", all.x = FALSE, all.y = TRUE)

tm_shape(filter(mapcounty,STATEFP != "02" & STATEFP != "15" & STATEFP != "72"
                & STATEFP != "78" & STATEFP < 70))+
  tm_fill(col="county.dev",style = "quantile", n=6, palette = "-RdBu", 
  title = "Deviance \n from natl change \n per 1,000") +
  tm_layout(frame = FALSE,legend.outside = TRUE) +
  tm_borders(lwd=0.5) +
 tm_shape(filter(statesf, !NAME %in% c('United States Virgin Islands',
                                       'Guam',
                                       'Commonwealth of the Northern Mariana Islands',
                                       'American Samoa',
                                       'Puerto Rico', 'Hawaii', 'Alaska'))) +
 tm_borders(lwd=.5)


