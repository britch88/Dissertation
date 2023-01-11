# title: "Descriptive Information for Spatial Chapter"
# author: "Brit Henderson"
# date: "1/10/2023"

# Directories ----
readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"

# Packages -----------------------------------------------------
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


# Read in arrest data -----------------------------------------------------
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")


arrestdat2 <- arrestdat %>% 
  filter(!fips_clean %in% c("0","00000","01000")) %>%  
  filter(!grepl('721', fips_clean)) %>% 
  filter(!grepl('780', fips_clean)) %>% 
  filter(!grepl('720', fips_clean)) %>% 
  filter(!grepl('691', fips_clean)) %>% 
  filter(!grepl('600', fips_clean)) %>% 
  filter(!grepl('660', fips_clean)) %>% 
  filter(!grepl('579', fips_clean)) 


# calculate national trends -----------------------------------------------
dat2000 <- filter(arrestdat2, year ==1999 | year ==2000 | year ==2001) %>% 
  filter(is.na(young_adult_rate)==0 & young_adult_rate > 0) %>% 
  group_by(year) %>% 
  summarise(natpop2000 = sum(n_young_adults, na.rm = TRUE),
            natarrests2000 = sum(c_young_adult_tot, na.rm = TRUE),
            natrates = natarrests2000/natpop2000 * 1000, na.rm = TRUE) %>% 
  summarise(natrate2000 = sum(natrates)/3)


dat2019 <- filter(arrestdat2, year ==2017 | year ==2018 | year ==2019) %>% 
  filter(is.na(young_adult_rate)==0 & young_adult_rate > 0) %>% 
  group_by(year) %>% 
  summarise(natpop2019 = sum(n_young_adults, na.rm = TRUE),
            natarrests2019 = sum(c_young_adult_tot, na.rm = TRUE),
            natrates = natarrests2019/natpop2019 * 1000, na.rm = TRUE) %>% 
  summarise(natrate2019 = sum(natrates)/3)


national.change = dat2019$natrate2019 - dat2000$natrate2000



# calculate state trends --------------------------------------------------
state2000 <- filter(arrestdat2, year ==1999 | year ==2000 | year ==2001) %>% 
  filter(is.na(young_adult_rate)==0 & young_adult_rate > 0) %>% 
  group_by(year, state) %>% 
  summarise(statepop2000 = sum(n_young_adults, na.rm = TRUE),
            statearrests2000 = sum(c_young_adult_tot, na.rm = TRUE),
            staterates = statearrests2000/statepop2000 * 1000, na.rm = TRUE)  %>% 
  ungroup %>% 
  group_by(state) %>% 
  summarise(staterate2000 = sum(staterates)/3) %>% filter(is.na(staterate2000)==0)


state2019 <- filter(arrestdat2, year ==2017 | year ==2018 | year ==2019) %>% 
  filter(is.na(young_adult_rate)==0 & young_adult_rate > 0) %>% 
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

# Graph state trends ----------------------------------------------------------
statesf <- as(states(), "Spatial") %>% mutate(state = tolower(NAME))
mapstate <- merge(statesf,state.change, by="state", all.x = FALSE, all.y = TRUE)


state.lines <- states()
florida <- state.lines %>% filter(STUSPS =="FL")
alaska <- state.lines %>% filter(STUSPS =="AK")
hawaii <- state.lines %>% filter(STUSPS =="HI")
wisconsin <- state.lines %>% filter(STUSPS =="WI")
illinois <- state.lines %>% filter(STUSPS =="IL")



tm_shape(filter(state.lines,STATEFP != "02" & STATEFP != "15" & STATEFP != "72"
                # & STATEFP != "78" & STATEFP != "17" & STATEFP != "55" & STATEFP != "12" 
                & STATEFP < 60)) +
  tm_polygons(col= "white") + # color for missing states
  tm_borders(lwd=0.5)  +
  tm_shape(filter(mapstate,STATEFP != "02" & STATEFP != "15" 
                  & STATEFP != "17" & STATEFP != "55" & STATEFP != "12" 
                  & STATEFP < 60))+
  tm_fill(col="state.dev",
          style = "fixed", 
          breaks = c(-150,-100,-50,-10,10,50,100,200),
          labels = c("Large Decrease",
                     "Moderate Decrease",
                     "Slight Decrease",
                     "Little to no change",
                     "Slight Increase",
                     "Moderate Increase",
                     "Large Increase",
                     "Very Large Increase"),
          
          # palette = "-RdBu", 
          palette = c("dodgerblue4","dodgerblue3","dodgerblue1","gray80","firebrick1","firebrick3","firebrick4"),
          title = "Deviance \n from natl change \n per 1,000") +
  tm_layout(frame = FALSE,legend.outside = TRUE) +
  tm_borders(lwd=0.5) 

# Graph county change ---------------------------------------------------------

desc1 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_analysis_file.rds")


county.change <- mutate(desc1, 
                        county.change = rate2019 - rate2000,
                        county.dev = county.change - national.change) %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico'))


countysf <- as(counties(), "Spatial") %>% mutate(fips_clean = GEOID)
mapcounty <- merge(countysf,county.change, by="fips_clean", all.x = FALSE, all.y = TRUE)



tm_shape(filter(countysf,STATEFP != "02" & STATEFP != "15" & STATEFP < 60)) +
  tm_polygons(col= "white") +
  tm_borders(lwd=0.5)  +
  tm_shape(filter(mapcounty,STATEFP != "02" & STATEFP != "15" 
                  & STATEFP != "17" & STATEFP != "55" & STATEFP != "12" 
                  & STATEFP < 60))+
  tm_fill(col="county.dev",
          style = "fixed", 
          breaks = c(-150,-100,-50,-10,10,50,100,200),
          labels = c("Large Decrease",
                     "Moderate Decrease",
                     "Slight Decrease",
                     "Little to no change",
                     "Slight Increase",
                     "Moderate Increase",
                     "Large Increase",
                     "Very Large Increase"),
          
          # palette = "-RdBu", 
          palette = c("dodgerblue4","dodgerblue3","dodgerblue1","gray80","firebrick1","firebrick3","firebrick4"),
          title = "Deviance \n from natl change \n per 1,000") +
  tm_layout(frame = FALSE,legend.outside = TRUE) +
  tm_borders(lwd=0.5) 



#### Create forest plot faceted by metro type ----

state.met.change <- county.change %>% 
  group_by(state, metstatcat) %>% 
  summarise(mean.change = mean(county.change, na.rm =)) %>% 
  ungroup() %>% 
  filter(is.na(mean.change)==0)



ggplot(state.met.change, aes(x = mean.change, y = state)) +
  geom_line(color = "gray") +
  geom_point(color = "darkorange2") +
  facet_wrap(.~metstatcat ) +
  labs(x = "Change 2000 - 2019", y = "state") +
  theme_bw()

#large <- metstat2013 %>% 

# Large metro ----
ggplot(filter(state.met.change, metstatcat == "Large Metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
  geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
  geom_point(color = "darkorange2") +
  coord_flip() +
  scale_y_continuous(limits = c(-200,110)) +
  facet_wrap(.~metstatcat, scales = "free" ) +
  labs(y = "Change 2000 - 2019", x = "state") +
  theme_bw()


# Medium metro ----
ggplot(filter(state.met.change, metstatcat == "Medium Metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
  geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
  geom_point(color = "darkorange2") +
  coord_flip() +
  scale_y_continuous(limits = c(-200,110)) +
  facet_wrap(.~metstatcat, scales = "free" ) +
  labs(y = "Change 2000 - 2019", x = "state") +
  theme_bw()


# Small metro ----  
ggplot(filter(state.met.change, metstatcat == "Small Metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
  geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
  geom_point(color = "darkorange2") +
  coord_flip() +
  scale_y_continuous(limits = c(-200,110)) +
  facet_wrap(.~metstatcat, scales = "free" ) +
  labs(y = "Change 2000 - 2019", x = "state") +
  theme_bw()



# Non-metro ------  
ggplot(filter(state.met.change, metstatcat == "Non-metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
  geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
  geom_point(color = "darkorange2") +
  coord_flip() +
  scale_y_continuous(limits = c(-200,110)) +
  facet_wrap(.~metstatcat, scales = "free" ) +
  labs(x = "Change 2000 - 2019", y = "state") +
  theme_bw()

# Table of characteristics ----
library(skimr)
summartdat <- as.data.frame(skim(desc1))
write_csv(summartdat, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_descriptive_table.csv")

# Distribution of Independent Variable ----

# Descriptive stats for change in rates
hist(desc1$county.change, breaks=800)
abline(v = mean(desc1$county.change), col = 'red')
sd(desc1$county.change)
g = desc1$county.change


h <- hist(g, breaks = 800, #density = 10,
          col = "lightgray", xlab = "Change", main = "Distribution of Change in Arrest Rate") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "red", lwd = 2)
