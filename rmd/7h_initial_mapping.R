#title: "7h_initial_mapping"
#author: "Brit Henderson"
#date: "2/05/2022"
#output: html_document

readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"


# packages

library(tidyverse)
library(Hmisc)
library(GGally)
library(cowplot)
library(reshape2)
library(tigris)
library(spdep)
library(spatialreg)
library(sf)
library(rgdal)
library(spdplyr)
library(stringr)
library(gstat)
#library(elsa)
library(sp)

# Source Functions
knitr::purl("/data/share/xproject/Training/Practice/henderson/Dissertation/rmd/0a_functions.Rmd")
source("/data/share/xproject/Training/Practice/henderson/Dissertation/rmd/0a_functions.R")

# load data

dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/gbtm4g2p.rds") %>% 
  mutate(group = as.factor(cluster))

  

# Read in County shapefiles

# Note shapefiles from 2020 used
shapefile <- counties()
state.sf <- states() %>% 
  filter(as.numeric(GEOID) <=56 & GEOID != "15" & GEOID != "02")


# Shapefiles contain territories which will not be included in my analyses so removing
shapefile2 <- filter(shapefile, STATEFP <= "56")


shapefile2$fips_clean <- shapefile2$GEOID


# Merge shapefiles and trajectory results

dat$in.t <- 1
shapefile2$in.s <- 1
countydat <- merge(shapefile2, dat, by = "fips_clean", all = TRUE)

table(countydat$in.t, countydat$in.s, useNA = "always")

checkmerge <- filter(countydat, is.na(in.s) |  is.na(in.t))

# 15 non-merges
### 3 territories so okay
### 4 without shapefiles
### 8 counties in shapefile but not in trajectory estimates data


# Graph counties by trajectory group
# assign max posterior probability as group assignment

group.colors <- c("1" = "#CC6677", "2" = "#6699CC", "3" ="#44aa99", "4" = "#882255")
group.colors2 <- c("1" = "firebrick2", "2" = "#6699CC", "3" ="purple", "4" = "gold1")

ggplot() + 
  geom_sf(data = filter(countydat, STATEFP != "02" & STATEFP != "15", is.na(group)==0), color="black",
                    size=0.25, mapping= aes(fill=group)) + 
  geom_sf(data = state.sf, color= "black", size = .5, fill= "white", alpha=0) +
  
  scale_fill_manual(values =group.colors2,
                     name = "Trajectory Group",
                     breaks = c("1","2","3","4"),
                     labels = c("Group 1 - Consistently high",
                                "Group 2 - Moderate with decrease",
                                "Group 3 - Consistently low",
                                "Group 4 - Moderate with increase")) +
  theme_minimal() +
  guides(shape = guide_legend(override.aes = list(size = 1))) + 
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10)) +
 # theme(element)
  coord_sf(datum = NA) # removes gridlines (graticules) 

  



# Qualitatively Checking for Regional and Divisional Clustering based on census definition: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf 
#Here, for both region and then division, I group the counties and calculate proportion of counties for each our 6 trajectory groups.
table(countydat$country_division, useNA = "always")


analysis.dat <- countydat %>% 
  mutate(region = ifelse(country_division %in% c("new england","middle atlantic"),"Northeast",
                          ifelse(country_division %in% c('west south central', 'east south central', 'south atlantic'),"South",
                                 ifelse(country_division %in% c('west north central', 'east north central'),"Midwest",
                                        ifelse(country_division %in% c("pacific","mountain"),"West", "other")))))

table(analysis.dat$region, analysis.dat$country_division, useNA = "always")
#NOTE: Consider revisiting handling of NA


analysis.df <- as.data.frame(analysis.dat)
regions.df <- analysis.df %>% 
  dplyr::select(fips_clean, region, country_division, group) %>% 
 filter(region != "other" ) %>% 
  group_by(region) %>% 
  summarise(Group_1 = sum(group=="Grp1")/n()*100,
            Group_2 = sum(group=="Grp2")/n()*100,
            Group_3 = sum(group=="Grp3")/n()*100,
            Group_4 = sum(group=="Grp4")/n()*100,
            Group_5 = sum(group=="Grp5")/n()*100,
            Group_6 = sum(group=="Grp6")/n()*100) 

regions.dfv2 <- analysis.df %>% 
  dplyr::select(fips_clean, region, country_division, group) %>% 
 filter(region != "other" ) %>% 
  group_by(group) %>% 
  summarise(South = sum(region=="South")/n()*100,
            West = sum(region=="West")/n()*100,
            Midwest = sum(region=="Midwest")/n()*100,
            Northeast = sum(region=="Northeast")/n()*100)



#By divisions
table(analysis.df$country_division, useNA = "always")
divisions.df <- analysis.df %>% 
  dplyr::select(fips_clean, region, country_division, group) %>% 
 filter(is.na(country_division) == 0 & country_division != "possessions") %>% 
  group_by(country_division) %>% 
  summarise(Group_1 = sum(group=="Grp1")/n()*100,
            Group_2 = sum(group=="Grp2")/n()*100,
            Group_3 = sum(group=="Grp3")/n()*100,
            Group_4 = sum(group=="Grp4")/n()*100,
            Group_5 = sum(group=="Grp5")/n()*100,
            Group_6 = sum(group=="Grp6")/n()*100) 


#State-level, oh boy
table(analysis.df$state, useNA = "always")
states.df <- analysis.df %>% 
  dplyr::select(fips_clean, region, country_division, group, state) %>% 
 filter(is.na(state) == 0 & !(state %in% c("puerto rico", "canal zone", "guam"))) %>% 
  group_by(state) %>% 
  summarise(Group_1 = sum(group=="Grp1")/n()*100,
            Group_2 = sum(group=="Grp2")/n()*100,
            Group_3 = sum(group=="Grp3")/n()*100,
            Group_4 = sum(group=="Grp4")/n()*100,
            Group_5 = sum(group=="Grp5")/n()*100,
            Group_6 = sum(group=="Grp6")/n()*100) 


#Overall Proportions
group.proportions <- as.data.frame(table(countydat$group, useNA="no")) %>% 
  mutate(percent = Freq/3141*100)



#
#Attempt at Correlograms...
##Region Correlogram
regions.df2 <- melt(regions.df, id.var = 'region')

ggplot(regions.df2, aes(variable,region)) + 
  geom_tile(aes(fill = value), color = "white", lwd = 1.5,linetype = 1) + 
  scale_fill_gradient(low = 'white', high = 'darkred') +
  geom_text(aes(label = round(value,2)), color = "white", size = 4)


# Division Correlogram
divisions.df2 <- melt(divisions.df, id.var = 'country_division')

ggplot(divisions.df2, aes(variable,country_division)) + 
  geom_tile(aes(fill = value), color = "white", lwd = 1.5,linetype = 1) + 
  scale_fill_gradient(low = 'white', high = 'darkred') +
  geom_text(aes(label = round(value,2)), color = "white", size = 4)


# State Correlogram
states.df2 <- melt(states.df, id.var = 'state')

ggplot(states.df2, aes(variable,state)) + 
  geom_tile(aes(fill = value), color = "white", lwd = 1.5,linetype = 1) + 
  scale_fill_gradient(low = 'white', high = 'darkred') +
  geom_text(aes(label = round(value,2)), color = "white", size = 4)


