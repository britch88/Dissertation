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

# Create average arrest rate for 1999-2001 and 2017-2019
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")

rate2000 <- filter(arrestdat, 1999 <= year & year <= 2001) %>% 
  group_by(fips_clean, state, Name) %>% 
  summarise(rate2000 = mean(young_adult_rate)) %>% 
  ungroup()

rate2019 <- filter(arrestdat, 2017 <= year & year <= 2019) %>% 
  group_by(fips_clean, state, Name) %>% 
  summarise(rate2019 = mean(young_adult_rate)) %>% 
  ungroup()

rate2000t2019 <- merge(rate2000,rate2019, by=c("fips_clean","Name","state"))



# Background info on U.S. Census Bureau County Shapefiles
## Source: https://rdrr.io/cran/tigris/man/counties.html

### Quoted text: Description from the US Census Bureau (see link for source): The primary legal divisions of most states are termed counties. In Louisiana, these divisions are known as parishes. In Alaska, which has no counties, the equivalent entities are the organized boroughs, city and boroughs, municipalities, and census areas; the latter of which are delineated cooperatively for statistical purposes by the state of Alaska and the Census Bureau. In four states (Maryland, Missouri, Nevada, and Virginia), there are one or more incorporated places that are independent of any county organization and thus constitute primary divisions of their states. These incorporated places are known as independent cities and are treated as equivalent entities for purposes of data presentation. The District of Columbia and Guam have no primary divisions, and each area is considered an equivalent entity for purposes of data presentation. All of the counties in Connecticut and Rhode Island and nine counties in Massachusetts were dissolved as functioning governmental entities; however, the Census Bureau continues to present data for these historical entities in order to provide comparable geographic units at the county level of the geographic hierarchy for these states and represents them as nonfunctioning legal entities in data products. The Census Bureau treats the following entities as equivalents of counties for purposes of data presentation: municipios in Puerto Rico, districts and islands in American Samoa, municipalities in the Commonwealth of the Northern Mariana Islands, and islands in the U.S. Virgin Islands. Each county or statistically equivalent entity is assigned a three-character numeric Federal Information Processing Series (FIPS) code based on alphabetical sequence that is unique within state and an eight-digit National Standard feature identifier.

###Usage: counties(state = NULL, cb = FALSE, resolution = "500k", year = NULL, ...)
### Arguments
#### state	- The two-digit FIPS code (string) of the state you want, or a vector of codes if you want multiple states. Can also be state name or state abbreviation.

#### cb-	If cb is set to TRUE, download a generalized (1:500k) counties file. Defaults to FALSE (the most detailed TIGER file).

#### resolution-The resolution of the cartographic boundary file (if cb == TRUE). Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).

#### year -the data year; defaults to 2020


#### arguments to be passed to the underlying 'load_tiger' function, which is not exported. Options include class, which can be set to "sf" (the default) or "sp" to request sf or sp class objects, and refresh, which specifies whether or not to re-download shapefiles (defaults to FALSE).

###See Also: https://www2.census.gov/geo/pdfs/reference/GARM/Ch4GARM.pdf

###Other general area functions: block_groups(), blocks(), county_subdivisions(), places(), pumas(), school_districts(), states(), tracts(), zctas()

# Read in County shapefiles

# Note shapefiles from 2020 used
shapefile <- counties()
state.lines <- states()

ggplot() + geom_sf(data = shapefile, color="black",
                   fill="white", size=0.25)

# Shapefiles contain territories which will not be included in my analyses so removing
shapefile2 <- filter(shapefile, STATEFP <= "56")

ggplot() + geom_sf(data = filter(shapefile2, STATEFP != "02" & STATEFP != "15"), color="black",
                   fill="white", size=0.25)

shapefile2$fips_clean <- shapefile2$GEOID


# Merge shapefiles and arrest rates results

rate2000t2019$in.t <- 1
shapefile2$in.s <- 1
countydat <- merge(shapefile2, rate2000t2019, by = "fips_clean", all = TRUE)

table(countydat$in.t, countydat$in.s, useNA = "always")

checkmerge <- filter(countydat, is.na(in.s) |  is.na(in.t))

# 15 non-merges
### 3 territories so okay
### 4 without shapefiles
### 7 counties in shapefile but not in trajectory estimates data

##### Initial Graphs of data
# Checking if data is of type spatial
class(countydat)
class(shapefile2)
# it is not of the correct type

# Convert data to a Large SpatialPolygonsDataFrame
#sf:::as_Spatial()
dis.sp <- as(shapefile2, "Spatial")

#Merge spatial data set to county data
countydatv2 <- merge(dis.sp, rate2000t2019, by = "fips_clean", all = TRUE)

## first removing missing values
county3 <- filter(countydatv2, is.na(rate2019)==0)  %>% 
  filter(!fips_clean %in% c("0","00000","01000")) %>%  
  #filter(str_detect(fips_clean, '721'|'780'|'720'|'600'|'579')  == 0)
  filter(!grepl('721', fips_clean)) %>% 
  filter(!grepl('780', fips_clean)) %>% 
  filter(!grepl('720', fips_clean)) %>% 
  filter(!grepl('691', fips_clean)) %>% 
  filter(!grepl('600', fips_clean)) %>% 
  filter(!grepl('660', fips_clean)) %>% 
  filter(!grepl('579', fips_clean)) %>% 
  filter(rate2000 >0) %>% 
  filter(rate2019 >0) 


# Check class of merged data to ensure it is of type spatial
class(county3)

#https://rpubs.com/FelipeSantos/LISA_tmap_geoda
#https://mgimond.github.io/simple_moransI_example/#step_1:_define_neighboring_polygons
#https://stats.oarc.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/



# graphing data but not including Alaska and Hawaii because makes map to wide
tm_shape(filter(county3,STATEFP != "02" & STATEFP != "15" ))+
  tm_fill(col="rate2019",style = "quantile", n=4, palette = "Blues", title = "Young Adult County Arrest Rates in 2019")+
  tm_layout(frame = FALSE,legend.outside = TRUE)+
  tm_borders(lwd=0.01)+
  tm_shape(state.lines) +
  tm_borders(lwd=.1)

# Creating spatial weights matrix
nb<- poly2nb(county3)
listw<- nb2listw(nb, style = "W", zero.policy=TRUE)

# Compute Global Moran's I
globalMoran <- moran.test(county3$rate2019, listw, na.action =  na.exclude, zero.policy=TRUE)
globalMoran

# Creating Table of Local Moran's 
#templmoran <- localmoran(county3$rate2019, listw, p.adjust.method="none", adjust.x=TRUE, zero.policy = TRUE)


lmoran<- cbind(county3@data, localmoran(county3$rate2019, listw, p.adjust.method="none", adjust.x=TRUE, zero.policy = TRUE))
#lmoran

# centers the local Moran's around the mean
lmoran$Ii <- lmoran$Ii - mean(lmoran$Ii, na.rm = TRUE) 
lmoran$lag.rate2019 <-  lag.listw(listw,lmoran$rate2019, NAOK = TRUE)

# centers the variable of interest around its mean
lmoran$rate2019s <- lmoran$rate2019 - mean(lmoran$rate2019, na.rm = TRUE) 
lmoran$lag.rate2019 <- lmoran$lag.rate2019 - mean(lmoran$lag.rate2019, na.rm = TRUE) 


signif <- 0.05
#lmoran


lmoran2 <- lmoran%>% 
  mutate(quadrant= ifelse(rate2019s>0 & lag.rate2019 > 0, 1, 0)) %>% 
  mutate(quadrant= ifelse(rate2019s<0 & lag.rate2019 < 0, 2, quadrant)) %>% 
  mutate(quadrant= ifelse(rate2019s<0 & lag.rate2019 > 0, 3, quadrant)) %>% 
  mutate(quadrant= ifelse(rate2019s>0 & lag.rate2019 < 0, 4, quadrant)) %>%   
  mutate(quadrant= ifelse(lmoran$`Pr(z != E(Ii))` > signif, 0, quadrant)) #%>% 
# mutate(quadrant2= ifelse(rate2019_st>0 & lagrate2019_st > 0, 1, 0)) %>% 
# mutate(quadrant2= ifelse(rate2019_st<0 & lagrate2019_st < 0, 2, quadrant2)) %>% 
# mutate(quadrant2= ifelse(rate2019_st<0 & lagrate2019_st > 0, 3, quadrant2)) %>% 
# mutate(quadrant2= ifelse(rate2019_st>0 & lagrate2019_st < 0, 4, quadrant2)) %>% 
# mutate(quadrant2= ifelse(lmoran$LISA_Prate2019 > signif, 0, quadrant2))

county_new<- merge(county3, lmoran2, by.x="fips_clean", by.y="fips_clean")

#Map of LISA Values
breaks = c(0, 1, 2, 3, 4, 5) 
LISA1<-tm_shape(filter(county_new, STATEFP.x != "02" & STATEFP.x != "15" & is.na(quadrant)==0)) + 
  tm_fill(col = "quadrant", breaks = breaks, 
          #palette=  c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), 
          palette=  c(rgb(1,1,1,alpha = .3),"red","blue",'purple','green'), 
          labels = c("Not significant", "High-High","Low-Low","Low-High","High-Low"), title="")+
  tm_legend(text.size = 1)  +
  # tm_scale_bar(position = c("LEFT", "BOTTOM"),text.size = 1.0)+
  # tm_compass(type = "8star",   position = c("RIGHT", "BOTTOM"),      show.labels = 2,   text.size = 0.5)+
  tm_borders(alpha=.5) +
  tm_shape(state.lines) +
  tm_borders(lwd=.5) +
  tm_layout( frame = FALSE,  
             title = "", 
             legend.position = c("right","bottom"),
             legend.outside = TRUE,
             legend.text.size = 1) 

LISA1
tmap_save(filename = "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/rate2019_lisa.png", height=5, width=5)

# Create Scatterplot


#mp <- moran.plot(as.vector(county_new$rate2019.x), listw)
#  labels=as.character(county_new$name), pch=19)

#  labels=as.character(county_new$name), pch=19)
#mp

lagdat <- as.data.frame(county_new)
sizeinfo <- select(filter(ungroup(arrestdat),year==2019),fips_clean,n_all_adults)
lagdat2 <- merge(lagdat,sizeinfo,by="fips_clean", all.x = TRUE, all.y =FALSE)

ggplot(lagdat2, aes(x=rate2019.y, y=lag.rate2019)) + 
  geom_point(aes( col=factor(quadrant))) +
  geom_hline(yintercept = 0, col = "black", lwd = 1.1) +
  geom_vline(xintercept = 104.5, col = "black", lwd = 1.1) +
  # scale_color_manual(values = c('gray','blue', 'green', 'purple','orange','yellow'))
  scale_color_manual(values = c(rgb(1,1,1,alpha = .3),"red","blue",'purple','green'))


# Global Moran's I for all predictors ----

library(sf)
library(dplyr)
library(spdep)

# a shapefile
shape <- st_read(system.file("shape/nc.shp", package="sf")) 

# list weight object
lw <- nb2listw(neighbours = poly2nb(shape, 
                                    queen = TRUE), 
               style = "W",
               zero.policy = TRUE)

# variable names to be investigated
variables <- c("BIR74", "SID74", "NWBIR74")

# initate empty resultset
result <- tibble(NULL)

# now let's iterate!! :)
for (i in variables) {
  
  # calculate the moran's object (as list)
  res <- moran.mc(pull(shape, !!i),
                  listw = lw, 
                  nsim = 999, 
                  zero.policy = TRUE)
  
  # use the res object to create a new row in results dataset
  result <- result %>% 
    bind_rows(tibble(variable = i,
                     statistic = res$statistic,
                     pvalue = res$p.value))
  
}

# check result
result
