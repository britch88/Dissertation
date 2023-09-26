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

# Read in Arrest Data
arrestdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/trajectory_analysis_file.rds")


# Read in Spatial Analysis file ----
space1 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_analysis_file.rds")

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

space1$in.t <- 1
shapefile2$in.s <- 1
countydat <- merge(shapefile2, space1, by = "fips_clean", all = TRUE)

table(countydat$in.t, countydat$in.s, useNA = "always")



##### Initial Graphs of data ----
# Checking if data is of type spatial
class(countydat)
class(shapefile2)
# it is not of the correct type

# Convert data to a Large SpatialPolygonsDataFrame
#sf:::as_Spatial()
dis.sp <- as(shapefile2, "Spatial")

#Merge spatial data set to county data
county3 <- merge(dis.sp, space1, by = "fips_clean", all = TRUE)
county3v2 <- merge(dis.sp, space1, by = "fips_clean", all.x = FALSE, all.y = TRUE) # no missings


# Check class of merged data to ensure it is of type spatial
class(county3)

#https://rpubs.com/FelipeSantos/LISA_tmap_geoda
#https://mgimond.github.io/simple_moransI_example/#step_1:_define_neighboring_polygons
#https://stats.oarc.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/



# graphing data but not including Alaska and Hawaii because makes map to wide
tm_shape(filter(county3v2,STATEFP != "02" & STATEFP != "15" ))+
  tm_fill(col="county.change",style = "quantile", n=6, palette = "RdYlBu", direction = -1,
          title = "Change in Young Adult County Arrest Rates from 2000 through 2019")+
  tm_layout(frame = FALSE,legend.outside = TRUE)+
  tm_borders(lwd=0.01)+
  tm_shape(state.lines) +
  tm_borders(lwd=.4)

# Creating spatial weights matrix
nb<- poly2nb(county3)
nb2 <- poly2nb(county3v2) # no missings
listw<- nb2listw(nb, style = "W", zero.policy=TRUE)
listw2<- nb2listw(nb2, style = "W", zero.policy=TRUE) # no missings


# Compute Global Moran's I
globalMoran <- moran.test(county3v2$county.change, listw2, na.action =  na.exclude, zero.policy=TRUE)
globalMoran

# Creating Table of Local Moran's 
templmoran <- localmoran(county3v2$county.change, listw2) #, p.adjust.method="none", adjust.x=TRUE, zero.policy = TRUE)


lmoran<- cbind(county3v2@data, localmoran(county3v2$county.change, listw2, adjust.x=TRUE, zero.policy = TRUE)) #,  p.adjust.method="none" ))
lmoran

# centers the local Moran's around the mean
lmoran$Ii <- lmoran$Ii - mean(lmoran$Ii, na.rm = TRUE) 
lmoran$lag.county.change <-  lag.listw(listw2,lmoran$county.change, NAOK = TRUE)

# centers the variable of interest around its mean
lmoran$county.changes <- lmoran$county.change - mean(lmoran$county.change, na.rm = TRUE) 
lmoran$lag.county.change <- lmoran$lag.county.change - mean(lmoran$lag.county.change, na.rm = TRUE) 


signif <- 0.05
#lmoran


lmoran2 <- lmoran%>% 
  mutate(quadrant= ifelse(county.changes>0 & lag.county.change > 0, 1, 0)) %>% 
  mutate(quadrant= ifelse(county.changes<0 & lag.county.change < 0, 2, quadrant)) %>% 
  mutate(quadrant= ifelse(county.changes<0 & lag.county.change > 0, 3, quadrant)) %>% 
  mutate(quadrant= ifelse(county.changes>0 & lag.county.change < 0, 4, quadrant)) %>%   
  mutate(quadrant= ifelse(lmoran$`Pr(z != E(Ii))` > signif, 0, quadrant)) #%>% 
# mutate(quadrant2= ifelse(rate2019_st>0 & lagrate2019_st > 0, 1, 0)) %>% 
# mutate(quadrant2= ifelse(rate2019_st<0 & lagrate2019_st < 0, 2, quadrant2)) %>% 
# mutate(quadrant2= ifelse(rate2019_st<0 & lagrate2019_st > 0, 3, quadrant2)) %>% 
# mutate(quadrant2= ifelse(rate2019_st>0 & lagrate2019_st < 0, 4, quadrant2)) %>% 
# mutate(quadrant2= ifelse(lmoran$LISA_Prate2019 > signif, 0, quadrant2))

county_new<- merge(county3v2, lmoran2, by.x="fips_clean", by.y="fips_clean")

#Map of LISA Values
breaks = c(0, 1, 2, 3, 4, 5) 
LISA1<-tm_shape(filter(county_new, STATEFP.x != "02" & STATEFP.x != "15" & is.na(quadrant)==0)) + 
  tm_fill(col = "quadrant", breaks = breaks, 
          palette=  c("yellow","red","blue","purple","green"), 
         # palette=  c(rgb(1,1,1,alpha = .3),"red","blue",'purple','green'), 
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
tmap_save(filename = "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/countychange_lisa.png", height=5, width=5)

# Create Scatterplot ----


#mp <- moran.plot(as.vector(county_new$rate2019.x), listw)
#  labels=as.character(county_new$name), pch=19)

#  labels=as.character(county_new$name), pch=19)
#mp

lagdat <- as.data.frame(county_new)
sizeinfo <- select(filter(ungroup(arrestdat),year==2019),fips_clean,n_all_adults)
lagdat2 <- merge(lagdat,sizeinfo,by="fips_clean", all.x = TRUE, all.y =FALSE)

ggplot(lagdat2, aes(x=county.change.y, y=lag.county.change)) + 
  geom_point(aes( col=factor(quadrant))) +
  geom_hline(yintercept = 0, col = "black", lwd = 1.1) +
  geom_vline(xintercept = -65, col = "black", lwd = 1.1) +
  # scale_color_manual(values = c('gray','blue', 'green', 'purple','orange','yellow'))
 # scale_color_manual(values = c(rgb(1,1,1,alpha = .3),"red","blue",'purple','green')) +
   scale_color_manual(values = c("yellow","red","blue",'purple','green')) +
  xlab("Change in arrest rate") +
  ylab("Spatial lag of change in arrest rate")


# Testing for spatial correlation in OLS model residuals ----



ols <- lm(county.change.x ~ 1 +  rate2000.x + n_young_adults2000.x + proportion.young.adult2000.x + metstatcat.x  + 
            income.ratio2000.x + povrate2000.x  + single.headed2000.x + nonwhite2000.x + corrections.pct2000.x + state.x, 
          data=county_new)

resid <- resid(ols)
summary(ols)
#mod_beta5 <- as.data.frame(summary(mod5)$coefficients) %>% mutate(model = 5)

neighb.data <- poly2nb(county_new, queen=T)
cont.neighb <- nb2listw(neighb.data,style="W", zero.policy = TRUE)

res <- lm.LMtests(ols, cont.neighb, test="all", zero.policy = TRUE)
summary(res)
