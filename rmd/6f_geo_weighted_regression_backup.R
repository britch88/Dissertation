
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
gdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_analysis_file.rds")


########## add in shapefiles
# Note shapefiles from 2020 used
shapefile <- counties()
state.lines <- states()
gdat$in.t <- 1
shapefile$in.s <- 1
shapefile$fips_clean <- shapefile$GEOID
gdat2 <- merge(shapefile, gdat, by = "fips_clean", all.x = FALSE, all.y = TRUE)
table(gdat2$in.t, gdat2$in.s, useNA = "always")


# Estimate an optimal bandwidth
# bwVal <- GWmodel::bw.gwr(county.change ~ 1 +  as.factor(state)  + metstatcat + young.adult.pop.change +
#                            income.ratio2000 + income.ratio2017 + povrate.change  + 
#                            single.headed.change + corrections.change + nonwhite.change , 
#                          data=reg.dat2 %>% sf::as_Spatial() , 
#                           kernel = 'bisquare', 
#                          adaptive = TRUE)

GWRbandwidth <- gwr.sel(county.change ~ metstatcat + rate2000 + n_young_adults2000 + proportion.young.adult2000 +
                          income.ratio2000  + povrate2000  + single.headed2000  + corrections.pct2000  + nonwhite2000 , 
                        data=gdat2 , 
                        coords = cbind(as.numeric(gdat2$INTPTLAT),as.numeric(gdat2$INTPTLON)), 
                        adapt = TRUE,
                        show.error.messages= TRUE,
                        longlat = TRUE)



gwr.model1 <- gwr(county.change ~ metstatcat + rate2000 + n_young_adults2000 + proportion.young.adult2000 +
                    income.ratio2000  + povrate2000  + single.headed2000  + corrections.pct2000  + nonwhite2000 , 
                  data=gdat2 , 
                  adapt=GWRbandwidth,
                  coords = cbind(as.numeric(gdat2$INTPTLAT),as.numeric(gdat2$INTPTLON)),
                  hatmatrix=TRUE,
                  se.fit=TRUE)

gwr.model1
# 
# gwr.model2 <- gwr.basic(county.change ~ metstatcat + rate2000 + n_young_adults2000 + proportion.young.adult2000 +
#                           income.ratio2000  + povrate2000  + single.headed2000  + corrections.pct2000  + nonwhite2000 , 
#                         data=gdat2 , 
#                         kernel = "bisquare", 
#                         adaptive = TRUE,
#                         bw=GWRbandwidth, 
#                         F123.test = TRUE)


results <-as.data.frame(gwr.model1$SDF)
names(results)

#pvalue<-gwr.t.adjust(results) 

#gwr.t.adjust()


# convert to spatial polygons dataframe ----
library(sp)
library(FRK)
# pols <- df_to_SpatialPolygons(df = gdat2,
#                               keys = "fips_clean",
#                               coords = c("INTPTLON","INTPTLAT"),
#                               proj = CRS("+proj=longlat +ellps=sphere"))
# 
# gdat_sf <- st_as_sf(gdat2, coords = c("INTPTLON", "INTPTLAT"),  crs = 4326)
# 
# # gdat_coords <- gdat2 %>% as_tibble() %>% 
# #   select(INTPTLON,INTPTLAT) %>% 
# #   mutate(INTPTLON = as.numeric(INTPTLON),
# #          INTPTLAT = as.numeric(INTPTLAT))
# # 
# # DM<-gw.dist(dp.locat=coordinates(gdat_coords), longlat = TRUE)


gdat_spation <- as(gdat2, 'Spatial')
is(gdat_spation, "Spatial")


# try other function ----
gwr.model2 <- gwr.basic(county.change ~ metstatcat + rate2000 + n_young_adults2000 + proportion.young.adult2000 +
                          income.ratio2000  + povrate2000  + single.headed2000  + corrections.pct2000  + nonwhite2000 ,
                        data=gdat_spation ,
                        kernel = "bisquare",
                        adaptive = TRUE,
                        bw=GWRbandwidth,
                        F123.test = TRUE)


# Monte Carlo test for significance ----

bgwr.mc1000 <- montecarlo.gwr(county.change ~ metstatcat + rate2000 + n_young_adults2000 + proportion.young.adult2000 + 
                                income.ratio2000  + povrate2000  + single.headed2000  + corrections.pct2000  + nonwhite2000, 
                              data=gdat_spation,
                              bw = 1000, 
                              nsim = 1000,
                              kernel = "bisquare", 
                              adaptive = FALSE)

summary(bgwr.mc1000)



bgwr.mc1000.v2 <- montecarlo.gwr(county.change ~ metstatcat + rate2000 + n_young_adults2000 + proportion.young.adult2000 + 
                                   income.ratio2000  + povrate2000  + single.headed2000  + corrections.pct2000  + nonwhite2000, 
                                 data=gdat_spation,
                                 bw = 1000, 
                                 nsim = 1000,
                                 kernel = "gaussian", 
                                 adaptive = FALSE)

summary(bgwr.mc1000.v2)

# Mapping results for concentrated disadvantage coefficients ----
gwr.map <- cbind(gdat2, as.matrix(results))
gwr.map2 <- st_as_sf(gwr.map)
qtm(gwr.map, fill = "localR2")

map1 <- tm_shape(gwr.map2) + 
  tm_fill("povrate2000.1",
          n = 5,
          style = "quantile",  
          title = "Poverty rate coefficient") +
  tm_layout(title = "Poverty rate",
            frame = FALSE,
            legend.text.size = .8,
            legend.title.size = 1)
map1


map2 <- tm_shape(gwr.map2) +
  tm_fill("income.ratio2000.1",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Income ratio coefficient") +
  tm_layout(title = "Income ratio",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map2


map3 <- tm_shape(gwr.map2) +
  tm_fill("nonwhite2000.1",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "% Non-white coefficient") +
  tm_layout(title = "% Non-white",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map3


map4 <- tm_shape(gwr.map2) +
  tm_fill("single.headed2000.1",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "% Single-headed households coefficient") +
  tm_layout(title = "% Single-headed households",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map4


map5 <- tm_shape(gwr.map2) +
  tm_fill("corrections.pct2000.1",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "% in Correctional facility coefficient") +
  tm_layout(title = "% in Correctional facility",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map5


# mapping raw values for paired maps ----
map1r <- tm_shape(gwr.map2) + 
  tm_fill("povrate2000",
          n = 5,
          style = "quantile",  
          title = "Poverty rate") +
  tm_layout(title = "Poverty rate",
            frame = FALSE,
            legend.text.size = .8,
            legend.title.size = 1)
map1r


map2r <- tm_shape(gwr.map2) +
  tm_fill("income.ratio2000",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Income ratio") +
  tm_layout(title = "Income ratio",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map2r


map3r <- tm_shape(gwr.map2) +
  tm_fill("nonwhite2000",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "% Non-white") +
  tm_layout(title = "% Non-white",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map3r


map4r <- tm_shape(gwr.map2) +
  tm_fill("single.headed2000",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "% Single-headed households") +
  tm_layout(title = "% Single-headed households",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map4r


map5r <- tm_shape(gwr.map2) +
  tm_fill("corrections.pct2000",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "% in Correctional facility") +
  tm_layout(title = "% in Correctional facility",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map5r


# mapping geo/demo results ----
map6  <- tm_shape(gwr.map2) +
  tm_fill("metstatcatSmall.Metro",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Small metros") +
  tm_layout(title = "Small metros",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map6

map6r <- tm_shape(gwr.map2) +
  tm_fill("metstatcatSmall.Metro",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Small metros") +
  tm_layout(title = "Small metros",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map6r


map7  <- tm_shape(gwr.map2) +
  tm_fill("metstatcatMedium.Metro",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Medium metros") +
  tm_layout(title = "Medium metros",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map7

map7r <- tm_shape(gwr.map2) +
  tm_fill("metstatcatMedium.Metro",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Medium metros") +
  tm_layout(title = "Medium metros",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map7r


map8  <- tm_shape(gwr.map2) +
  tm_fill("metstatcatNon.metro",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Non-metros") +
  tm_layout(title = "Non-metros",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map8

map8r <- tm_shape(gwr.map2) +
  tm_fill("metstatcatNon.metro",
          n = 5,
          # palette = "-viridis",
          #midpoint = -5,
          style = "quantile",
          title = "Non-metros") +
  tm_layout(title = "Non-metros",
            frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map8r

