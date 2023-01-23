# title: "Spatial Error Models"
# author: "Brit Henderson"
# date: "1/13/2023"

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
countydatv2 <- merge(shapefile2, space1, by = "fips_clean", all.y = TRUE, all.x = FALSE)

table(countydatv2$in.t, countydat$in.s, useNA = "always")




# neighbors ----
neighb.data <- poly2nb(countydatv2, queen=T)
cont.neighb <- nb2listw(neighb.data,style="W", zero.policy = TRUE)

### spatial error model

#2.4.5 Spatial Error Model
#Now we will run the Spatial Error Model that was suggested earlier in our analysis by the Moran’s Correlation 
#and LaGrange Multiplier Tests. The Spatial Error Model does not include lagged dependent or independent variables, 
#but instead includes a function of our unexplained error and that of our neighbors. In this analysis, higher 
#than expected residual values suggest a missing explanatory variable that is spatially correlated. 
#In this analysis, Lambda is the error multiplier.

sp.err.model <- spatialreg::errorsarlm(county.change.x ~ 1 +  rate2000.x + n_young_adults2000.x + proportion.young.adult2000.x + metstatcat.x  + 
                                         income.ratio2000.x + povrate2000.x  + single.headed2000.x + nonwhite2000.x + corrections.pct2000.x + state.x, 
                                       data=county_new, 
                                       cont.neighb,
                                       zero.policy = TRUE)
summary(sp.err.model, Nagelkerke = TRUE)


# Derive the residuals from the regression. Need to handle those missed values
seResiduals <- rep(0, length(county_new$county.change.x))
resIndex <- sp.err.model$residuals %>% names() %>% as.integer();
seResiduals[resIndex] <- sp.err.model$residuals

test1 <-  sp.err.model$residuals %>% names()

# Test if there is spatial autocorrelation in the regression residuals (errors).
cont.neighb %>%
  spdep::moran.test(seResiduals, ., zero.policy = TRUE) 
# Cannot reject the null of no spatial dependence! spatial autocorrelation is gone!

# Add residuals to analysis data set
#county_new$residuals <- sp.err.model$resid 

## Saving results  ----
mod_err <- as.data.frame(summary(test1)$coefficients) %>% mutate(model = "err")

beta <-  as.data.frame(summary(sp.err.model)$coefficients) %>% 
  rownames_to_column("var") %>% 
  mutate(var = gsub("/.","",var)) %>% 
  mutate(var = gsub('[[:punct:] ]+',' ',var)) %>% 
  mutate(var = gsub("\\s+","",var)) 
  
  

se <-  as.data.frame(summary(sp.err.model)$rest.se) %>% 
  rownames_to_column("var") %>% 
  mutate(var = gsub('[[:punct:] ]+',' ',var)) %>% 
  mutate(var = gsub("\\s+","",var)) %>% 
  mutate(var = gsub("IxlambdaWX","",var)) %>% 
  mutate(var = gsub("/.","",var))
 
mod_err_results <- merge(beta,se,by="var") 


#### save results ----
saveRDS(mod_err_results,"/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_error_model_results.rds")

writexl::write_xlsx(mod_err_results, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/spatial_error_model_results.xlsx")


