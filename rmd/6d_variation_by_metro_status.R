# title: "Variation in Change from 2000 to 2019 by County and Metro Status"
# author: "Brit Henderson"
# date: "12/29/2022"

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

# Read in metro status data
metstat2013 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/metstat2013.rds")


############### Calculate county change from 2000 to 2019 
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
                        county.change = rate2019 - rate2000) %>% 
  filter(!state %in% c('canal zone','guam', 'puerto rico')) %>% 
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


# Merge met stat and county change data

county.change2 <- merge(county.change, metstat2013, by = "fips_clean", all.x = TRUE, all.y = FALSE)

state.met.change <- county.change2 %>% 
  group_by(state, metstatcat) %>% 
  summarise(mean.change = mean(county.change, na.rm =)) %>% 
  ungroup() %>% 
  filter(is.na(mean.change)==0)


#### Create forest plot faceted by metro type

ggplot(state.met.change, aes(x = mean.change, y = state)) +
  geom_line(color = "gray") +
  geom_point(color = "darkorange2") +
  facet_wrap(.~metstatcat ) +
  labs(x = "Change 2000 - 2019", y = "state") +
  theme_bw()

#large <- metstat2013 %>% 
  
# Large metro
ggplot(filter(state.met.change, metstatcat == "Large Metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
  geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
  geom_point(color = "darkorange2") +
  coord_flip() +
  scale_y_continuous(limits = c(-200,110)) +
  facet_wrap(.~metstatcat, scales = "free" ) +
  labs(y = "Change 2000 - 2019", x = "state") +
  theme_bw()
  

# Medium metro  
  ggplot(filter(state.met.change, metstatcat == "Medium Metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
    geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
    geom_point(color = "darkorange2") +
    coord_flip() +
    scale_y_continuous(limits = c(-200,110)) +
    facet_wrap(.~metstatcat, scales = "free" ) +
    labs(x = "Change 2000 - 2019", y = "state") +
  theme_bw()

  
# Small metro  
ggplot(filter(state.met.change, metstatcat == "Small Metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
  geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
  geom_point(color = "darkorange2") +
  coord_flip() +
  scale_y_continuous(limits = c(-200,110)) +
  facet_wrap(.~metstatcat, scales = "free" ) +
  labs(x = "Change 2000 - 2019", y = "state") +
  theme_bw()



# Non-metro  
ggplot(filter(state.met.change, metstatcat == "Non-metro"), aes(x = reorder(state,mean.change), y = mean.change)) +
  geom_segment(aes(x=reorder(state,mean.change), xend=state, y=0, yend=mean.change), col = "blue") +
  geom_point(color = "darkorange2") +
  coord_flip() +
  scale_y_continuous(limits = c(-200,110)) +
  facet_wrap(.~metstatcat, scales = "free" ) +
  labs(x = "Change 2000 - 2019", y = "state") +
  theme_bw()

# ggplot(data=A, aes(x=IV, y=ES, ymin=LCI, ymax=UCI)) +
#   geom_pointrange()+ # Makes range for ggplot values based on the data and AES specified in first line
#   geom_hline(yintercept=0, lty=2, size =1) +  # add a dotted line at x=0 after flip
#   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0.5, cex=1)+ # Makes whiskers on the range (more aesthetically pleasing)
#   facet_wrap(~DV)+ # Makes DV header (Can handle multiple DVs)
#   coord_flip() + # flip coordinates (puts labels on y axis)
#   geom_point(shape = 15, size = 2) + # specifies the size and shape of the geompoint
#   ggtitle("")+ # Blank Title for the Graph
#   xlab("Independent Variables") + # Label on the Y axis (flipped specification do to coord_flip)
#   ylab("b (95% CI)") + # Label on the X axis (flipped specification do to coord_flip)
#   scale_y_continuous(limits = c(-.50,.50), breaks = c(-.50,-.25,0,.25,.50))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
#   theme(line = element_line(colour = "black", size = 3), # My personal theme for GGplots
#         strip.background = element_rect(fill="gray90"), 
#         legend.position ="none", 
#         axis.line.x = element_line(colour = "black"), 
#         axis.line.y = element_blank(), 
#         panel.border= element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.ticks = element_blank(),
#         axis.title.x = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         plot.title = element_text(family="Times New Roman", colour = "Black", margin = margin(t = 0, r = 0, b = 20, l = 0)),
#         axis.text=element_text(family="Times New Roman",size=24, color = "Black"), 
#         text=element_text(family="Times New Roman",size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))
# 
# 











