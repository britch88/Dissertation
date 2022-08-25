# Packages
library(tidyverse)

# Read in trajectory results and combine
grp1 <- readRDS(here::here("rda/vi results","vi_grp1.rds"))
grp2 <- readRDS(here::here("rda/vi results","vi_grp2.rds"))
grp3 <- readRDS(here::here("rda/vi results","vi_grp3.rds"))
grp4 <- readRDS(here::here("rda/vi results","vi_grp4.rds"))
grp5 <- readRDS(here::here("rda/vi results","vi_grp5.rds"))
grp6 <- readRDS(here::here("rda/vi results","vi_grp6.rds"))


all.results <- bind_rows(mutate(grp1,group=1),
                         mutate(grp2,group=2),
                         mutate(grp3,group=3),
                         mutate(grp4,group=4),
                         mutate(grp5,group=5),
                         mutate(grp6,group=6))


#sig.results <- bind_cols(select(grp1,predictor,mutate(rgrp1=significant)(grp1,rgrp1 = significant) %>% ,
 #                        mutate(grp2,rgrp2 = significant) %>% select(predictor,rgrp2,-contains("low")))


                         
#put all data frames into list
df_list <- list(mutate(grp1,rgrp1 = significant) %>% select(predictor,rgrp1,-contains("low")),
               mutate(grp2,rgrp2 = significant) %>% select(predictor,rgrp2,-contains("low")), 
               mutate(grp3,rgrp3 = significant) %>% select(predictor,rgrp3,-contains("low")),
               mutate(grp4,rgrp4 = significant) %>% select(predictor,rgrp4,-contains("low")),
               mutate(grp5,rgrp5 = significant) %>% select(predictor,rgrp5,-contains("low")),
               mutate(grp6,rgrp6 = significant) %>% select(predictor,rgrp6,-contains("low")))

                         
#merge all data frames in list
sig.results <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) %>% 
  select(-lowValue) %>% 
  mutate_all(funs(str_replace(.,"Yes","1"))) %>% 
  mutate_all(funs(str_replace(.,"No","0"))) %>% 
  mutate(nsig = as.numeric(rgrp1) + 
           as.numeric(rgrp2) + 
           as.numeric(rgrp3) + 
           as.numeric(rgrp4) + 
           as.numeric(rgrp5) +
           as.numeric(rgrp6)) %>% 
  filter(nsig > 0)

#saveRDS(sig.results, here::here("rda/vi results","sig.results22_08_20.rds"))
#saveRDS(all.results, here::here("rda/vi results","all.results22_08_20.rds"))


