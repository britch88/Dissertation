# Read in vi results and combine
grp1 <- readRDS(here::here("rda/vi results","vi_grp1_no_state.rds"))
grp2 <- readRDS(here::here("rda/vi results","vi_grp2_no_state.rds"))
grp3 <- readRDS(here::here("rda/vi results","vi_grp3_no_state.rds"))
grp4 <- readRDS(here::here("rda/vi results","vi_grp4_no_state.rds"))
grp5 <- readRDS(here::here("rda/vi results","vi_grp5_no_state.rds"))
grp6 <- readRDS(here::here("rda/vi results","vi_grp6_no_state.rds"))


all.results <- bind_rows(mutate(grp1,group=1),
                         mutate(grp2,group=2),
                         mutate(grp3,group=3),
                         mutate(grp4,group=4),
                         mutate(grp5,group=5),
                         mutate(grp6,group=6))

all.results$domain <- as.factor(ifelse(all.results$predictor %in% 
                                         c("prek_2017.gtp75","some_college_2017.gtp75","hs_grad_2017.gtp75"), 
                                       'education',
                                       
                                       ifelse(all.results$predictor %in% 
                                                c("bankrate2017.gtp75","unemployment_rate_2017.ltp25","broadband_2017.gtp75",
                                                  "median_household_income_2017.gtp75","poverty_2017.ltp25",
                                                  "income_ratio_2017.ltp25","rentburden_2017.ltp25"), 
                                              'economy',
                                              
                                              ifelse(all.results$predictor %in% 
                                                       c("lbw_2017.ltp25","uninsured_2017.ltp25","deaths_2017.ltp25"), 
                                                     'health',
                                                     
                                                     ifelse(all.results$predictor %in% 
                                                              c("volunteer_2017.gtp75","voting_2017.gtp75",
                                                                "disconnected.2017.ltp25","violent.2017.ltp25","pcp_2017.gtp75",
                                                                "food.2017.gtp75","incarcerated_2017.ltp25"), 
                                                            'community', 
                                                                                  'NA')))))



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

saveRDS(sig.results, here::here("rda/vi results","sig.results_no_state.rds"))
saveRDS(all.results, here::here("rda/vi results","all.results_no_state.rds"))


