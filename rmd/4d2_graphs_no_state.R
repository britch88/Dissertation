# Packages
library(tidyverse)

# List of key predictors
key.predictors <- c("bankrate2017.gtp75",  "bankrate2017.p25top75", 
                    "broadband_2017.gtp75", "broadband_2017.p25top75", "deaths_2017.lt10",
                    "deaths_2017.ltp25","deaths_2017.p25top75","disconnected.2017.p25top75",
                    "disconnected.2017.ltp25","female_2017.ltp25", "female_2017.gtp75",
                    "food.2017.gtp75","food.2017.p25top75",
                    "hs_grad_2017.p25top75", "hs_grad_2017.gtp75","incarcerated_2017.ltp25",
                    "incarcerated_2017.p25top75","income_ratio_2017.ltp25","income_ratio_2017.p25top75",
                    "lim_eng_2017.gtp75","lim_eng_2017.ltp25","lbw_2017.ltp25","lbw_2017.p25top75",
                    "median_household_income_2017.gtp75","median_household_income_2017.p25top75",
                    "pcp_2017.gtp75","pcp_2017.p25top75","poverty_2017.ltp25", "poverty_2017.p25top75","prek_2017.gtp75",
                    "prek_2017.p25top75", "rentburden_2017.ltp25", "rentburden_2017.p25top75", 
                    "rural_2017.gtp75", "rural_2017.ltp25", "segregation.2017.ltp25", 
                    "segregation.2017.p25top75", "some_college_2017.gtp75",
                    "some_college_2017.p25top75", "under18_2017.ltp25", "unemployment_rate_2017.p25top75", 
                    "unemployment_rate_2017.ltp25", "uninsured_2017.ltp25", "uninsured_2017.p25top75", 
                    "violent.2017.p25top75", "violent.2017.ltp25","volunteer_2017.gtp75", 
                    "volunteer_2017.p25top75","voting_2017.gtp75", "voting_2017.p25top75")

# "bankrate2017.na", "food.2017.na", "disconnected.2017.na", "segregation.2017.na", "violent.2017.na", 

sig.predictors <-c( )

slim.predictors <- c("bankrate2017.gtp75",  
                     "broadband_2017.gtp75",  
                    # "deaths_2017.lt10",
                     "deaths_2017.ltp25", 
                     "disconnected.2017.ltp25",
                    # "female_2017.ltp25", 
                    # "female_2017.gtp75",
                     "food.2017.gtp75",
                     "hs_grad_2017.gtp75",
                     "incarcerated_2017.ltp25",
                     "income_ratio_2017.ltp25",
                   #  "lim_eng_2017.gtp75",
                   #  "lim_eng_2017.ltp25",
                     "lbw_2017.ltp25",
                     "median_household_income_2017.gtp75",
                     "pcp_2017.gtp75",
                     "poverty_2017.ltp25", 
                     "prek_2017.gtp75",
                     "rentburden_2017.ltp25", 
                   #  "rural_2017.gtp75", 
                   #  "rural_2017.ltp25", 
                   #  "segregation.2017.ltp25", 
                     "some_college_2017.gtp75",
                  #   "under18_2017.ltp25",  
                     "unemployment_rate_2017.ltp25", 
                     "uninsured_2017.ltp25",  
                     "violent.2017.ltp25",
                     "volunteer_2017.gtp75", 
                     "voting_2017.gtp75")




############### All groups, state fixed effects removed
# The graph below ranks the relative importance of each target predictor to the outcome.
# Those predictors with green highlighted confidence intervals are those with statistically significant differences.
# Those with 1 have an approximately N% higher chance of being in group than those with 0

# Read in trajectory results and combine
all.results <- readRDS(here::here("rda/vi results","all.results_no_state.rds"))
all.plot <- filter(all.results, predictor %in% key.predictors)





### slim version
all.plot.slim <- filter(all.results, predictor %in% slim.predictors)

outcome <- unique(all.plot.slim$outcome)
predictorLabels <- paste0( all.plot.slim$predictor)
# Plot variable importance 
ggplot(all.plot.slim,  aes(x = factor(predictor_levels),y = estimate_percent, color = significant)) +
  facet_grid(domain~outcome, scales = "free_y") +
  geom_segment(aes(xend = predictor_levels, yend = ci_high_percent), size = 1) +
  geom_segment(aes(xend = predictor_levels, yend = ci_low_percent), size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(estimate_percent), "%"), vjust = -.7), size = 4) +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", text = element_text(size = 12)) +
  scale_color_manual(values = c("Yes" = "forestgreen", "No" = "darkgrey")) +
  scale_y_discrete(labels = predictorLabels) +
  xlab("") +
  ylab("Estimate") +
  ggtitle(paste("Variable Importance of Opportunity Indicators")) +
  theme (plot.title = element_text(size = 12, face = "bold",vjust = 1, hjust = 0.5))

#break up the plot a little
ggplot(all.plot.slim %>% filter(domain=="community"),  aes(x = factor(predictor_levels),y = estimate_percent, color = significant)) +
  facet_grid(domain~outcome, scales = "free_y") +
  geom_segment(aes(xend = predictor_levels, yend = ci_high_percent), size = 1) +
  geom_segment(aes(xend = predictor_levels, yend = ci_low_percent), size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(estimate_percent), "%"), vjust = -.7), size = 4) +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", text = element_text(size = 12)) +
  scale_color_manual(values = c("Yes" = "forestgreen", "No" = "darkgrey")) +
  scale_y_discrete(labels = predictorLabels) +
  xlab("") +
  ylab("Estimate") +
  ggtitle(paste("Variable Importance of Opportunity Indicators")) +
  theme (plot.title = element_text(size = 12, face = "bold",vjust = 1, hjust = 0.5))

### Negative predictors
mod.predictors <- c("bankrate2017.p25top75",  
                     "broadband_2017.p25top75",  
                     "deaths_2017.p25top75", 
                     "disconnected.2017.p25top75",
                     "food.2017.p25top75",
                     "hs_grad_2017.p25top75",
                     "incarcerated_2017.p25top75",
                     "income_ratio_2017.p25top75",
                     "lbw_2017.p25top75",
                     "median_household_income_2017.p25top75",
                     "pcp_2017.p25top75",
                     "poverty_2017.p25top75", 
                     "prek_2017.p25top75",
                     "rentburden_2017.p25top75", 
                     "some_college_2017.p25top75",
                     "unemployment_rate_2017.p25top75", 
                     "uninsured_2017.p25top75",  
                     "violent.2017.p25top75",
                     "volunteer_2017.p25top75", 
                     "voting_2017.p25top75")



### moderate version
all.plot.mod <- filter(all.results, predictor %in% mod.predictors)

outcome.mod <- unique(all.plot.mod$outcome)
predictorLabelsmod <- paste0( all.plot.mod$predictor)

ggplot(all.plot.mod,  aes(x = factor(predictor_levels),y = estimate_percent, color = significant)) +
  facet_grid(.~outcome, scales = "free_y") +
  geom_segment(aes(xend = predictor_levels, yend = ci_high_percent), size = 1) +
  geom_segment(aes(xend = predictor_levels, yend = ci_low_percent), size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(estimate_percent), "%"), vjust = -.7), size = 4) +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", text = element_text(size = 12)) +
  scale_color_manual(values = c("Yes" = "forestgreen", "No" = "darkgrey")) +
  scale_y_discrete(labels = predictorLabelsmod) +
  xlab("") +
  ylab("Estimate") +
  ggtitle(paste("Variable Importance of Opportunity Indicators")) +
  theme (plot.title = element_text(size = 12, face = "bold",vjust = 1, hjust = 0.5))

## Demographic indicators

demog.predictors <- c(  
                      "female_2017.ltp25", 
                      "female_2017.gtp75",
                       "lim_eng_2017.gtp75",
                       "lim_eng_2017.ltp25",
                       "rural_2017.gtp75", 
                       "rural_2017.ltp25", 
                       "segregation.2017.ltp25", 
                      "segregation.2017.p25top75",               
                      "under18_2017.ltp25",
                      "under18_2017.gtp75" ,
                      "black_2017.gtp75",
                      "black_2017.ltp25",
                      "white_2017.gtp75",
                      "white_2017.ltp25",
                      "region_other",
                      "region_West",
                      "region_Northeast",
                      "region_Midwest",
                      "native_2017.gtp75",                     
                      "native_2017.ltp25",                    
                      "over64_2017.gtp75",                     
                      "over64_2017.ltp25",                    
                      "pacific_islander_2017.gtp75",           
                      "pacific_islander_2017.ltp25", 
                      "hispanic_2017.gtp75",                   
                      "hispanic_2017.ltp25"
                      
)

all.plot.demog <- filter(all.results, predictor %in% demog.predictors)
outcome.demog <- unique(all.plot.demog$outcome)
predictorLabelsdemog <- paste0( all.plot.demog$predictor)

ggplot(all.plot.demog,  aes(x = factor(predictor_levels),y = estimate_percent, color = significant)) +
  facet_grid(.~outcome, scales = "free_y") +
  geom_segment(aes(xend = predictor_levels, yend = ci_high_percent), size = 1) +
  geom_segment(aes(xend = predictor_levels, yend = ci_low_percent), size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(estimate_percent), "%"), vjust = -.7), size = 4) +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", text = element_text(size = 12)) +
  scale_color_manual(values = c("Yes" = "forestgreen", "No" = "darkgrey")) +
  scale_y_discrete(labels = predictorLabelsdemog) +
  xlab("") +
  ylab("Estimate") +
  ggtitle(paste("Variable Importance of Opportunity Indicators")) +
  theme (plot.title = element_text(size = 12, face = "bold",vjust = 1, hjust = 0.5))

