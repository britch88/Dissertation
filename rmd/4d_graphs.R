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

# Read in trajectory results and combine
grp1 <- readRDS(here::here("rda/vi results","vi_grp1.rds"))
grp1.plot <- filter(grp1, predictor %in% key.predictors)

outcome <- unique(grp1$outcome)
predictorLabels <- paste0(grp1.plot$predictor)


# Plot variable importance Group 1
ggplot(grp1.plot,
            aes(x = factor(predictor_levels),
                y = estimate_percent, color = significant)) +
  geom_segment(aes(xend = predictor_levels, yend = ci_high_percent), size = 1) +
  geom_segment(aes(xend = predictor_levels, yend = ci_low_percent), size = 1 ) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(estimate_percent), "%"), vjust = -.7), size = 4) +
  coord_flip() +
  theme_bw() +
  facet_grid(significant ~., scales = "free")+
  theme(legend.title = element_blank(), legend.position = "none",text = element_text(size = 12)) +
  scale_color_manual(values = c("Yes" = "forestgreen", "No" = "darkgrey")) +
  scale_x_discrete(labels = predictorLabels) +
  xlab("") +
  ylab("Estimate") +
  ggtitle(paste("Variable Importance of Predictors for Group 1 Membership")) +
  theme (plot.title = element_text(size = 12, face = "bold",vjust = 1,
                                   hjust = 0.5))



############### All groups
# The graph below ranks the relative importance of each target predictor to the outcome.
# Those predictors with green highlighted confidence intervals are those with statistically significant differences.
# Those with 1 have an approximately N% higher chance of being in group than those with 0

# Read in trajectory results and combine
all.plot <- filter(all.results, predictor %in% key.predictors)


outcome <- unique(all.plot$outcome)
predictorLabels <- paste0( all.plot$predictor)
# Plot variable importance 
ggplot2::ggplot(all.plot,
                aes(x = factor(predictor_levels),
                    y = estimate_percent, color = significant)
) +
  facet_grid(.~outcome) +
  ggplot2::geom_segment(
    aes(xend = predictor_levels, yend = ci_high_percent), size = 1
  ) +
  ggplot2::geom_segment(
    aes(xend = predictor_levels, yend = ci_low_percent), size = 1
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_text(
    aes(label = paste0(round(estimate_percent), "%"), vjust = -.7), size = 4
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.title = element_blank(), legend.position = "none",
                 text = element_text(size = 12)) +
  scale_color_manual(values = c("Yes" = "forestgreen", "No" = "darkgrey")) +
  scale_x_discrete(labels = predictorLabels) +
  xlab("") +
  ylab("Estimate") +
  ggtitle(
    paste("Variable Importance of Predictors for \nOutcome:", outcome)) +
  theme (plot.title = element_text(size = 12,
                                   face = "bold",
                                   vjust = 1,
                                   hjust = 0.5))



