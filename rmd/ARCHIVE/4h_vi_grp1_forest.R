##### Specify directories

datapath <- "rda"    # Insert folder storing the data object
datasetname <- "analysis.vi.rds" # Insert the name of the data object

##### Loading Packages

library(discrim)
library(dplyr)        # For Data Processing
library(DT)           # For pretty markdown tables
library(ggplot2)      # For Visualizing in ggplots
library(ggpubr)       # For ggplot publication comparison
library(furrr)        # For parallelization
library(future)       # For parallelization
library(here)         # For Link & Relative Path Setting
library(kableExtra)   # For Table formatting
library(kernlab)      # For ML algorithms
library(magrittr)     # For Piping
library(naivebayes)   # For naive bayes ML algorithm
library(parsnip)      # Framework to put model through
library(purrr)        # For mapping functions
library(ranger)       # For random forest algorithm
library(Rcpp)
#library(renv)         # For package version stability
library(rio)          # For Reading in all sorts of data files: Function
library(stringr)      # For string expressions
library(tidymodels)   # For Tidy Model framework
library(vip)          # For Variable Importance calculation and plots
library(xgboost)      # For xgboost algorithm
library(yardstick)    # For model results
library(visdat)       # For visualizing missingness



#### Sourcing scripts
source(here::here("rmd", "0b_variable_importance.R"))
source(here::here("rmd", "0d_helpers.R"))
source(here::here("rmd", "0e_learner_training.R"))

# load the data set
dat <-readRDS(here::here("rda","analysis_vi_2017.rds"))

# Specify learners
# Insert both X and A predictors
predset_x01 <- c(
  "bankrate2017.na",                    "bankrate2017.gtp75",   "bankrate2017.p25top75",         
  "poverty_2017.ltp25",                 "poverty_2017.p25top75",          
  "unemployment_rate_2017.ltp25",       "unemployment_rate_2017.p25top75",          
  "median_household_income_2017.gtp75", "median_household_income_2017.p25top75",    
  "some_college_2017.gtp75",            "some_college_2017.p25top75",     
  "hs_grad_2017.gtp75",                 "hs_grad_2017.p25top75",          
  "broadband_2017.gtp75",               "broadband_2017.p25top75",
  "disconnected.2017.na",               "disconnected.2017.ltp25",              "disconnected.2017.p25top75",    
  "food.2017.na",                       "food.2017.gtp75",     "food.2017.p25top75",   
  "violent.2017.na",                    "violent.2017.ltp25",     "violent.2017.p25top75",          
  "segregation.2017.na",                "segregation.2017.ltp25",                  "segregation.2017.p25top75",     
  "pcp_2017.gtp75",                     "pcp_2017.p25top75",   
  "lbw_2017.ltp25",                     "lbw_2017.p25top75",   
  "income_ratio_2017.ltp25",            "income_ratio_2017.p25top75",    
  "uninsured_2017.ltp25",               "uninsured_2017.p25top75",       
  
  
  "under18_2017.ltp25",   "under18_2017.gtp75",            
  "over64_2017.ltp25",    "over64_2017.gtp75",              
  "black_2017.ltp25",     "black_2017.gtp75",                
  "native_2017.ltp25",    "native_2017.gtp75",              
  "pacific_islander_2017.ltp25",    "pacific_islander_2017.gtp75",             
  "hispanic_2017.ltp25",  "hispanic_2017.gtp75",         
  "white_2017.ltp25",     "white_2017.gtp75",               
  "lim_eng_2017.ltp25",   "lim_eng_2017.gtp75",      
  "female_2017.ltp25",    "female_2017.gtp75",              
  "rural_2017.ltp25",     "rural_2017.gtp75",     
  
  "prek_2017.gtp75",      "prek_2017.p25top75",  
  "rentburden_2017.ltp25",                  "rentburden_2017.p25top75",      
  "deaths_2017.lt10",     "deaths_2017.ltp25",    "deaths_2017.p25top75",           
  "incarcerated_2017.ltp25",  "incarcerated_2017.p25top75",     
  "voting_2017.gtp75",             "voting_2017.p25top75",           
  "volunteer_2017.gtp75",          "volunteer_2017.p25top75",        
  
  # Leaving out South which has most counties
  "region_Midwest",       "region_Northeast",  "region_other",        "region_West", 
  
  # Leaving out west north central
  "country_division_east north central",     "country_division_east south central",     "country_division_middle atlantic",       
  "country_division_mountain",     "country_division_new england",  "country_division_pacific",     
  "country_division_possessions",  "country_division_south atlantic",         "country_division_west north central",    
  "country_division_west south central"
  
)  


# User should insert more if they have more predictor sets
allPredsets <- list(
  predset_x01 = predset_x01 
)
# Insert name of outcome
outcomeName <- c("Grp1") 
# Insert modeling approaches for each of the predictor sets
specifiedLearners <-
  list(
    predset_x01 = c("naive_bayes") #, "random_forest") 
  )


######## Tuning algorithms
# for glm 
tune_glm <- data.frame(
  penalty = NULL
)
# for lasso 
# lambda (termed penalty here) is the amount of regularization,
# mixture (same as alpha) = 1 means lasso here defines the type of regularization
# mixture (alpha) = 0 means ridge regression
tune_lasso <- expand.grid(
  penalty = 0.1,
  mixture = 1
) 
tune_random_forest <- data.frame(
  m_tries_opt = 3,
  ntrees_opt = 20,
  max_depth_opt = 5
)
tune_xgboost <- data.frame(
  tree_depth = 5,
  trees = 5,
  learn_rate = 0.2
)
tune_naive_bayes <- data.frame(
  smoothness = 0.5,
  Laplace = "NULL"
)
tune_svm_poly <- data.frame(cost = 10, 
                            degree = 2, 
                            scale_factor = .5, 
                            margin = .15)
tune_svm_rbf <- data.frame(
  cost = 10, 
  rbf_sigma = 1, 
  margin = .05
)

# In the code chunk below, we save the learner names and specifications.

learnerNames <- NULL
counter <- 1
for (i in 1:length(allPredsets))
{
  predsetName <- names(allPredsets)[i]
  algorithmList <- unlist(specifiedLearners[predsetName])
  for (j in 1:length(algorithmList)){
    ML <- algorithmList[[j]]
    learnerName <- paste(ML, predsetName, 1, sep = "_")
    learnerNames[counter] <- learnerName
    counter <- counter + 1
  }
}
learnerSpec <- list(
  datapath = datapath, 
  trainingDataName = datasetname,
  outcomeName = outcomeName,
  allPredsets = allPredsets,
  specifiedLearners = specifiedLearners,
  learnerNames = learnerNames,
  tuningSets = list( # add additional algorithms here if more added above
    glm = tune_glm,
    random_forest = tune_random_forest,
    lasso = tune_lasso,
    naive_bayes = tune_naive_bayes,
    xgboost = tune_xgboost,
    svm_poly = tune_svm_poly,
    svm_rbf = tune_svm_rbf
  )
)

print(learnerNames)


#drop unused variables
dat2 <- dat %>% select(predset_x01,Grp1)
# Checking for missing data
#summary(dat2)
#visdat::vis_dat(dat)
#visdat::vis_miss(dat)
check.miss <- as.data.frame(100 * (apply(apply(dat2, 2, is.na), 2, sum)/nrow(dat)))



# Deleting records with missing predictors

dat3 <- drop_na(dat2,c(predset_x01, "Grp1"))
# 230 records deleted

# dat4 <- dat2 %>% select( bankrate.na ,  bankrate.ltp25 ,  bankrate.gtp75 , 
#                          poverty_2017.ltp25 , poverty_2017.gtp75 ,  unemployment_rate_2017.ltp25 , 
#                          unemployment_rate_2017.gtp75 ,  median_household_income_2017.gtp75 ,
#                          median_household_income_2017.ltp25 ,  some_college_2017.ltp25 ,
#                          some_college_2017.gtp75 ,  hs_grad_2017.ltp25 ,  hs_grad_2017.gtp75 ,
#                          broadband_2017.ltp25 , broadband_2017.gtp75 ,  Grp1 ) %>% 
#   mutate_if(is.logical,as.numeric)

#-----------------------------------#
outcome_variable <- "Grp1" # Insert outcome variable
continuous_variables <- c() # Insert continuous variable
factor_variables <- predset_x01 # Insert factor variable
#-----------------------------------#

lows <-rep(c(0),each=82)
highs <-rep(c(1),each=82)

#-------------------------#
learnerName <- "naive_bayes_predset_x01_1" # Insert Learner Name
varInfo <- tibble(
  # Insert target predictors for variable importance analysis
  varSelect = predset_x01, 
  # Note that your varLowVal and varHighVal specifications have to match correspondingly
  # in index position in vector here with the target predictors you specified in varSelect.
  varLowVal  = lows, 
  varHighVal = highs 
)
bootDraws <- 1000
stratVar <- NULL
#-------------------------#
print(varInfo)

## Estimation

vi_est <- get_all_vi_ests(
  dat = dat3,
  learnerName = learnerName,
  learnerSpec = learnerSpec,
  varInfo = varInfo,
  stratVar = stratVar,
  bootDraws = bootDraws,
  parallel = TRUE
)



vi_est2 <- vi_est %>% 
  dplyr::group_by(predictor, lowValue, highValue) %>%
  dplyr::summarise(estimate = mean(est, na.rm = TRUE), 
                   se = sd(est, na.rm = TRUE), 
                   ci_low = quantile(est, .025, na.rm = TRUE), 
                   ci_high = quantile(est, .975, na.rm = TRUE)) %>%
  dplyr::mutate(
    outcome = learnerSpec$outcomeName) %>%
  dplyr::mutate(significant = 
                  factor(if_else((ci_low < 0 & ci_high < 0) | (ci_low > 0 & ci_high > 0),
                                 "Yes", "No"), levels = c("Yes", "No"))) 
head(vi_est2)


vi_est2_summary <- vi_est2 %>% 
  mutate(estimate_percent = round(100 * estimate, 2),
         ci_low_percent = round(100 * ci_low, 2),
         ci_high_percent = round(100 * ci_high, 2)) %>%
  mutate(predictor_levels = paste(predictor, lowValue, highValue, sep = "_")) %>%
  select(predictor_levels, predictor, lowValue, highValue,
         estimate_percent, ci_low_percent, ci_high_percent,
         outcome, significant)

head(vi_est2_summary)

saveRDS(vi_est2_summary, here::here("rda/vi results","vi_grp1_bayes.rds"))

# The graph below ranks the relative importance of each target predictor to the outcome.
# Those predictors with green highlighted confidence intervals are those with statistically significant differences.
# Those with 1 have an approximately N% higher chance of being in group than those with 0

outcome <- unique(vi_est2_summary$outcome)
predictorLabels <- paste0(
  vi_est2_summary$predictor, "\n",
  vi_est2_summary$lowValue, " to ",
  vi_est2_summary$highValue
)
# Plot variable importance 
ggplot2::ggplot(vi_est2_summary,
                aes(x = factor(predictor_levels),
                    y = estimate_percent, color = significant)
) +
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


