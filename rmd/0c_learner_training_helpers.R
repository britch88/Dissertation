# metric_cols <- c("mean_AUC_ROC", "sd_AUC_ROC", "min_AUC_ROC",
#                  "mean_AUC_PR", "sd_AUC_PR", "min_AUC_PR",
#                  "mean_MSE", "sd_MSE", "max_MSE",
#                  "mean_RMSE","sd_RMSE", "max_RMSE",
#                  "mean_logloss", "sd_logloss",
#                  "mean_r2", "sd_r2",
#                  "mean_accuracy", "sd_accuracy",
#                  "mean_precision", "sd_precision",
#                  "mean_f1_man", "sd_f1_man",
#                  "mean_min_predProbs", "sd_min_predProbs",
#                  "mean_mean_predProbs", "sd_mean_predProbs",
#                  "mean_max_predProbs", "sd_max_predProbs",
#                  "mean_first_Q_predProbs", "sd_first_Q_predProbs",
#                  "mean_median_predProbs", "sd_median_predProbs",
#                  "mean_third_Q_predProbs", "sd_third_Q_predProbs",
#                  "mean_tpr", "sd_tpr",
#                  "mean_tpc", "sd_tpc",
#                  "mean_tnr", "sd_tnr",
#                  "mean_tnc", "sd_tnc",
#                  "mean_fpr", "sd_fpr",
#                  "mean_fpc", "sd_fpc",
#                  "mean_fnr", "sd_fnr",
#                  "mean_fnc", "sd_fnc")

metric_cols <- c("mean_AUC_ROC", "sd_AUC_ROC", "min_AUC_ROC",
                 "mean_AUC_PR", "sd_AUC_PR", "min_AUC_PR",
                 "mean_MSE", "sd_MSE", "max_MSE",
                 "mean_RMSE","sd_RMSE", "max_RMSE",
                 "mean_logloss", "sd_logloss",
                 "mean_r2", "sd_r2")

#' Function that converts a named list of list with variable length to a
#' 2 column data frames with variable length strings in 1 column
#'
#' @param listoflist a named list of list with varying length
#'
#' @return a data frame with that contains the predictor sets of varying lengths
#' @export
#'
#' @examples
list_to_frame <- function(listoflist){
  
  # getting the length of each varying named list
  list_length <- length(listoflist)
  
  tmp <- as.data.frame(matrix(NA, nrow = list_length, ncol = 2))
  names(tmp) <- c("Predictor_Sets", "Predictor_Names")
  tmp$Predictor_Sets <- names(listoflist)
  tmp$Predictor_Names <- sapply(listoflist, toString)
  # returning flat group of data frames
  return(tmp)
}

#' Creating Cross validation folds
#'
#' @param data the training data set to be cross validated
#' @param nfolds the number of folds we want
#' @param seed seed for persistent memory: We would have this be empty if we want to run repeated k-folds
#' @author CDI: Zarni Htet
#' @return a data set with new columns for indexes and folds
#' @export
#'
#' @examples
fold_create <- function(data,nfolds,seed){
  
  #if the fold is not a numeric value of a group K nature deriving from a categorical/factor column
  if(!is.numeric(nfolds)){
    
    # check if the specified column exists
    if(nfolds %in% colnames(data) == TRUE) {
      
      # Setting the seed to make random states fixed
      set.seed(1234)
      
      # Return the folds with factor level (numeric value) of the specified column as the designated fold
      data$folds <- as.numeric(as.factor(data[[nfolds]]))
      
      # Return data
      return(data)
      
    } else {
      
      print("Your input on nfold is incorrect. It has to be either a
      numeric value or a string/character value matching
            one of the column in your data frame that you want to fold by.")
      
    } #inner else
    
  } else {
    
    # set the random seed
    set.seed(seed)
    
    # create row indexes for the data set to save which indexes are which
    data$rowIndex <- 1:nrow(data)
    
    # shuffle the data set by the row Index
    shuff <- base::sample(data$rowIndex)
    
    # rearrange the data set by the shuffle
    data <- data[shuff,]
    
    # create K-number of folds
    folds <- cut(seq(1, nrow(data)), breaks = nfolds, labels = FALSE)
    
    # put the designated fold number for each of the row back into the data set
    data$folds <- folds
    
    return(data)
    
  } # outer else
  
}

#' Function to create stratified cross fold
#'
#' This function splits the data by the column we specify.
#' For instance, if our binary outcome is imbalanced, we would stratify by the outcome.
#' There will be two groups of data - positive class and negative class.
#' Those two groups will then be put into a create fold function that will
#' # create either numbered folds or group K folds
#'
#'
#' @param data training sample
#' @param nfolds number of folds (could be numeric or a factor/categorical column)
#' @author CDI: Zarni Htet
#' @param strat stratification covariate
#' @param seed the seed to produce these folds
#'
#' @return
#' @export
#'
#' @examples
strat_fold <- function (data, nfolds, strat, seed){
  
  if (strat %in% colnames(data) == TRUE ){
    
    # Split the data by stratifying covariate
    strat_data <- data %>% group_split(!!as.symbol(strat))
    
    # Apply the stratified data to the cross fold function to create numbered folds
    # or group k folds specified on category/factor column
    strat_cross_fold <- purrr::map(.x = strat_data, .f = fold_create, nfolds = nfolds, seed = seed)
    
    # Collapse the strat_cross_fold data into a single data frame
    strat_cross_fold <- bind_rows(strat_cross_fold)
    
    # Rearrange the stratification cross fold by fold order and return the data object
    strat_cross_fold <- strat_cross_fold %>% arrange(folds)
    
    # Convert back to matrix for subsetting in models_results function
    strat_cross_fold <- as.data.frame(strat_cross_fold)
    
  } else {
    
    print("Note - No stratification is specified in cross validation. If you want to stratify your folds, respecify in the cross validation section on 01_03.")
    strat_cross_fold <- as.data.frame(fold_create(data = data, nfolds = nfolds, seed = seed))
  }
  
  return(strat_cross_fold)
}

#' The function to create the mean and standard deviation/standard error
#' of the performance metrics of the algorithims
#'
#' @param dat data frame containing the performance metrics of all learners for each fold
#'
#' @return a list of data frame containing
#' mean and standard deviation results of the performance metrics of all learners for each fold
#' @export
#'
#' @examples
metric_mean_sd <- function(dat){
  
  # Converting to a data frame
  dat <- as.data.frame(dat)
  
  # Columns that we are dropping
  drop_col <- c("learnerName", "ML",
                "predSet", "tuningSet","y_true_vec")
  
  # tmp data set to calculate the mean and standard deviation
  metricsDat <- dat[, !(names(dat) %in% drop_col)]
  
  # Calculate the mean values for the performance metrics
  learner_mean <- apply(metricsDat, 2, mean)
  learner_mean <- as.data.frame(t(as.data.frame(learner_mean)))
  colnames(learner_mean) <- paste("mean", colnames(learner_mean), sep ="_")
  
  # Calculate the standard deviation values for the performance metrics
  learner_sd <- apply(metricsDat, 2, sd)
  learner_sd <- as.data.frame(t(as.data.frame(learner_sd)))
  colnames(learner_sd) <- paste("sd", colnames(learner_sd), sep = "_")
  
  # Calculate the min values for the performance metrics
  learner_min <- apply(metricsDat, 2, min)
  learner_min <- as.data.frame(t(as.data.frame(learner_min)))
  colnames(learner_min) <- paste("min", colnames(learner_min), sep = "_")
  
  # Calculate the max values for the performance metrics
  learner_max <- apply(metricsDat, 2, max)
  learner_max <- as.data.frame(t(as.data.frame(learner_max)))
  colnames(learner_max) <- paste("max", colnames(learner_max), sep = "_")
  
  # Combining mean and standard deviation
  learnerDat <- cbind(learner_mean, learner_sd, learner_min, learner_max)
  
  # Rearrange the column values
  learnerDat <- learnerDat[,metric_cols]
  
  # Add the ML, Predictor Set and Tuning Set into the calculation from the 1st row of the X folds
  learnerDat$ML <- dat$ML[1]
  learnerDat$predSet <- dat$predSet[1]
  learnerDat$tuningSet <- dat$tuningSet[1]
  
  # Creating the new learner name
  learnerDat <- within(learnerDat, learnerName <- paste(ML, predSet, tuningSet, sep = "_"))
  
  # Listing out learner columns to be arranged
  learner_cols <- c("learnerName", metric_cols, "ML", "predSet", "tuningSet")
  
  # rearraging the learner data frame
  learnerDat <- learnerDat[, learner_cols]
  
  return(learnerDat)
  
}

#' Combine the summary stats metrics of each learner into one table
#'
#' @param perfResults a data frame of performance metric results across folds
#' @param listOfPredictorSets passing in the training specifications to get the predictor list
#'
#' @return return a single data frame containing the summary stats metrics of each learner with predictor
#'         set details
#' @export
#'
#' @examples
metric_mean_sd_combine <- function(perfResults, allPredsets){
  
  comb_results <- NULL
  # summarize
  for(learner in unique(perfResults$learnerName))
  {
    learner_dat <- perfResults %>%
      filter(learnerName == learner)
    
    learner_results <- metric_mean_sd(learner_dat)
    comb_results <- dplyr::bind_rows(comb_results, learner_results)
  }
  
  # Joining the comb_results to training specifications predictor list to
  # have a single table with predictor list details
  predictorSetTable <- list_to_frame(allPredsets)
  comb_results <- comb_results %>% left_join(predictorSetTable, by = c("predSet" = "Predictor_Sets"))
  
  return(comb_results)
}

#' Combined a list of data frames & return the specified column name (which is the metrics measurement of interest)
#'
#' @param list A list of data frames with performance metrics of each learner for each fold
#' @param col the specific metrics we want to return (optional)
#'
#' @return a combined data frame of the performance metrics of each learner for each fold or one metrics
#' @export
#'
#' @examples
comb_listdata_column <- function(list,col =NA){
  
  comb_data <- list %>% dplyr::bind_rows(.id = "learnerName")
  comb_data <- within(comb_data, learnerName <- paste(ML, predSet, tuningSet, sep = "_"))
  
  # if a particular column is not null in in R, otherwise, return the whole data frame
  if(!is.na(col)){
    
    comb_data <- comb_data %>% dplyr::select(learnerName, col) %>%
      arrange(desc(!!(sym(col)))) #arrange does not take in string arguments. It has to be a symbol, thus sym
    
  }
  
  # Convering the combined list to a dataframe
  comb_data <- as.data.frame(comb_data)
  return(comb_data)
}


#' Function calculates the mean and standard deviation of all the learners
#'
#' This function like metric_mean_sd_combine calculates the mean and standard deviation of
#' each of the learners. The main difference between the two functions is that this function uses
#' newly added indicators of Machine Learning Algorithm, Predictor Set and Tuning parameter from a
#' combined data frame (instead of a list of data frames like earlier) to estimate the mean and
#' standard deviation of each of the metrics.
#'
#' @param all_learners_measures_table a data frame of the metrics of all the learners
#'
#' @return
#' @export
#'
#' @examples
metric_mean_sd_combine_plyr <- function(all_learners_measures_table){
  
  
  # all learners grouped by Machine Learning algorithim, predictor set and tuning set
  # mean and standard deviations are calculated for each of the measures
  
  all_learners_measures_sum_plyr <-
    all_learners_measures_table %>% group_by(ML, predSet, tuningSet) %>%
    dplyr::summarise(mean_auc_roc = mean(AUC_ROC),
                     sd_auc_roc = sd(AUC_ROC),
                     min_auc_roc = min(AUC_ROC),
                     mean_auc_pr = mean(AUC_PR),
                     sd_auc_pr = sd(AUC_PR),
                     min_auc_pr = min(AUC_PR),
                     mean_MSE = mean(MSE),
                     sd_MSE = sd(MSE),
                     max_MSE = max(MSE),
                     mean_RMSE = mean(RMSE),
                     sd_RMSE = sd(RMSE),
                     max_RMSE = max(RMSE),
                     mean_logloss = mean(logloss),
                     sd_logloss = sd(logloss),
                     mean_r2 = mean(r2), sd_r2 = sd(r2)
                     # mean_accuracy = mean(accuracy), sd_accuracy = sd(accuracy),
                     # mean_precision = mean(precision), sd_precision = sd(precision),
                     # mean_f1_man = mean(f1_man),
                     # sd_f1_man = sd(f1_man),
                     # mean_min_predProbs = mean(min_predProbs),
                     # sd_min_predProbs = sd(min_predProbs),
                     # mean_mean_predProbs = mean(mean_predProbs),
                     # sd_mean_predProbs = sd(mean_predProbs),
                     # mean_max_predProbs = mean(max_predProbs),
                     # sd_max_predProbs = sd(max_predProbs),
                     # mean_first_Q_predProbs = mean(first_Q_predProbs),
                     # sd_first_Q_predProbs = sd(first_Q_predProbs),
                     # mean_median_predProbs = mean(median_predProbs),
                     # sd_median_predProbs = sd(median_predProbs),
                     # mean_third_Q_predProbs = mean(third_Q_predProbs),
                     # sd_third_Q_predProbs = sd(third_Q_predProbs),
                     # mean_tpr = mean(tpr),
                     # sd_tpr = sd(tpr),
                     # mean_tpc = mean(tpc),
                     # sd_tpc = sd(tpc),
                     # mean_tnr = mean(tnr),
                     # sd_tnr = sd(tnr),
                     # mean_tnc = mean(tnc),
                     # sd_tnc = sd(tnc),
                     # mean_fpr = mean(fpr),
                     # sd_fpr = sd(fpr),
                     # mean_fpc = mean(fpc),
                     # sd_fpc = sd(fpc),
                     # mean_fnr = mean(fnr),
                     # sd_fnr = sd(fnr),
                     # mean_fnc = mean(fnc),
                     # sd_fnc = sd(fnc)
    )
  
  # learners' mean and standard deviation into a single data frame
  
  all_learners_measures_sum_plyr <- within(all_learners_measures_sum_plyr,
                                           learnerName <- paste(ML, predSet, tuningSet, sep = "_"))
  
  # Returning all_learners_measures_table with name of learners
  
  return(all_learners_measures_sum_plyr)
}

#' Function that will pick the best algorithim and predictor set out of all tuning options we have
#' based on the metric of selection.
#'
#' @param all_learners_measures_table all the learners' metric for mean and standard deviation
#' @param metric_f_best the metric by which we will tune the model (AUC, MSE, RMSE etc)
#'
#' @return
#' @export
#'
#' @examples
metric_mean_sd_combine_best <- function(all_learners_measures_table, metric_f_best){
  
  # Estimate the mean and standard deviation
  all_learners_measures_sum_plyr <- metric_mean_sd_combine_plyr(all_learners_measures_table)
  
  # Sort the metrics by specified metrics
  learners_best <- all_learners_measures_sum_plyr %>%
    dplyr::group_by(ML, predSet) %>%
    dplyr::arrange(desc(!!(sym(metric_f_best)))) %>%
    dplyr::slice(1:1)
  return(learners_best)
}

#' Function that will pick the best algorithim and predictor set out of all tuning options we have
#' based on the metric of selection. It will also only return the table with the selected metrics
#'
#' @param all_learners_measures_table all the learners' metric for mean and standard deviation
#' @param metric_f_best the metric by which we will tune the model (AUC, MSE, RMSE etc)
#'
#' @return
#' @export
#'
#' @examples
metric_mean_sd_combine_best_spec <- function(all_learners_measures_table, metric_f_best){
  
  # Estimate the mean and standard deviation
  all_learners_measures_sum_plyr <- metric_mean_sd_combine_plyr(all_learners_measures_table)
  
  # Sort the metrics by specified metrics
  learners_best <- all_learners_measures_sum_plyr %>% group_by(ML, predSet) %>%
    arrange(desc(!!(sym(metric_f_best)))) %>% slice(1)
  learners_best <- learners_best[, c("learnerName","ML", "predSet", metric_f_best)]
  learners_best %>% arrange(ML, predSet) %>% print()
  
  return(learners_best)
}

#' Function that will pick the best algorithim and predictor set out of all tuning options we have
#' based on the metric of selection. It will also only return the table with the selected metrics.
#' It will also make the data wide.
#'
#' @param all_learners_measures_table all the learners' metric for mean and standard deviation
#' @param metric_f_best the metric by which we will tune the model (AUC, MSE, RMSE etc)
#'
#' @return
#' @export
#'
#' @examples
metric_mean_sd_combine_best_spec_wide <- function(all_learners_measures_table, metric_f_best){
  
  # Returning the best tuned learners
  learners_best <- metric_mean_sd_combine_best_spec(all_learners_measures_table, metric_f_best)
  
  # Restricting learner's columns without the learner's name for tighter wide format table
  learners_best <- learners_best[,c("ML", "predSet", metric_f_best)]
  
  # Spread the best tuned learners in wide format
  learners_best_wide <- spread(learners_best, predSet, mean_auc_roc)
  
  return(learners_best_wide)
}

#' This function takes the best tuned learners & pull out the CV results for those tuned learners
#'
#' @param all_learners_measures_table data frame containing all the crossfold results for all learners
#' @param metric_f_best the mean metric by which we will tune the model (AUC_ROC, MSE, RMSE etc)
#'
#' @return
#' @export
#'
#' @examples
metric_cv_best <- function(all_learners_measures_table, metric_f_best){
  
  # setting all the columns we want for the final joined table
  all_columns <- colnames(all_learners_measures_table)
  
  # pulling out the best leanrers
  learners_best <- metric_mean_sd_combine_best(all_learners_measures_table,metric_f_best)
  
  # Joining tuned best learners models based on ML algo, Predictor Set and Tuning Set
  cv_best <- all_learners_measures_table %>% inner_join(learners_best, by = c("ML", "predSet", "tuningSet"))
  
  # Grap only the table on CV values from the left handside table
  cv_best <- cv_best[, 1:length(all_columns)]
  
  # Rename the column values to the original one after the joins created .x and .y values
  names(cv_best) <- all_columns
  
  cv_best <- cv_best %>% arrange(ML)
  
  return(cv_best)
}


#' This function takes in all the learners' values pull out the CV results for those tuned learners
#' filtered by selected the mean of selected metrics. Further, it also pulls out the particular metric
#' we would like to see such as AUC_ROC, RMSE etc.
#'
#' @param all_learners_measures_table data frame containing all the crossfold results for all learners
#' @param metric_f_best the mean metric by which we will tune the model (AUC_ROC, MSE, RMSE etc)
#' @param metric_select the particular metric we want to select
#'
#' @return
#' @export
#'
#' @examples
metric_cv_best_specific <- function(all_learners_measures_table, metric_f_best, metric_select){
  
  # all crossfold results
  cv_best <- metric_cv_best(all_learners_measures_table, metric_f_best)
  
  # specific metric crossfold results
  cv_best <- cv_best[,c("ML", "predSet", metric_select)]
  cv_best <- cv_best %>% arrange(ML)
  
  return(cv_best)
}

#' Widening model results
#'
#' This function takes in model results out of Tidy Model function in long format and convert it to
#' wide format.
#'
#' @param model_results model results data frame in long format
#'
#' @return
#' @export
#'
#' @author Zarni Htet, CDI
#' @examples
model_results_wide <- function(model_results){
  
  # Rename the name of the result column table
  names(model_results) <- c("metric", "est", "mean", "total_folds",
                            "st_err", "learner", "predSet", "ml", "tuningSet")
  
  # Rearrange the result column names
  model_results <- model_results %>%
    dplyr::select(learner, metric, mean,
                  st_err, total_folds, predSet, ml, tuningSet)
  
  # Round the results to 3 decimal places
  model_results$mean <- round(model_results$mean,3)
  model_results$st_err <- round(model_results$st_err,3)
  
  # Widening the results data frame
  model_results <- model_results %>%
    pivot_wider(names_from = metric, values_from = c(mean, st_err))
  
  # Rearranging the model results data frame after widening
  model_results <- model_results %>%
    dplyr::select(learner,
                  mean_roc_auc, st_err_roc_auc,
                  mean_pr_auc, st_err_pr_auc,
                  mean_accuracy, st_err_accuracy,
                  mean_f_meas, st_err_f_meas,
                  total_folds,predSet, ml, tuningSet)
  
  return(model_results)
} # model_results_wide


#' Best Learner by metric
#'
#' This function pulls out the best learner by metric of choice and machine learning algorithms
#' we have prespecified for each predictor set.
#'
#' @param AllLearnersSumMetricsTable Mean metric results across all learners
#' @param Metric mean metric results of interest.
#'
#' @return
#' @export
#'
#' @examples
best_learner_by_metric_predset_ml <- function(AllLearnersSumMetricsTable, Metric){
  
  metric <- paste0("mean_", Metric)
  sd <- paste0("sd_", Metric)
  
  best_learner_by_metric_ML <-
    AllLearnersSumMetricsTable %>%
    dplyr::group_by(ML, predSet) %>%
    dplyr::arrange(desc(paste0(metric))) %>%
    dplyr::slice(1:1) %>%
    dplyr::ungroup() %>%
    dplyr::select(learnerName, all_of(metric), all_of(sd), ML, predSet, tuningSet)
  head(best_learner_by_metric_ML, n = nrow(best_learner_by_metric_ML))
  return(best_learner_by_metric_ML)
  
}

pick_threshold <- function(
    learner_metrics_thres, learnerNameSelect,
    metricName, metricNumber, below
)
{
  learnerRows <- learner_metrics_thres %>%
    dplyr::filter(learnerName == learnerNameSelect)
  
  if(below)
  {
    candidates <- learnerRows[learnerRows[,metricName] < metricNumber,]
    thres <- candidates$threshold[which.max(candidates[,metricName])]
  } else
  {
    candidates <- learnerRows[learnerRows[,metricName] > metricNumber,]
    thres <- candidates$threshold[which.min(candidates[,metricName])]
  }
  
  return(thres)
}

# filter out learner results to just a subset of learners
filter_results <- function(learnersResults, learnerNamesFocus)
{
  # first make a copy
  learnersResultsFocus <- learnersResults
  
  # filter out to just relevant learners
  learnersResultsFocus$perfResults <- learnersResultsFocus$perfResults %>%
    filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$predProbs <- learnersResultsFocus$predProbs %>%
    filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$rocResults <- learnersResultsFocus$rocResults %>%
    filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$prResults <- learnersResultsFocus$prResults %>%
    filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$learnersMetrics <- learnersResultsFocus$learnersMetrics %>%
    filter(learnerName %in% learnerNamesFocus)
  
  # add in additional info
  learnersResultsFocus$learnerSpec <- learnerSpec
  learnersResultsFocus$learnerNames <- learnerNamesFocus
  
  return(learnersResultsFocus)
}
