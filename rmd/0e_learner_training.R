##########################################################################
# Title         : Model Training Wrapper Function
# Authors       : Kristin Porter, Kristen Hunter & Zarni Htet
# Collaborators : Brit Henderson & Dakota Denison
#########################################################################

# Sourcing helper functions
source(here::here("R", "0c_learner_training_helpers.R"))
source(here::here("R", "0d_helpers.R"))
source(here::here("R", "0f_performance_metrics.R"))
source(here::here("R", "0g_plots.R"))

#' This function takes in a model specification,
#' training data, and test data, and returns predictions on the test data.
#'
#' @param trainDat_pred data frame of predictors for training set
#' @param trainDat_outcome data frame of outcome column for training set
#' @param trainDat_other data frame of other columns, including id columns, etc
#' @param testDat_pred data frame of all variables for test set
#' @param testDat_outcome data frame of outcome column for test set
#' @param testDat_other data frame of other columns, including id columns
#' @param model_spec model specification: model type, tuning parameters, etc.

fit_model <- function(
    trainDat_pred, trainDat_outcome, trainDat_other = NULL,
    testDat_pred = NULL, testDat_outcome = NULL, testDat_other = NULL,
    model_spec
)
{
  # fitting the outcome and predictors for training data
  model_train <-
    model_spec %>%
    parsnip::fit_xy(
      x = trainDat_pred,
      y = trainDat_outcome[,1] %>% factor()
    )
  
  # predicted probabilities on the train set
  trainResults <- predict(
    model_train, new_data = trainDat_pred, type = "prob"
  )
  trainResults <- as.data.frame(trainResults)
  colnames(trainResults) <- c("pred_0", "pred_1")
  
  if(!is.null(trainDat_other))
  {
    trainResults <- dplyr::bind_cols(trainDat_other,
                                     trainDat_pred,
                                     trainDat_outcome,
                                     trainResults)
  }
  
  if(!is.null(testDat_pred))
  {
    # predicted probabilities on the test set
    testResults <- predict(
      model_train, new_data = testDat_pred, type = "prob"
    )
    testResults <- as.data.frame(testResults)
    colnames(testResults) <- c("pred_0", "pred_1")
    # combine predicted probabilities with predictors and outcome
    testResults <- dplyr::bind_cols(testDat_other,
                                    testDat_pred,
                                    testDat_outcome,
                                    testResults)
  } else
  {
    testResults <- NULL
  }
  
  return(list(model_train = model_train,
              trainResults = trainResults,
              testResults = testResults))
  
}

get_model_spec <- function(ML, tuningFrame, t)
  
{
  # for generalized linear model
  if (ML == "glm"){
    
    # setting glm Model Specifications
    model_spec <-
      parsnip::logistic_reg() %>%
      parsnip::set_engine("glm") %>%
      parsnip::set_mode("classification") %>%
      # glm tuning parameter for the t-th tuning specification
      parsnip::set_args(penalty = tuningFrame$penalty[t])
    
  } else if (ML == "lasso"){
    
    model_spec <-
      parsnip::logistic_reg() %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(penalty = tuningFrame$penalty[t],
                        mixture = tuningFrame$mixture[t])
    
  } else if (ML == "random_forest"){
    
    model_spec <-
      parsnip::rand_forest() %>%
      parsnip::set_engine("ranger") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(mtry = tuningFrame$m_tries[t],
                        trees = tuningFrame$ntrees_opt[t],
                        min_n = tuningFrame$node_size[t],
                        importance = "permutation")
    
  } else if (ML == "naive_bayes"){
    
    model_spec <-
      discrim::naive_Bayes() %>%
      parsnip::set_engine("naivebayes") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(smoothness = tuningFrame$smoothness[t],
                        Laplace = tuningFrame$Laplace[t])
    
  } else if (ML == "xgboost"){
    
    model_spec <-
      parsnip::boost_tree() %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(tree_depth = tuningFrame$tree_depth[t],
                        trees = tuningFrame$trees[t],
                        learn_rate = tuningFrame$learn_rate[t])
    
  } else if (ML == "svm_poly"){
    
    model_spec <-
      parsnip::svm_poly() %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(cost = tuningFrame$cost[t],
                        degree = tuningFrame$degree[t],
                        scale_factor = tuningFrame$scale_factor[t],
                        margin = tuningFrame$margin[t])
    
  } else if (ML == "svm_rbf"){
    
    model_spec <-
      parsnip::svm_rbf() %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(cost = tuningFrame$cost[t],
                        degree = tuningFrame$degree[t],
                        scale_factor = tuningFrame$scale_factor[t],
                        margin = tuningFrame$margin[t])
    
  } else
  {
    print(paste0("Invalid ML:", ML))
  }
  
  return(model_spec)
}
#' This function takes in a model specification object saved from a
#' previous step 01_03_Specifying Learners for Targeting Objective R Notebook
#' and returns an R object with all the performance metrics estimated
#'
#' @param learnerSpec the model is an R object that contains the
#' path to the data set, the name of the training dataset,
#' the name of the meta data set, the labels of the learning/target objective,
#' the outcome variable, the time point of interest (To be utilized),
#' the population of interest (To be implemented), the predictor sets,
#' the corresponding learner/model sets and the corresponding tuning parameters.
#'
train_learners <- function(learnerSpec){
  
  # 1st Step: Check whether we are using the right R version
  # If the R Version is not 4.0.0, then the ML packages would
  # not necessarily work and are not tested.
  if(R.Version()$major != "4" | R.Version()$minor != "0.0"){
    
    print("This platform requires R version 4.0.0. Please switch to that version.")
    stop()
    
  } # R version check
  
  # 2nd Step: Load the training and metadata using the path and dataframe names from the object.
  wholeTrainDat <- get(base::load(here::here(
    learnerSpec$dataInputPath, learnerSpec$trainingDataName
  )))
  
  # predictor sets
  allPredsets <- learnerSpec$allPredsets
  
  # the seed for the model object. This is used to set the cross folds in place
  seed <- learnerSpec$folds$seed
  
  # the number of folds for cross validation
  # this could be a numeric number of a column of character/factor values
  # if it's a column of character/factor values, this would kick in group K folds
  nfolds <- learnerSpec$folds$nfolds
  
  # stratification for the folds in the data set
  # Usually we would stratify the outcome variable in an imbalanced class scenario
  strat <- learnerSpec$folds$strat
  
  # creating the specified fold (note the data frame gets sorted by fold number.
  # Fold numbers are created and saved onto the data frame by the function.)
  # for more on the function, please read strat_fold and fold_create
  # function documentation in models_results_helper_function.R
  wholeTrainDat <- strat_fold(data = wholeTrainDat,
                              nfolds = nfolds,
                              strat = strat,
                              seed = seed)
  
  
  print((wholeTrainDat %>%
           group_by(across(c('folds', learnerSpec$outcomeName))) %>% tally()))
  
  # 4th Step:
  # Each fold's performance metrics results will be saved in a data frame first.
  # When all the folds of the combination of a predictor set, a ML algo and
  # tuning parameter is done, that data frame will be stored onto the model_results() list object.
  # The list R object will have the length that is equal to the number of predictor
  # set times the the number ML algos times the number of tuning parameters times
  # the number of cross fold repeats.
  
  # List object to store the data frame of a complete cross validated results of the
  # combination of predictor set, ML algorithm and tuning parameter.
  model_results <- NULL
  
  # List object to store the data frame of predictive probabilities for each of the fold
  training_predProbs <- NULL
  
  # save out ROC curve and PR curve information
  roc_data_all <- NULL
  pr_data_all <- NULL
  
  # parsing out the specified outcome
  outcomeName <- learnerSpec$outcomeName
  
  # looping through predictor sets
  for (i in 1:length(allPredsets)){
    
    # name of predictor set
    predsetName <- names(allPredsets)[i]
    
    # parsing out the current predictor set
    predset <- allPredsets[[i]] # i is for the current predictor set the loop is in
    
    # unlist the ML algorithm names for the ith predictor set to make it
    # flat and be able to count the number of item
    # all.learners is a named list of predictorsetA = c("glm", "random_forest")
    # Please refer to 01_03 specifications if you are confused.
    algorithmList <- unlist(learnerSpec$specifiedLearners[predsetName])
    
    # looping through machine learning algorithms
    for (j in 1:length(algorithmList)){
      
      # parsing out each machine learning algorithm for each of the predictor set
      ML <- algorithmList[[j]] # j is for the ML algo in consideration for the current predictor set
      
      # parsing out the tuning parameters of the specified machine learning algorithm
      tuningFrame <- learnerSpec$tuningSets[[ML]]
      
      # for each of the tuning parameter of the specified machine learning algorithm
      for (t in 1:dim(tuningFrame)[1]){
        
        # learner's label for the particular predictor set, machine learning algorithm and tuning set
        learnerName <- paste(ML, predsetName, t, sep = "_")
        
        # printing the current learner label
        print(paste("Training learner:", learnerName))
        
        # data frame to store the results of all the folds
        fold_results <- NULL
        
        # for each of the fold
        for(k in 1:nfolds){
          
          # make sure outcome is factor
          wholeTrainDat[,outcomeName] <- as.factor(wholeTrainDat[,outcomeName])
          
          # segment the data by fold using the which() function
          
          # getting the data frame indexes of the kth fold
          testInd <- which(wholeTrainDat$folds == k, arr.ind = TRUE)
          
          # the kth fold is pulled out as the test data frame
          testDat <- wholeTrainDat[testInd, ]
          
          # the rest of the data frame is set as the training data frame
          trainDat <- wholeTrainDat[-testInd, ]
          
          # save out useful data subsets
          trainDat_pred <- trainDat %>%
            dplyr::select(dplyr::all_of(predset))
          trainDat_outcome <- trainDat %>%
            dplyr::select(dplyr::all_of(outcomeName))
          # id variables, excluded predictors, etc.
          trainDat_other <- trainDat %>%
            dplyr::select(
              !c(dplyr::all_of(predset), dplyr::all_of(outcomeName),
                 rowIndex, folds)
            )
          
          testDat_pred <- testDat %>%
            dplyr::select(dplyr::all_of(predset))
          testDat_outcome <- testDat %>%
            dplyr::select(dplyr::all_of(outcomeName))
          testDat_other <- testDat %>%
            dplyr::select(
              !c(dplyr::all_of(predset), dplyr::all_of(outcomeName),
                 rowIndex, folds)
            )
          
          # convert all strings to factors for predsets
          trainDat_pred <- trainDat_pred %>%
            mutate_if(sapply(trainDat_pred, is.character), as.factor)
          testDat_pred <- testDat_pred %>%
            mutate_if(sapply(testDat_pred, is.character), as.factor)
          
          
          # fit model
          model_spec   <- get_model_spec(ML, tuningFrame, t)
          model_output <- fit_model(trainDat_pred    = trainDat_pred,
                                    trainDat_outcome = trainDat_outcome,
                                    trainDat_other   = trainDat_other,
                                    testDat_pred     = testDat_pred,
                                    testDat_outcome  = testDat_outcome,
                                    testDat_other    = testDat_other,
                                    model_spec       = model_spec)
          testResults <- model_output$testResults
          
          
          # Save the performance metrics of interest into a data frame
          fold_results_k <- calc_metrics(
            testResults = testResults,
            outcomeName = outcomeName
          )
          
          fold_results_k$fold        <- testResults$fold        <- k
          fold_results_k$ML          <- testResults$ML          <- ML
          fold_results_k$predSet     <- testResults$predSet     <- predsetName
          fold_results_k$tuningSet   <- testResults$tuningSet   <- as.numeric(t)
          fold_results_k$learnerName <- testResults$learnerName <- learnerName
          
          # save out ROC and PR data
          # ground truth in factor form
          truth.factor <- as.factor(as.character(testResults[[learnerSpec$outcomeName]]))
          # in case there is only one observed level
          levels(truth.factor) <- c("0", "1")
          # set the positive class to be the reference level (necessary for metrics below)
          truth.factor <- relevel(truth.factor, ref = "1")
          
          roc_data <- yardstick::roc_curve(
            data = testResults, truth = truth.factor, pred_1
          )
          pr_data <- yardstick::pr_curve(
            data = testResults, truth = truth.factor, pred_1
          )
          
          # save out relevant info for objects
          roc_data$ML          <- pr_data$ML          <- ML
          roc_data$predSet     <- pr_data$predSet     <- predsetName
          roc_data$tuningSet   <- pr_data$tuningSet   <- as.numeric(t)
          roc_data$fold        <- pr_data$fold        <- k
          roc_data$learnerName <- pr_data$learnerName <- learnerName
          
          # bind everything together
          roc_data_all <- dplyr::bind_rows(roc_data_all, roc_data)
          pr_data_all <- dplyr::bind_rows(pr_data_all, pr_data)
          training_predProbs <- dplyr::bind_rows(training_predProbs, testResults)
          fold_results <- dplyr::bind_rows(fold_results, fold_results_k)
          
        } # end of for loop for the k-fold cross validation
        
        model_results <- dplyr::bind_rows(model_results, fold_results)
        
      } # Loop for tuning parameter rows
    } # Loop for learners
  } # Loop for predictor sets
  
  return(list(perfResults = model_results,
              predProbs = training_predProbs,
              rocResults = roc_data_all,
              prResults = pr_data_all))
} # model wrapper function

fit_train_learner <- function(learnerSpec, learnerNameSelect)
{
  
  # learner attributes
  learnerAttributes <- learnerNameSplits(selectedLearners = learnerNameSelect)
  ML <- learnerAttributes$mlSet[[1]]
  t <- as.numeric(learnerAttributes$tuningSet[[1]])
  predsetName <- learnerAttributes$predSet[[1]]
  predset <- learnerSpec$allPredsets[[predsetName]]
  
  # load training and testing data
  wholeTrainDat <- get(base::load(
    here::here(learnerSpec$dataInputPath,
               learnerSpec$trainingDataName
    )))
  
  # save out useful data subsets
  wholeTrainDat_pred <- wholeTrainDat %>%
    dplyr::select(dplyr::all_of(predset))
  wholeTrainDat_outcome <- wholeTrainDat %>%
    dplyr::select(dplyr::all_of(learnerSpec$outcomeName))
  # id variables, excluded predictors, etc.
  wholeTrainDat_other <- wholeTrainDat %>%
    dplyr::select(
      !c(dplyr::all_of(predset), dplyr::all_of(learnerSpec$outcomeName))
    )
  
  # get model spec
  tuningFrame <- learnerSpec$tuningSets[[ML]]
  model_spec <- get_model_spec(ML, tuningFrame, t)
  
  # fit model
  model_output <- fit_model(trainDat_pred    = wholeTrainDat_pred,
                            trainDat_outcome = wholeTrainDat_outcome,
                            trainDat_other   = wholeTrainDat_other,
                            testDat_pred     = NULL,
                            testDat_outcome  = NULL,
                            testDat_other    = NULL,
                            model_spec       = model_spec)
  
  return(model_output$model_train)
}


#' Function takes in the predictive probabilities and thresholds
#' The function returns the range of performance metrics available to the PATool framework user
#'
#' The metrics are listed as below -
#'
#'
#'
#' @param testResults model results on "test" data
#          (can be held-out validation set or held-out CV fold)
#' @param outcomeName outcome of the model results
#'
#' @return
#' @export
#'
#' @examples

calc_metrics <- function(testResults,
                         outcomeName)
{
  
  truth.factor  <- as.factor(as.character(testResults[[outcomeName]]))
  # set the positive class to be the reference level (necessary for metrics below)
  levels(truth.factor) <- c("0", "1")
  truth.factor  <- relevel(truth.factor, ref = "1")
  truth.numeric <- as.numeric(as.character(testResults[[outcomeName]]))
  predictions <- testResults$pred_1
  
  # check
  if(max(truth.numeric) > 1 | min(truth.numeric) < 0)
  {
    stop('Error in coding variable.')
  }
  if(min(predictions) < 0 | max(predictions) > 1)
  {
    stop('Error in coding variable.')
  }
  
  auc_roc  <- yardstick::roc_auc_vec(truth = truth.factor, estimate = predictions)
  auc_pr   <- yardstick::pr_auc_vec(truth = truth.factor, estimate = predictions)
  log_loss <- yardstick::mn_log_loss_vec(truth = truth.factor, estimate = predictions)
  rmse     <- yardstick::rmse_vec(truth = truth.numeric, estimate = predictions)
  mse      <- rmse^2
  
  metric_r2 <- metric_set(rsq)
  metric_r2_results <- metric_r2(testResults,
                                 truth = truth.numeric,
                                 estimate = pred_1)
  r2 <- metric_r2_results$.estimate
  
  # for now don't calculate threshold-specific metrics
  # # Accuracy
  # accur <- calc_accuracy_man(outcomes = truth.factor,
  #                            predictions = predictions,
  #                            threshold = thres)
  #
  # # True Positive Rate
  # tpr <- calc_tpr(predictions = predictions,
  #                 outcomes = truth.factor,
  #                 threshold = thres)
  #
  # # True Positive Count
  # tpc <- calc_tp_man(predictions = predictions,
  #                    outcomes = truth.factor,
  #                    threshold = thres)
  #
  # # True Negative Rate
  # tnr <- calc_tnr(predictions = predictions,
  #                 outcomes = truth.factor,
  #                 threshold = thres)
  #
  # # True Negative Count
  # tnc <- calc_tn_man(predictions = predictions,
  #                    outcomes = truth.factor,
  #                    threshold = thres)
  #
  # # False Positive Rate
  # fpr <- calc_fpr(predictions = predictions,
  #                 outcomes = truth.factor,
  #                 threshold = thres)
  #
  # # False Positive  Count
  # fpc <- calc_fp_man(predictions = predictions,
  #                    outcomes = truth.factor,
  #                    threshold = thres)
  #
  # # False Negative Rate
  # fnr <- calc_fnr(predictions = predictions,
  #                 outcomes = truth.factor,
  #                 threshold = thres)
  #
  # # False Negative Count
  # fnc <- calc_fn_man(predictions = predictions,
  #                    outcomes = truth.factor,
  #                    threshold = thres)
  #
  # # Precision
  # precision <- calc_precision(
  #   predictions = predictions,
  #   outcomes = truth.factor,
  #   threshold = thres)
  #
  # # manual f1_ estimation
  # f1_man <- (tpc/(tpc + (fpc + fnc)/2))
  #
  # # manual f05_ estimation
  # f05_man <- ((1+ 0.5^2) * tpc)/((1 + 0.5^2) * tpc + (0.5^2 * fnc) + fpc)
  
  # # Min Predictive Probabilities for each fold
  # min_pred <- min(predictions, na.rm = TRUE)
  #
  # # Mean Predictive Probabilities for each fold
  # mean_pred <- mean(predictions, na.rm = TRUE)
  #
  # # Max Predictive Probabilities for each fold
  # max_pred <- max(predictions, na.rm = TRUE)
  #
  # # 1st quartile Predictive Probabilities for each fold
  # first_Q_pred <- summary(predictions, na.rm = TRUE)[[2]]
  #
  # # median Predictive Probabilities for each fold
  # median_pred <- summary(predictions, na.rm = TRUE)[[3]]
  #
  # # 3rd quartile Predictive Probabilities for each fold
  # third_Q_pred <- summary(predictions, na.rm = TRUE)[[5]]
  
  results <- data.frame(
    AUC_ROC = auc_roc,
    AUC_PR = auc_pr,
    MSE = mse,
    RMSE = rmse,
    logloss = log_loss,
    r2 = r2
  )
  
  return(results)
}