# categorize predictors into numeric and categorical
categorizePredictors <- function(dat, predictors)
{
  numValues <- apply(dat[,predictors], 2, function(x){ length(unique(x)) })
  catPredictors <- predictors[numValues <= 10]
  numericPredictors <- predictors[numValues > 10]
  
  return(list(catPredictors = catPredictors, numericPredictors = numericPredictors))
}

# plot histograms of all predictors
genPredictorHistograms <- function(dat, allPredictors)
{
  # categorize into categorical or numeric predictors
  predictorTypes <- categorizePredictors(dat = trainingDat, predictors = allPredictors)
  numericPredictors <- predictorTypes$numericPredictors
  
  # plot histograms of numeric predictors
  predictorPlots <- NULL
  for(i in 1:length(numericPredictors))
  {
    predictor <- numericPredictors[i]
    
    # histogram plot
    p <- ggplot(trainingDat) +
      aes_string(predictor) +
      geom_histogram()
    
    predictorPlots[[i]] <- p
  }
  
  return(predictorPlots)
}

#' Helper function to split the incoming learners
#' into machine learning, predictor set and tuning sets
#'
#' @param selectedLearners enter selected Learners saved from the previous 02_01_Learner_Training Rmd
#'
#' @return
#' @export
#'
#' @examples

learnerNameSplits <- function(selectedLearners)
{
  
  # setting up null vectors to store the split ml,
  # predSet and tuningSet
  
  mlSet <- NULL
  predSet <- NULL
  tuningSet <- NULL
  
  for (i in 1:length(selectedLearners))
  {
    learnerSplit <- unlist(str_split(selectedLearners[i], "_predset_"))
    predSplit <- unlist(str_split(learnerSplit[2], "_"))
    
    mlSet[i] <- learnerSplit[1]
    predSet[i] <- paste0("predset_", predSplit[1])
    tuningSet[i] <- predSplit[2]
  }
  
  return(list(mlSet = mlSet,
              predSet = predSet,
              tuningSet = tuningSet))
} # learnerNameSplits


# generate predictor plots by prediction category
generatePredPlots <- function(trainResults, predictors)
{
  predPlots <- NULL
  
  for(i in 1:length(predictors))
  {
    predictor <- predictors[i]
    
    # base plot
    p <- ggplot(trainResults) +
      aes_string(predictor)
    
    # try to determine if categorical
    if(length(unique(trainResults[[predictor]])) <= 10)
    {
      trainResults[[predictor]] <- as.factor(as.character(trainResults[[predictor]]))
      p <- p + geom_bar(aes(fill = predCat), position = position_dodge())
    } else
    {
      p <- p + geom_density(aes(color = predCat))
    }
    predPlots[[i]] <- p
  }
  
  return(predPlots)
}