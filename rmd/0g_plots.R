get_selectedMetrics <- function(metricType)
{
  if(metricType == "count")
  {
    selectedMetrics = c("tnc", "fnc", "fpc", "tpc")
  } else if(metricType == "rate")
  {
    selectedMetrics = c("tnr", "fnr", "fpr", "tpr")
  } else if(metricType == "fdr")
  {
    selectedMetrics = "fdr"
  }
  return(selectedMetrics)
}

# MDRC Color Palette
mdrc_colors <- data.frame(a = c("#cae3eb", "#F9DABC"    , "#D9C89E"    , "#DAE6D6"    , "#e6e7e7", NA_character_),
                          b = c("#63b6cf", "#EFA967"    , "#A89968"    , "#A1BD7A"    , "#b1b3b5", NA_character_),
                          c = c("#00558C", "#D57800"    , "#816040"    , "#538549"    , "#808284", "#B7312C"    ),
                          d = c("#002B49", NA_character_, NA_character_, NA_character_, "#000000", NA_character_),
                          row.names = c("blue", "orange", "brown", "green", "grey", "red"))

# MDRC Theme
theme_mdrc <- function(base_size = 9, base_family= "ArialMT") {
  
  # All text should appear in Arial 9-point font, except axis labels and titles.
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          # No legend - use labels directly in graph
          # legend.position = "none",
          
          # Show only x and y borders
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          
          # All labels should be horizontal
          axis.title.y = element_text(angle = 360),
          
          # Making axis values darker
          axis.text = element_text(colour = "black", size = 9),
          
          # Center and bold titles should (Arial Bold 11-point)
          plot.title = element_text(face = "bold",
                                    hjust = 0.5,
                                    size = 11,
                                    margin= ggplot2::margin(0,0,1,0, unit = "line")),
          
          # Left-align source/notes/footnotes
          plot.caption = element_text(hjust= 0,
                                      margin= ggplot2::margin(1,0,0,0, unit = "line")),
          # Additional formatting added
          # Remove tickmarks
          axis.ticks.y =element_blank(),
          axis.ticks.x =element_blank(),
          
          # Remove background color for panel labels created by facet_*
          strip.background = element_blank(),
          
          # Make panel label horizontal
          strip.text = element_text(size = 9),
          strip.text.y.right = element_text(angle = 0),
          strip.text.y.left = element_text(angle = 0),
          
          # Make panel label outside of the axis line (outside/inside)
          strip.placement = "outside"
          
    )
}

#' Wrapper to produce the mdrc themed graph
#' appropriating from RTU
#'
#' @param plotData prepared Long Data with confusion matrix rates or counts along with learner name and thresholds
#' @param outcomeDescription predictive negative and positive descriptions
#' @param metricType to plot a count or a rate graph
#' @param graphTitle title of a graph
#'
#' @return
#' @export
#'
#' @examples
conf_mat_bar_plot <- function(
    plotData, equityVar = NULL, metricType, outcomeDescription, graphTitle
)
{
  if(metricType == "count")
  {
    metrics_labels = c(
      "tnc" = "True \nNegative",
      "fnc" = "False\nNegative",
      "fpc" = "False\nPositive",
      "tpc" = "True \nPositive"
    )
    axis.title.y <- "Count"
  } else if(metricType == "rate")
  {
    metrics_labels = c(
      "tnr" = "True \nNegative",
      "fnr" = "False\nNegative",
      "fpr" = "False\nPositive",
      "tpr" = "True \nPositive"
    )
    axis.title.y <- "Rate"
  } else if(metricType == "fdr")
  {
    metrics_labels = c(
      "fdr" = "False\nDiscovery"
    )
    axis.title.y <- "False Discovery Rate"
  }
  
  if(metricType %in% c("count", "rate"))
  {
    # data preparation for plotting
    plotData <- plotData %>%
      dplyr::mutate(
        metricType_fct = ordered(metricType,
                                 levels = names(metrics_labels),
                                 labels = metrics_labels),
        predict_cat4 = ordered(metricType,
                               levels = names(metrics_labels),
                               labels = c(outcomeDescription[1],
                                          " ",
                                          outcomeDescription[2],
                                          "  ")))
    
  }
  
  # setting the mdrc theme
  pal <- c(mdrc_colors["blue",3],
           mdrc_colors["blue",1],
           mdrc_colors["orange",1],
           mdrc_colors["orange",3])
  
  if(is.null(equityVar))
  {
    learnerNames <- unique(plotData$learnerName)
    p <- plotData %>%
      ggplot2::ggplot(aes(x = learnerName, y = value, fill = learnerName)) +
      ggplot2::scale_fill_manual("Learner", values = pal, labels = learnerNames)
  } else
  {
    # ensure it is a factor
    plotData[[equityVar]] <- as.factor(as.character(plotData[[equityVar]]))
    equityVarCats <- unique(plotData[[equityVar]])
    
    p <- plotData %>%
      ggplot2::ggplot( aes(x = .data[[equityVar]], y = value, fill = .data[[equityVar]])) +
      ggplot2::scale_fill_manual("Category", values = pal, labels = equityVarCats)
  }
  
  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    # Extend the y/x axis by a multiplier
    scale_y_continuous(position = "right") +
    # Prevent ggplot from cutting off text
    ggplot2::coord_cartesian(clip = "off") +
    # Label around text
    labs(title = graphTitle, x = "Threshold", y = axis.title.y) +
    theme_mdrc() +
    # Additional modifications specific this plot
    theme(
      axis.text.x = element_blank(),
      # Adjusting spacing between panels
      panel.spacing.y = unit(2, "lines"),
      panel.spacing.x = unit(.5, "lines"),
      # Adjusting spacing around title text
      plot.title = element_text(margin= ggplot2::margin(0,0,3,0, unit = "line")),
      strip.text.y = element_text(margin= ggplot2::margin(0,1,0,0, unit = "line"))
    )
  
  if(metricType %in% c("rate", "count"))
  {
    p <- p +
      ggplot2::facet_grid(predict_cat4 + metricType_fct  ~  threshold  ,
                          # Move strip label from top to bottom ("x"), right to left ("y")
                          # Or do for both ("both")
                          switch = "both",
                          scales = "fixed",
                          space = "fixed")
  } else
  {
    p <- p +
      ggplot2::facet_grid(~  threshold  ,
                          # Move strip label from top to bottom ("x"), right to left ("y")
                          # Or do for both ("both")
                          switch = "both",
                          scales = "fixed",
                          space = "fixed")
  }
  
  return(p)
}


#' Making a dot plot of a specified metric of all the different leaners
#'
#' @param results two column data frame: 1st column is all the learners we are considering & the second is
#' the performance metrics in consideration
#'
#' @return
#' @export
#'
#' @examples
helper_dotplot <- function(results){
  
  # Parsing out the column names
  colnames <- names(results)
  
  # ggplot
  metric_dotplot <- ggplot(results, aes_string(x = colnames[1],
                                               y = colnames[2])) +
    geom_dotplot(binaxis = "y",
                 dotsize = .5) +
    stat_summary(fun.y = mean,
                 geom = "point",
                 shape = 18,
                 size = 4,
                 color = "red") +
    coord_flip() +
    ggtitle(paste0(colnames[2]," distribution of each learner"))
  
  return(metric_dotplot)
  
}

#' Making a point plot of a specified metrics of all the different learners
#'
#' @param results two column data frame: 1st column is all the learners we are considering & the second is
#' the performance metrics in consideration
#'
#' @return
#' @export
#'
#' @examples
helper_pointplot <- function(results, xscale = TRUE){
  
  # Parsing out the column names
  colnames <- names(results)
  
  # geom point plot with mean values
  metric_pointplot <- ggplot(results, aes_string(x = colnames[1],
                                                 y = colnames[2])) +
    geom_point(aes(color = 'a')) +
    stat_summary(fun.y = mean,
                 geom = "point",
                 shape = 18,
                 size = 3,
                 aes(color = 'b')) +
    coord_flip() +
    scale_color_manual(name = "Measures",
                       values = c('a' = "black", 'b' = "red"),
                       labels = c('cv values', 'mean of cv values')) +
    ggtitle(paste0(colnames[2], " by cross-validation fold and mean across all folds")) +
    theme(legend.position = "bottom")
  
  # if we set xscale to be TRUE, we will limit the probabilities on the xscale to be btw 0 and 1
  if (xscale == TRUE){
    metric_pointplot <- metric_pointplot +
      scale_y_continuous(limits = c(0,1)) # y because the x scale on the graph is actually a flipped y
  } # end if
  
  return(metric_pointplot)
}

#' Point plot comparison of metric results of learners
#'
#' In this graphic, we are plotting each cv results of a particular metric that we want to examine for
#' each of the learners as well as the mean of said cv results.
#'
#' @param list A list of data frames with performance metrics of each learner for each fold
#' @param col the specific metrics we want to return (optional)
#' @param xscale if TRUE (the default), xscale would be within 0 and 1. Otherwise, ggplot will set it to where all the points scatter
#'
#' @return
#' @export
#'
#' @examples
metric_pointplot <- function(list, col, xscale = TRUE){
  
  # Getting the specific metric results out for each of the learner
  metric_results <- comb_listdata_column(list, col)
  
  # Getting the visualization for the metric plot out
  metric_graph <- helper_pointplot(metric_results, xscale = xscale)
  
  return(metric_graph)
  
}


#' This function takes in two data frames: the cross validaton fold values of a particular metric
#' and the summarized value of a particular metric
#'
#' @param all_learners_measures_table data frame containing all the crossfold results for all learners
#' @param tuner the mean metric by which we will tune the learners (AUC, MSE, RMSE etc)
#' @param metrics the particular metric we want to select
#'
#' @return
#' @export
#'
#' @examples
metric_multi_graph <- function(all_learners_measures_table, tuner, metrics){
  
  #best_cv_learners
  best_cv_learners <- metric_cv_best_specific(all_learners_measures_table, tuner, metrics)
  
  #best_sum_learners
  best_sum_learners <- metric_mean_sd_combine_best_spec(all_learners_measures_table, tuner)
  
  # getting the size of the loop based on the number of tuned learners we have
  loop_size <- nrow(best_sum_learners)
  
  # Creating a list variable to save the graph objects
  graphs <- list()
  
  # Parsing out the column names
  
  for (i in 1:loop_size){
    
    # get the particular ML object and predictor set from the summary learners based
    # on loop iteration
    
    ML <- best_sum_learners$ML[i]
    predSet <- best_sum_learners$predSet[i]
    
    graph_data <- best_cv_learners[best_cv_learners$ML == ML & best_cv_learners$predSet == predSet,]
    
    # Parsing out the colnames from graph data
    colnames <- names(graph_data)
    
    graphs[[i]]  <- ggplot(graph_data, aes_string(x = colnames[1], y = colnames[3])) + geom_point(aes(color = 'a')) +
      stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, aes(color = 'b')) +
      coord_flip() +
      scale_color_manual(name = "Measures",
                         values = c('a' = "black", 'b' = "red"),
                         labels = c('cv values', 'mean of cv values')) +
      scale_y_continuous(limits = c(0,1)) + # y because the x scale on the graph is actually a flipped y
      ggtitle(paste0(predSet)) +
      theme(legend.position = "bottom")
  }
  
  return(graphs)
  
}

#' The function takes in all the cross fold results of all the learners, selects the best tuned model
#' by a particular metrics and further selects a metrics we want to compare across machine learning
#' algorithm and predictor sets in facet_grid ggplot format.
#'
#' @param all_learners_measures_table data frame containing all the crossfold results for all learners
#' @param tuner the mean metric by which we will tune the learners (AUC_ROC, MSE, RMSE etc)
#' @param metrics the particular metric we want to select
#' @param xscale between 0 and 1 if TRUE
#' @param predSet if x, Predictor sets are displayed as columns else, predictor sets are on the y-axis
#'
#' @return
#' @export
#'
#' @examples
metric_multi_facets <- function(all_learners_measures_table,
                                tuner, metric, predSet = 'x', xscale = TRUE){
  
  #best_cv_learners
  best_cv_learners <- metric_cv_best_specific(all_learners_measures_table, tuner, metric)
  
  # Parsing out the colnames from graph data
  colnames <- names(best_cv_learners)
  
  # Creating the facet_metrics graph
  
  # color version instead of facet wrap
  # if(predSet == 'x'){
  #   facet_metric_graph <- ggplot(
  #     best_cv_learners, aes_string(
  #       x = colnames[1],
  #       y = colnames[3],
  #       color = colnames[2])
  #     ) +
  #     geom_point(shape = 16) +
  #     stat_summary(fun = mean,
  #                  geom = "point",
  #                  shape = 18,
  #                  size = 3) +
  #     coord_flip() +
  #     scale_shape_manual(name = "Measures",
  #                        values = c(16, 18),
  #                        labels = c('cv values', 'mean of cv values')) +
  #     theme(axis.text.x = element_text(angle = 65,
  #                                      hjust =1)) +
  #     theme(legend.position = "bottom")
  # }
  
  if(predSet == 'x'){
    facet_metric_graph <- ggplot(best_cv_learners, aes_string(x = colnames[1],
                                                              y = colnames[3])) +
      geom_point(aes(color = 'a')) +
      stat_summary(fun = mean,
                   geom = "point",
                   shape = 18,
                   size = 3,
                   aes(color = 'b')) +
      coord_flip() +
      facet_grid(cols = vars(predSet)) +
      scale_color_manual(name = "Measures",
                         values = c('a' = "black", 'b' = "red"),
                         labels = c('cv values', 'mean of cv values')) +
      theme(axis.text.x = element_text(angle = 65,
                                       hjust =1)) +
      theme(legend.position = "bottom")
  } else {
    facet_metric_graph <- ggplot(best_cv_learners, aes_string(x=colnames[2], y = colnames[3])) +
      geom_point(aes(color = 'a')) +
      stat_summary(fun.y = mean,
                   geom = "point",
                   shape = 18,
                   size = 3,
                   aes(color = 'b')) +
      coord_flip() +
      facet_grid(cols = vars(ML)) +
      scale_color_manual(name = "Measures",
                         values = c('a' = "black", 'b' = "red"),
                         labels = c('cv values', 'mean of cv values')) +
      theme(axis.text.x = element_text(angle = 65, hjust =1)) +
      theme(legend.position = "bottom")
  }
  
  # if we set xscale to be TRUE, we will limit the probabilities on the xscale to be btw 0 and 1
  if (xscale == TRUE){
    facet_metric_graph <- facet_metric_graph +
      scale_y_continuous(limits = c(0,1)) # y because the x scale on the graph is actually a flipped y
  } # end if
  
  return(facet_metric_graph)
}

#' Predictive Probability Graph function
#'
#' This function will plot the predictive probabilities of the learner of choice when we specify
#' the ML, predSet, tuningSet and predProbs
#'
#' @param ML learner's ML
#' @param predSet learner's predictor set
#' @param tuningSet learner's tuning set
#' @param predProbs the dataset with predictive probabilities of all learners
#' @param splitCategory NULL or character; specify whether to split data by a certain category,
#' such as success/failure or a protected class like gender
#'
#' @return
#' @export
#'
#' @examples
pred_prob_graph <- function(
    predProbs,
    ML = NULL, predSet = NULL, tuningSet = NULL,
    learnerName = NULL,
    splitCategory = NULL)
{
  
  if(!is.null(learnerName) & (!is.null(ML) | !is.null(predSet) | !is.null(tuningSet)))
  {
    stop("Please provide either the learnerName or the combination of ML, predSet, and tuningSet")
  }
  if(is.null(learnerName))
  {
    if(is.null(ML) | is.null(predSet) | is.null(tuningSet))
    {
      stop("Please provide all: ML, predSet, tuningSet")
    }
    else
    {
      learnerName <- paste(ML, predSet, tuningSet, sep = "_")
    }
  }
  
  # Pulling the predictive probabilities of the learner of choice
  choice_learner <- predProbs %>%
    dplyr::filter(learnerName == !!learnerName)
  
  if(is.null(splitCategory)){
    
    # ggplot for the learner of choice
    choice_learner_graph <-
      ggplot(choice_learner, aes(pred_1)) +
      geom_histogram() +
      xlab ("Predictive probabilities") +
      ylab ("Count") +
      ggtitle(paste("Predictive probability distribution for\n", learnerName)) +
      scale_x_continuous(limits = c(0,1)) +
      theme (plot.title = element_text(size = 12,
                                       face = "bold",
                                       vjust = 1,
                                       hjust = 0.5))
  } else {
    
    choice_learner[[splitCategory]] <- as.factor(as.character(choice_learner[[splitCategory]]))
    
    # ggplot for the learner of choice
    choice_learner_graph <-
      ggplot(choice_learner,
             aes_string(x = "pred_1", fill = splitCategory, alpha = 0.5)) +
      geom_density() +
      xlab ("Predictive Probabilities") +
      ggtitle (paste("Predictive probability distribution for\n", learnerName)) +
      scale_x_continuous(limits = c(0,1)) +
      theme(plot.title = element_text(size = 12,
                                      face = "bold",
                                      vjust = 1,
                                      hjust = 0.5)) +
      guides(alpha = "none")
  }
  
  return(choice_learner_graph)
  
}

# generate variable importance graphs
generateVipGraphs <- function(learnerTrainFits)
{
  
  # error handling for ML algorithms where VIP is not supported.
  robustVip <- function(learnerName, fit)
  {
    tryCatch(vip::vip(fit),
             warning = function(w) { print("Warning"); return(x) },
             error = function(e) { print(paste("Error:", e, learnerName)); return(NULL) })
  }
  
  vipGraphs <- list()
  counter <- 0
  
  for(i in 1:length(learnerTrainFits)){
    
    learnerName <- learnerTrainFits[[i]]$learnerName
    fit <- learnerTrainFits[[i]]$learner$fit
    
    # try to create plot
    vipGraph <- robustVip(learnerName, fit)
    
    if(!is.null(vipGraph))
    {
      counter <- counter + 1
      vipGraph <- vipGraph +
        ggtitle(paste0("Variable Importance for\n", learnerNamesSelect[i])) +
        theme(plot.title = element_text(size = 12,
                                        face = "bold",
                                        vjust = 1,
                                        hjust = 0.5))
      
      
      vipGraphs[[counter]] <- vipGraph
    }
  }
  return(vipGraphs)
}