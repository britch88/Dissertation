## ----setup, include=FALSE---------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------
options(scipen = 100)

long_traj <- function(model,data){
  df <- data.frame(data)
  vars <- names(df)
  prob <- model['gwt'] #posterior probabilities
  df$GMax <- apply(prob$gwt,1,which.max) #which group # is the max
  df$PMax <- apply(prob$gwt,1,max)       #probability in max group
  df$Ord <- 1:dim(df)[1]                 #Order of the original data
  prob <- data.frame(prob$gwt)
  names(prob) <- paste0("G",1:dim(prob)[2]) #Group probabilities are G1, G2, etc.
  longD <- reshape(data.frame(df,prob), varying = vars, v.names = "y", 
                   timevar = "x", times = 1:length(vars), 
                   direction = "long") #Reshape to long format, time is x, y is original count data
  return(longD)                        #GMax is the classified group, PMax is the probability in that group
}


weighted_means <- function(model,long_data){
  G_names <- paste0("G",1:model$ng)
  G <- long_data[,G_names]
  W <- G*long_data$y                                    #Multiple weights by original count var
  Agg <- aggregate(W,by=list(x=long_data$x),FUN="sum")  #then sum those products
  mass <- colSums(model$gwt)                            #to get average divide by total mass of the weight
  for (i in 1:model$ng){
    Agg[,i+1] <- Agg[,i+1]/mass[i]
  }
  long_weight <- reshape(Agg, varying=G_names, v.names="w_mean",
                         timevar = "Group", times = 1:model$ng, 
                         direction = "long")           #reshape to long
  return(long_weight)
}
  
pred_means <- function(model){
    prob <- model$prob               #these are the model predicted means
    Xb <- model$X %*% model$beta     #see getAnywhere(plot.dmZIPt), near copy
    lambda <- exp(Xb)                #just returns data frame in long format
    p <- exp(-model$tau * t(Xb))
    p <- t(p)
    p <- p/(1 + p)
    mu <- (1 - p) * lambda
    t <- 1:nrow(mu)
    myDF <- data.frame(x=t,mu)
    long_pred <- reshape(myDF, varying=paste0("X",1:model$ng), v.names="pred_mean",
                         timevar = "Group", times = 1:model$ng, direction = "long")
    return(long_pred)
}

#Note, if you estimate a ZIP model instead of the ZIP-tau model
#use this function instead of pred_means
pred_means_Nt <- function(model){
    prob <- model$prob               #these are the model predicted means
    Xb <- model$X %*% model$beta     #see getAnywhere(plot.dmZIP), near copy
    lambda <- exp(Xb)                #just returns data frame in long format
	Zg <- model$Z %*% model$gamma
    p <- exp(Zg)
    p <- p/(1 + p)
    mu <- (1 - p) * lambda
    t <- 1:nrow(mu)
    myDF <- data.frame(x=t,mu)
    long_pred <- reshape(myDF, varying=paste0("X",1:model$ng), v.names="pred_mean",
                         timevar = "Group", times = 1:model$ng, direction = "long")
    return(long_pred)
}

occ <- function(long_data){
 subdata <- subset(long_data,x==1)
 agg <- aggregate(subdata$PMax,by=list(group=subdata$GMax),FUN="mean")
 names(agg)[2] <- "AvePP" #average posterior probabilites
 agg$Freq <- as.data.frame(table(subdata$GMax))[,2]
 n <- agg$AvePP/(1 - agg$AvePP)
 p <- agg$Freq/sum(agg$Freq)
 d <- p/(1-p)
 agg$OCC <- n/d #odds of correct classification
 agg$ClassProp <- p #observed classification proportion
 #predicted classification proportion
 agg$PredProp <- colSums(as.matrix(subdata[,grep("^[G][0-9]", names(subdata), value=TRUE)]))/sum(agg$Freq) 
 #Jeff Ward said I should be using PredProb instead of Class prop for OCC
 agg$occ_pp <- n/ (agg$PredProp/(1-agg$PredProp))
 return(agg)
}

