## ----setup, include=FALSE----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------------------------------
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



## ----------------------------------------------------------------------------------------------------------
# Q.test(fx = analysis.sf$group, coor = coords, m = 1, r = 1, na.action = na.omit)
# debug: {
#     con <- list(distance = "Euclidean", nsim = 999, seedinit = 1111, 
#         dtmaxabs = 0, dtmaxpc = 0, dtmaxknn = 0)
#     nmsC <- names(con)
#     con[(namc <- names(control))] <- control
#     if (length(noNms <- namc[!namc %in% nmsC])) 
#         warning("unknown names in control: ", paste(noNms, collapse = ", "))
#     distance <- con$distance
#     nsim <- con$nsim
#     seedinit <- con$seedinit
#     dtmaxabs <- con$dtmaxabs
#     dtmaxpc <- con$dtmaxpc
#     dtmaxknn <- con$dtmaxknn
#     cl <- match.call()
#     if (!is.null(formula) && !is.null(data)) {
#         if (inherits(data, "Spatial")) 
#             data <- as(data, "sf")
#         mfx <- model.frame(formula, data, na.action = na.action)
#     }
#     else if (!is.null(fx) && !is.null(coor)) {
#         mfx <- fx
#         if (!is.matrix(mfx) && !is.data.frame(mfx)) 
#             mfx <- as.matrix(mfx, ncol = 1)
#         mfx <- as.data.frame(mfx)
#         if (is.matrix(coor)) 
#             coor <- sp::SpatialPoints(coor)
#         if (inherits(coor, "Spatial")) 
#             coor <- as(coor, "sf")
#         data <- coor
#     }
#     else stop("input data wrong")
#     for (i in 1:ncol(mfx)) {
#         if (!is.factor(mfx[, i])) 
#             mfx[, i] <- as.factor(mfx[, i])
#     }
#     mfx <- as.matrix(mfx)
#     lres <- NULL
#     if (!is.null(seedinit)) 
#         set.seed(seedinit)
#     initobs <- sample(1:nrow(mfx), 1)
#     for (i in 1:ncol(mfx)) {
#         xfi <- mfx[, i]
#         if (!is.factor(xfi)) 
#             xfi <- as.factor(xfi)
#         N <- length(xfi)
#         ki <- length(levels(xfi))
#         lms <- vector(mode = "list", length = length(m))
#         for (j in seq_len(length(m))) {
#             mj <- m[j]
#             symbi <- cr_symb(ki, mj)
#             if (distr == "asymptotic") {
#                 rcut <- r[!(r > (mj - 1))]
#                 lms[[j]] <- vector(mode = "list", length = length(rcut))
#                 for (h in seq_len(length(rcut))) {
#                   rh <- rcut[h]
#                   lms[[j]][[h]] <- m.surround(x = data, m = mj, 
#                     r = rh, distance = distance, control = list(initobs = initobs, 
#                       dtmaxabs = dtmaxabs, dtmaxpc = dtmaxpc, 
#                       dtmaxknn = dtmaxknn))
#                   R <- nrow(lms[[j]][[h]]$ms)
#                   qi <- q_symb_A2(xfi, lms[[j]][[h]]$ms, symbi)
#                   qi$qp_df <- nrow(symbi$p_symb) - 1
#                   qi$qc_df <- nrow(symbi$c_symb) - 1
#                   statistic <- c(qi$qp, qi$qc)
#                   names(statistic) <- c("Qp", "Qc")
#                   method <- c("Qp (asymptotic distrib.) for standard symbolization based on permutations", 
#                     "Qc (asymptotic distrib.) for equivalent symbolization based on combinations")
#                   if (!is.null(colnames(mfx)[i])) {
#                     data.name <- rep(colnames(mfx)[i], 2)
#                   }
#                   else data.name <- c(NULL, NULL)
#                   parameter <- c(qi$qp_df, qi$qc_df)
#                   names(parameter) <- rep("df", 2)
#                   p.value <- rep(0, length(parameter))
#                   for (l in 1:length(p.value)) {
#                     p.value[l] <- pchisq(statistic[l], df = parameter[l], 
#                       lower.tail = FALSE)
#                   }
#                   lres_i <- vector(mode = "list", length = length(statistic))
#                   for (t in 1:length(statistic)) {
#                     lres_i[[t]] <- list(statistic = statistic[t], 
#                       parameter = parameter[t], p.value = p.value[t], 
#                       method = method[t], data.name = paste("Variable", 
#                         data.name[t], " m = ", mj, " r = ", rh), 
#                       var.name = data.name[t], N = N, R = R, 
#                       m = mj, r = rh, k = ki, df = parameter[t])
#                     lres_i[[t]]$distr <- distr
#                     lres_i[[t]]$ms <- lms[[j]][[h]]$ms
#                     lres_i[[t]]$mdtms <- lms[[j]][[h]]$mdtms
#                     lres_i[[t]]$initobs <- lms[[j]][[h]]$initobs
#                     lres_i[[t]]$distance <- lms[[j]][[h]]$distance
#                     if (names(statistic)[t] == "Qp") {
#                       lres_i[[t]]$type <- "standard-permutations"
#                       lres_i[[t]]$symb <- symbi$p_symb
#                       lres_i[[t]]$efp_symb <- qi$efp_symb
#                       lres_i[[t]]$qp_symb <- qi$qp_symb
#                       lres_i[[t]]$PSymb <- qi$PSymb
#                     }
#                     else if (names(statistic)[t] == "Qc") {
#                       lres_i[[t]]$type <- "equivalent-combinations"
#                       lres_i[[t]]$symb <- symbi$c_symb
#                       lres_i[[t]]$efc_symb <- qi$efc_symb
#                       lres_i[[t]]$qc_symb <- qi$qc_symb
#                       lres_i[[t]]$CSymb <- qi$CSymb
#                     }
#                     else {
#                       stop("statistic neither Qp or Qc")
#                     }
#                     lres_i[[t]]$n <- nrow(lres_i[[t]]$symb)
#                     class(lres_i[[t]]) <- c("htest")
#                   }
#                   lres <- c(lres, lres_i)
#                 }
#             }
#             else if (distr == "mc") {
#                 qi <- q_mc(fx = xfi, x = data, m = mj, nsim = nsim, 
#                   distance = distance, seedinit = seedinit)
#                 statistic <- c(qi$qp, qi$qc)
#                 names(statistic) <- c("Qp", "Qc")
#                 method <- c("Qp (mc distrib.) for symbolization based on permutations", 
#                   "Qc (mc distrib.) for symbolization based on combinations")
#                 if (!is.null(colnames(mfx)[i])) {
#                   data.name <- rep(colnames(mfx)[i], 2)
#                 }
#                 else data.name <- c(NULL, NULL)
#                 parameter <- c("NA", "NA")
#                 names(parameter) <- rep("df", 2)
#                 p.value <- c(qi$pvaluemc_qp, qi$pvaluemc_qc)
#                 lres_i <- vector(mode = "list", length = length(statistic))
#                 for (t in 1:length(statistic)) {
#                   lres_i[[t]] <- list(statistic = statistic[t], 
#                     parameter = parameter[t], p.value = p.value[t], 
#                     method = method[t], data.name = paste("Variable", 
#                       data.name[t], " m = ", mj, " r = ", mj - 
#                         1), var.name = data.name[t], N = N, R = nrow(qi$ms), 
#                     m = mj, r = mj - 1, k = ki, df = parameter[t])
#                   lres_i[[t]]$distr <- distr
#                   lres_i[[t]]$ms <- qi$ms
#                   lres_i[[t]]$mdtms <- qi$mdtms
#                   lres_i[[t]]$distance <- qi$distance
#                   if (names(statistic)[t] == "Qp") {
#                     lres_i[[t]]$type <- "standard-permutations"
#                     lres_i[[t]]$symb <- symbi$p_symb
#                     lres_i[[t]]$efp_symb <- qi$efp_symb
#                     lres_i[[t]]$qp_symb <- qi$qp_symb
#                     lres_i[[t]]$PSymb <- qi$PSymb
#                     lres_i[[t]]$qp_mc <- qi$qpmc
#                     lres_i[[t]]$efp_symb_mc <- qi$efp_symb_mc
#                   }
#                   else if (names(statistic)[t] == "Qc") {
#                     lres_i[[t]]$type <- "equivalent-combinations"
#                     lres_i[[t]]$symb <- symbi$c_symb
#                     lres_i[[t]]$efc_symb <- qi$efc_symb
#                     lres_i[[t]]$qc_symb <- qi$qc_symb
#                     lres_i[[t]]$CSymb <- qi$CSymb
#                     lres_i[[t]]$qc_mc <- qi$qcmc
#                     lres_i[[t]]$efc_symb_mc <- qi$efc_symb_mc
#                   }
#                   else {
#                     stop("statistic neither Qp or Qc")
#                   }
#                   lres_i[[t]]$n <- nrow(lres_i[[t]]$symb)
#                   class(lres_i[[t]]) <- c("htest")
#                 }
#                 lres <- c(lres, lres_i)
#             }
#         }
#     }
#    class(lres) <- c("spqtest", class(lres))
#    return(lres)
#}



