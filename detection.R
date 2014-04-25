#*********************************************************************
# This is free software and comes with ABSOLUTELY NO WARRANTY.
# You are welcome to redistribute it under GPL conditions.
# Copyright (C) Andrea Dal Pozzolo
#*********************************************************************

library(ROCR)
source(paste(path,"Code/plots.R",sep=""))
library(PerfMeas)
source(paste(path,"Code/cosine.R",sep=""))
source(paste(path,"Code/metrics/confusion.R",sep="")) #mlearning packages
source(paste(path,"Code/metrics/precision_recall.R",sep=""))
source(paste(path,"Code/metrics/pranker.R",sep="")) #Miguel's code
source(paste(path,"Code/caret/confMatrix.R",sep="")) #code from caret


#*********************
#compute the confusion matrix
#Actual are the true values (1 or 0)
#Predicted are the predicted values (1 or 0)
#return as list: TP,FP,TN,FN
#*********************
confMatrix <- function(Predicted, Actual, saveOut=F, verbose=T){
  
  if(length(Predicted)!=length(Actual)) 
    stop("Predicted and Actual have different length")
  
  val=c(0,1)
  if(!all((Predicted %in% val))){
    print(summary(Predicted))
    stop("confMatrix: Predicted taking values different from 0 and 1")
  }
  
  #   #from mlearning packages
  #   conf<-confusion(as.factor(Actual),Predicted)
  #   if(verbose){
  #     print(conf,sums = T, error.col = F, digits = 3)
  #     plot(conf)
  #     print(summary(conf))
  #   }
  
  #   if(verbose){
  #     require(caret)
  #     conf<-confusionMatrix(Predicted, Actual)
  #     print(conf)
  #   }
  
  if (all(Predicted==0) & all(Actual==0)) res<-c(length(Actual),0,0,0)
  if (all(Predicted==1) & all(Actual==1)) res<-c(0,0,0,length(Actual))
  if (all(Predicted==0) & all(Actual==1)) res<-c(0,0,length(Actual),0)
  if (all(Predicted==1) & all(Actual==0)) res<-c(0,length(Actual),0,0)
  
  if (any(Predicted!=Predicted[1]) | any(Actual!=Actual[1])){
    res<-table(Predicted,Actual)
    if (dim(res)[1]==1) {
      if(verbose) cat("WARNING: Predictions are all of the same class \n")
      if (all(Predicted==0)) res=rbind(res,c(0,0))
      if (all(Predicted==1)) res=rbind(c(0,0),res)
    }
    if (dim(res)[2]==1) {
      if(verbose) cat("WARNING: testing observations are all of the same class \n")
      if (all(Actual==0)) res=cbind(res,c(0,0))
      if (all(Actual==1)) res=cbind(c(0,0),res)
    }
  }
  
  if(verbose){
    print(res)
    #plots
    #pdf("confusionPlot.pdf")
    #confusionPlot(res)
    #dev.off()
    #confHeatMap(Actual,Predicted)
  }
  
  r=as.double(res)
  TN=r[1]
  FP=r[2]
  FN=r[3]
  TP=r[4]
  
  if(saveOut) save(file="confMatrix.Rdata",TN,TP,FN,FP)
  
  return(list(TP=TP,FP=FP,TN=TN,FN=FN))
}



#*********************
#print the confusion matrix and return its metrics
#Yts are the true values (1 or 0)
#predictions are the predicted values (1 or 0)
#return as list: acc,recall,precision,Fmeasure,BalErrorRate,Mcc,Tcost
#*********************
classMetrics=function(predictions,Yts,verbose=T,saveOut=F){
  confMat <- confMatrix(predictions,Yts,saveOut,verbose)
  TP <- confMat$TP; FP <- confMat$FP; TN <- confMat$TN; FN <- confMat$FN;
  #mdat <- matrix(c(TP,FP,FN,TN), nrow = 2, ncol=2,byrow=TRUE,
  #               dimnames=list(c("1.pred","0.pred"),c("1.true","0.true")))
  
  results<-getConfMatrixMetrics(TP,FP,FN,TN)
  
  if(saveOut) 
    save(file="classMetrics.Rdata",results)
  
  #print results
  if(verbose){
    #cat("\t Confusion matrix \n")
    #print(mdat)
    #print(unlist(results[1:4]))
    print(round(unlist(results[-c(1:4)]),digit=3))
  }
  
  return(results)
}


#*********************
#given TP,FP,FN,TN compute several metric
#*********************
getConfMatrixMetrics<-function(TP,FP,FN,TN){
  
  acc<-getConfMatrixMetric(TP,FP,FN,TN,metric="acc")
  recall<-getConfMatrixMetric(TP,FP,FN,TN,metric="recall")
  precision<-getConfMatrixMetric(TP,FP,FN,TN,metric="precision")
  normPrecision<-getConfMatrixMetric(TP,FP,FN,TN,metric="normPrecision")
  Fmeasure<-getConfMatrixMetric(TP,FP,FN,TN,metric="Fmeasure")
  BalErrorRate<-getConfMatrixMetric(TP,FP,FN,TN,metric="BalErrorRate")
  Mcc<-getConfMatrixMetric(TP,FP,FN,TN,metric="Mcc")
  TruePositiveRate<-getConfMatrixMetric(TP,FP,FN,TN,metric="TruePositiveRate")
  TrueNegativeRate<-getConfMatrixMetric(TP,FP,FN,TN,metric="TrueNegativeRate")
  Gmean<-getConfMatrixMetric(TP,FP,FN,TN,metric="Gmean")
  
  results<-list(TP=TP,FP=FP,FN=FN,TN=TN,
                Accuracy=acc,Recall=recall,Precision=precision,Fmeasure=Fmeasure,
                BER=BalErrorRate,Mcc=Mcc,normPrec=normPrecision,Gmean=Gmean
  )
  
  return(results)
}



#*********************
#given TP,FP,FN,TN compute the selected metric
#*********************
getConfMatrixMetric<-function(TP,FP,FN,TN,metric="precision"){
  P.pred=TP+FP;	N.pred=FN+TN
  P.act=TP+FN;	N.act=FP+TN
  
  acc <- sum(TN+TP)/sum(TP+FP+TN+FN)
  recall <- ifelse(P.act!=0, TP/P.act, 0)
  precision <- ifelse(P.pred!=0, TP/P.pred, 0)
  Fmeasure <- ifelse((precision+recall) != 0, 2*(precision*recall)/(precision+recall), 0)
  #randomPrecision is equal to the P/(P+N)
  randomPrecision <- P.act/(P.act+N.act)
  normPrecision <- ifelse(randomPrecision!=0, precision/randomPrecision, 0)
  BalErrorRate <- 0.5*(FP/(TN+FP)+FN/(FN+TP))
  Mcc <- MCC(TP,FP,FN,TN)
  Chisq <- Chisquare(TP,FP,FN,TN)
  TruePositiveRate<- recall #TP/(TP+FN)
  TrueNegativeRate<- TN/(TN+FP)
  Gmean<-sqrt(TruePositiveRate*TrueNegativeRate)
  
  if(!(metric %in% c("acc","recall","precision","normPrecision","Fmeasure",
                     "BalErrorRate","Mcc","TruePositiveRate","TrueNegativeRate","Gmean"))) 
    stop("ERROR: metric not supported")
  
  if(metric=="acc")
    result=acc
  if(metric=="recall")
    result=recall
  if(metric=="precision")
    result=precision
  if(metric=="normPrecision")
    result=normPrecision
  if(metric=="Fmeasure")
    result=Fmeasure
  if(metric=="BalErrorRate")
    result=BalErrorRate
  if(metric=="Mcc")
    result=Mcc
  if(metric=="TruePositiveRate")
    result=TruePositiveRate
  if(metric=="TrueNegativeRate")
    result=TrueNegativeRate
  if(metric=="Gmean")
    result=Gmean
  
  return=(result)
}


## MCC : Matthews correlation coefficient
MCC <- function(TP,FP,FN,TN){
  Sum1 <- TP + FP
  Sum2 <- TP + FN
  Sum3 <- TN + FP
  Sum4 <- TN + FN
  Denominator <- sqrt(Sum1 * Sum2 * Sum3 * Sum4)
  ZeroIdx <- Sum1 == 0 | Sum2 == 0 | Sum3 == 0 | Sum4 == 0
  if (any(ZeroIdx)) Denominator[ZeroIdx] <- 1
  Mcc<-((TP * TN) - (FP * FN)) / Denominator
  #   if(((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))^0.5!=0)
  #     Mcc<-((TP*TN)-(FP*FN))/(((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))^0.5)
  #   else Mcc<-0
  
  return(Mcc)
}

## Chi-square Significance
Chisquare <- function(TP,FP,FN,TN){
  return( (((TP * TN) - (FP * FN))^2 * (TP + TN + FP + FN)) /
            ((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)) )
}


#*********************
#combute the dissimilarity between two list using cosinde distance
#*********************
# cosineDissim<-function(listA,listB){
#   
#   if(!is.list(listA) | !is.list(listB))
#     stop("inputs must be a list")
#   
#   if(length(listA)!=length(listB))
#     stop("ERROR different lengths")
#   
#   vectA<-unlist(listA)
#   vectB<-unlist(listB)
#   
#   dissim<-cosine(vectA,vectB)
#   
#   return(as.numeric(dissim))
# }


#*********************
#combute the dissimilarity between two objects using cosinde distance
#*********************
cosineDissim<-function(x,y){
  
  if(is.list(x) & is.list(y)){    
    x<-unlist(x)
    y<-unlist(y)
  }
  
  if(class(x) != class(y))
    stop("object of differnt class")
  
  if(is.list(x) | is.list(y))
    stop("class not supported")
  
  if(length(x)!=length(y))
    stop("objects of different lengths")
  
  dissim<-cosine(x,y)
  
  return(as.numeric(dissim))
}


#*********************
#compute metrics based on the probabiltiy 
#phat.1 = P(y=1|x)
#y = real class {0,1}
#*********************
# example
# y <- c(1,1,1,0,0,0,1,0,1,0)
# phat.1 <- c(0.9,0.8,0.4,0.5,0.3,0.2,0.8,0.3,0.8,0.3)
# probMetrics(phat.1, y)
#*********************
probMetrics <- function(phat.1, y, k = length(y)){
  
  vals <- c(0, 1)
  stopifnot(all(unique(y) %in% vals), length(y) > 0, length(phat.1) > 1)
  
  if(k < length(y)){
    sort.order <- sort(phat.1, decreasing=T, index.return=T)$ix
    pp <- phat.1[sort.order]
    yy <- y[sort.order]
    #keep only the first k observations with highest phat
    phat.1 <- pp[1:k]
    y <- yy[1:k]
  }
  
  #compute Average precision
  #AP <- AvgPrec(phat.1, y, length(y))
  AP <- APk(phat.1, y)
  #compute average precision cumulative recall
  #cumPR <- normCumPR(phat.1, y, length(y))
  
  AUC <- AUCr(phat.1, y)
  Gini <- 2*AUC - 1
  AUCpr <- AUCPR(phat.1, y)
  #AUCpr <- auprc(phat, y)$area  #slower but can compute confidence intervals
  
  N.0 <- length(which(y == 0))
  N.1 <- length(which(y == 1))
  minAUC <- minAuprc(N.0, N.1)
  AUCNPR <- (AUCpr - minAUC) / (1 - minAUC)
  
  BS <- brierScore(phat.1, y)
  sBS <- stratBrierScore(phat.1, y)
  BSpos <- sBS$BS.pos
  BSneg <- sBS$BS.neg
  
  #pBias <- probBias(phat.1, y)
  pBias <- (BSpos + BSneg)/2
  
  metrics <- list(AP = AP, AUROC = AUC, Gini = Gini, AUPRC = AUCpr, AUCNPR  = AUCNPR,
                  BS = BS, BSpos = BSpos, BSneg = BSneg, pBias = pBias) #, cumPR = cumPR
  
  return(metrics)
}


#*********************
# Empirical Area under ROC/ Wilcoxon-Mann-.... stat.
# Also calculate the empirical variance thereof.  Goes as O(n*log(n)).
#*********************
AuROC<-function(neg,pos) {  
  
  nx<-length(neg);
  ny<-length(pos);
  nall<-nx+ny;
  rankall<-rank(c(neg,pos)) # rank of all samples with respect to one another.
  
  rankpos<-rankall[(nx+1):nall];    # ranks of the positive cases
  ranksum <-sum(rankpos)-ny*(ny+1)/2 #sum of ranks of positives among negs.
  
  ranky<-rank(pos); ## ranks of just the y's (positives) among themselves
  rankyx<-rankpos-ranky    # ranks of the y's among the x's (negatives)
  p21<-sum(rankyx*rankyx-rankyx)/nx/(nx-1)/ny; #term in variance
  
  rankx<-rank(neg);  ## ranks of x's (negatives) among each other
  ## reverse ranks of x's with respect to y's.
  rankxy<- ny- rankall[1:nx]+ rankx ;
  p12<- sum(rankxy*rankxy-rankxy)/nx/ny/(ny-1); #another variance term
  
  a<-ranksum/ny/nx;       # the empirical area
  v<-(a*(1-a)+(ny-1)*(p12-a*a) + (nx-1)*(p21-a*a))/nx/ny;
  
  c(a,v);  # return vector containing Mann-Whitney stat and the variance.
}


#*********************
#AUC is equal to the probability that a true positive is scored greater than a true negative.
#*********************
AUCprob<-function(phat, y){
  id.1<-which(y==1)
  pos.scores<-phat[id.1] 	#vector containing a score of the positive examples.
  neg.scores<-phat[-id.1] #vector containing a score of the negative examples.
  AUC<-mean(sample(pos.scores,1000,replace=T) > sample(neg.scores,1000,replace=T))
  return(AUC)
}

#*********************
#Area Under ROC using ROCR package
#*********************
AUCr<-function(phat, y){
  require(ROCR)
  
  n.class <- length(unique(y))
  if(n.class == 1){
    cat("only one class available, NA returned as AUC \n")
    return(NA)
  }
  if(n.class > 2) 
    stop("AUCr only for binary classification problems")
  
  pred.ROCR <- ROCR::prediction(phat, y)
  AUC<- ROCR::performance(pred.ROCR,"auc")
  AUC<- unlist(slot(AUC, "y.values"))
  
  return(AUC)
}


#*********************
#Area Under ROC using pROC package
#*********************
AUROC <- function(phat, y){
  require(pROC)
  
  AUC <- as.numeric(pROC::auc(y, phat))
  return(AUC)
}


#*********************
#confidence interval of AUROC using pROC package
#*********************
ci.AUROC <- function(phat, y, nboot = 2000){
  require(pROC)
  
  CI <- ci(y, phat,  conf.level = 0.95, method ="bootstrap", boot.n = nboot, boot.stratified = TRUE)
  #   avg <- CI[2]
  
  confIterval <- CI[-2]
  names(confIterval) <- c("2.5%", "97.5%")
  
  #names(confIterval) <- c("2.5%", "avg", "97.5%")
  
  return(confIterval)
}


#*********************
#returns the area of the unachievable region in PR space and the minimum AUCPR
#for  a given skew π (pi = Number of Positives / Total number of instances)
#see paper "Unachievable Region in Precision-Recall Space and Its Effect on Empirical Evaluation"
#N: number of negatives instances
#P: number of positives instances
#*********************
minAuprc <- function(N, P){
  
  pi <- P / (P + N)
  minAUC <- 1 + ((1 - pi) * log (1 - pi) / pi) 
  
  return(minAUC)
}


#*********************
#Area Under Precsion Recall Curve
#k is the number of instances to inspect
#*********************
# EXAMPLE:
# y <- c(1,1,1,0,0,0,1,0,1,0)
# phat <- c(0.9,0.8,0.4,0.5,0.3,0.2,0.8,0.3,0.8,0.3)
# auprc(phat, y)
# it is slower than AUCPR function
# system.time(auprc(runif(1:10000), rep(y, 1000)))
# system.time(AUCPR(runif(1:10000), rep(y, 1000)))
#*********************
auprc <- function(phat, y, method = "ap", conf.level=0.95, conf.int.method = "binomial", k = length(y), random = FALSE, replicates = 1000){
  
  n.class <- length(unique(y))
  if(n.class == 1) {
    cat("Warnings: only", unique(y), "class in y, AUPRC is zero \n")
    pr <- list(area = 0)
    return(pr)
  }
  if(n.class > 2) 
    stop("auprc only for binary classification problems")
  
  if(is.null(k))
    k <- length(y)
  
  vals <- c(0,1)
  stopifnot(conf.level < 1, conf.level > 0, 
            conf.int.method %in% c("binomial", "expit", "bootstrap"), 
            all(unique(y) %in% vals), k <= length(y))
  
  #keep only the first k observations with highest phat
  sort.order <- sort(phat, decreasing=T, index.return=T)$ix
  pp <- phat[sort.order]
  yy <- y[sort.order]
  phat <- pp[1:k]
  y <- yy[1:k]
  
  pos.values <- phat[which(y == 1)]
  neg.values <- phat[which(y == 0)]
  P <- length(pos.values)
  N <- length(neg.values)
  
  m = match.arg(method, c("ap", "uppertrap", "lowertrap", "apslow", "binormal", "miguel", "PerfMeas"))
  
  if (m == "ap")
    pr <- prcurve.ap(pos.values, neg.values, conf.level, conf.int.method)
  if (m == "uppertrap")
    pr <- prcurve.uppertrap(pos.values, neg.values, conf.level, conf.int.method)
  if (m == "lowertrap")
    pr <- prcurve.lowertrap(pos.values, neg.values, conf.level, conf.int.method)
  if (m == "apslow")
    pr <- prcurve.ap.slow(pos.values, neg.values, conf.level, conf.int.method)
  if (m == "binormal")
    pr <- prcurve.binormal(pos.values, neg.values, conf.level, conf.int.method)
  if (m == "miguel"){
    pr <- pranker(phat, y)
    auc <- pr$auprc
    ci <- pr$conf.int
    pr <- list(area = auc, conf.int = ci, P = P, N = N)
  }
  if (m == "PerfMeas"){
    auc <- AUCPR(phat, y)
    ci <- ci.AUCPR(phat, y, conf.level, replicates)
    p.value <- slot(ci, "p.value")
    areas <- slot(ci, "areas")
    nullAreas <- slot(ci, "nullAreas")
    normAreaNull <- slot(ci, "meanNorm")
    pr <- list(area = auc, conf.int = ci, p.value = p.value, P = P, N = N, areas = areas, nullAreas = nullAreas, normAreaNull = normAreaNull)
  }
  
  if(random){
    random <- randAUPRC(P + N, P)
    pr$randArea <- random$mean
  }
  
  minAUC <- minAuprc(N, P)
  pr$minArea <- minAUC
  #normalized area using its minimum value for given N, P instances
  pr$normArea <- (pr$area - minAUC) / (1 - minAUC)
  
  #cat("minimum AUCPR:", minAUC, "random:", random$mean, "AUCPR:", pr$area, "\n")
  
  return(pr)
}




#*********************
#returns the normalized area under PR curve
#see paper "Unachievable Region in Precision-Recall Space and Its Effect on Empirical Evaluation"
#*********************
AUCNPR <- function(phat, y){
  
  AUC <- AUCPR(phat, y)
  
  N <- length(which(y == 0))
  P <- length(which(y == 1))
  minAUC <- minAuprc(N, P)
  #normalized area using its minimum value for given N, P instances
  AUCnorm <- (AUC - minAUC) / (1 - minAUC)
  
  return(AUCnorm)
}


#*********************
#Area Under PrecsionRecall Curve using PerfMeas package
#*********************
AUCPR <- function(phat, y){
  require(PerfMeas)
  
  if(is.factor(y)){
    y<-fact2num(y)
  } 
  n.class<-length(unique(y))
  if(n.class==1) {
    cat("Warnings: only", unique(y), "class in y, AUPRC is zero \n")
    return(0)
  }
  if(n.class>2) 
    stop("AUCPR only for binary classification problems")
  
  pr.levels<-precision.at.all.recall.levels(phat,y)
  AUCPRc<-AUPRC(list(pr.levels), comp.precision=TRUE)
  
  return(as.numeric(AUCPRc))
}

#*********************
# stratified boostrap confidence interval for AUCPR
#*********************
ci.AUCPR <- function(phat, y, conf.level=0.95, replicates=1000) {
  
  areas = rep(0, replicates)
  nullAreas <- rep(0, replicates)
  pos.values <- phat[which(y == 1)]
  neg.values <- phat[which(y == 0)]
  
  for (i in 1:replicates) {
    p.v = sample(pos.values, replace=TRUE)
    n.v = sample(neg.values, replace=TRUE)
    pp <- c(p.v,n.v)
    yy <- c(rep(1, length(p.v)), rep(0, length(n.v)))
    areas[i] = AUCPR(pp, yy)
    nullAreas[i] <- nullAUCPR(yy)
  }
  
  
  #   # compute boostrap samples to define a confidence interval
  #   bs <- function(data, indices) {
  #     data <- data[indices, ] # allows boot to select sample
  #     y <- data[ ,ncol(data)]
  #     phat <- data[ ,-ncol(data)]
  #     auc <- AUCPR(phat, y)
  #     return(auc) 
  #   } 
  #   
  #   resamples <- lapply(1:replicates, function(i) {c(sample(which(y == 1), replace=T), sample(which(y == 0), replace=T)) })
  #   areas2 <- foreach(i= 1:replicates, .packages="PerfMeas", .combine=cbind, .export='AUCPR',.inorder = FALSE) %dopar% { 
  #     bs(data.frame(phat, y), resamples[[i]]) 
  #   }
  
  #   w <- wilcox.test(areas, nullAreas, alternative = "greater", paired = TRUE)
  #   p.value <- w$p.value
  t <- t.test(areas, nullAreas, alternative = "greater", paired = TRUE)
  p.value <- t$p.value
  
  #random aerea in the assintotic case of infinite number of instances
  nullAreaInf <- length(which(y == 1))/length(y)
  
  q = quantile(areas, c((1-conf.level)/2, (1+conf.level)/2))
  
  ci = c(q[1],q[2])
  attr(ci,"conf.level") = conf.level
  attr(ci,"method") = "bootstrap"
  attr(ci,"median") = median(areas)
  attr(ci,"mean") = mean(areas)
  attr(ci,"p.value") = p.value
  attr(ci,"areas") = areas
  attr(ci,"nullAreas") = nullAreas
  attr(ci,"nullAreaInf") = nullAreaInf
  attr(ci,"meanNorm") = mean((areas - nullAreas) / (1 - nullAreas))
  
  return (ci)
}


#*********************
#Area Under PrecsionRecall Curve in the case of random predictions
#*********************
nullAUCPR <- function(y){
  require(PerfMeas)
  
  if(is.factor(y)){
    y<-fact2num(y)
  } 
  n.class<-length(unique(y))
  if(n.class==1) {
    cat("Warnings: only", unique(y), "class in y, AUPRC is zero \n")
    return(0)
  }
  if(n.class>2) 
    stop("AUCPR only for binary classification problems")
  
  N <- length(y)
  phat <- runif(N)
  
  pr.levels <- precision.at.all.recall.levels(phat,y)
  AUCPRc <- AUPRC(list(pr.levels), comp.precision=TRUE)
  
  return(as.numeric(AUCPRc))
}




#*********************
#Area Under Fmeasure Curve using PerfMeas package
#*********************
AUCF<-function(phat, y){ 
  n.class<-length(unique(y))
  if(n.class==1) {
    cat("Warnings: only", unique(y), "class in y, AUCF is zero \n")
    return(0)
  }
  if(n.class>2) 
    stop("AUCF only for binary classification problems")
  
  pr.levels<-precision.at.all.recall.levels(phat,y)
  AUCf<-AUPRC(list(pr.levels), comp.precision=FALSE)
  
  return(as.numeric(AUCf))
}


#*********************
#compute AUC using a Monte Carlo simulation
#*********************
rAUC<-function(phat,y,N=10000000){
  if(!all(y %in% c(0,1)))
    stop("must take {0,1} values")
  
  r_pos_p <- sample(phat[y==1], N, replace=TRUE)
  r_neg_p <- sample(phat[y==0], N, replace=TRUE)
  # Monte Carlo probability of a randomly drawn 1 having a higher score than
  # a randomly drawn 0
  rAUC <- mean(r_pos_p > r_neg_p)
  return(rAUC)
}




# #*********************
# #function tha compute the Average Precision fast
# #http://stackoverflow.com/questions/13754900/a-faster-r-implementation-of-average-precision-at-n
# # example
# # actual <- c(1,1,1,0,0,0,1,0,1,0)
# # predicted <- c(1,1,0,0,0,1,1,0,1,0)
# # avgpk(k = length(predicted), actual, predicted)
# #*********************
# avgpk <- function (k = length(predicted), actual, predicted)  {
# 
#   predicted <- head(predicted, k)
#   is.new <- rep(FALSE, length(predicted))
#   is.new[match(unique(predicted), predicted)] <- TRUE
#   
#   is.relevant <- predicted %in% actual & is.new
#   
#   score <- sum(cumsum(is.relevant) * is.relevant / seq_along(predicted)) / min(length(actual), k)
#   
#   score
# }


# #code from "lago" package
# avgp <- function(y, phat, ties=FALSE)
# {
#   # This version written by Mu Zhu, March 2005.
#   # Now takes care of ties ...
#   # Inputs:
#   # y    = true label; MUST be zero or one.
#   # phat = stuff used to rank the items; MUST be the same length as y.
#   # Assumes if phat[i] > phat[j], then item i should come ahead of item j.
#   # Can think of phat[i] as the predicted probability that item i is a one.
#   
#   y<-data.matrix(y)
#   phat<-data.matrix(phat)
#   n<-length(phat)     # count total number of items
#   m<-sum(y)           # count total number of ones
#   sort.order <- order(phat, decreasing=TRUE)
#   pp <- phat[sort.order]
#   yy <- y[sort.order]
#   
#   # this part takes care of ties
#   if (ties) {
#     anchor <- unique(pp); N <- length(anchor);
#     for (i in 1:N) {
#       this <- which(pp==anchor[i])
#       hits <- sum(yy[this])
#       size <- length(this)
#       yy[this] <- hits/size
#     }}
#   
#   dr <- yy/m
#   p <- cumsum(yy)/(1:n)
#   return(sum(p*dr))
#   
# }


#*********************
#compute Average Precision using PerfMeas function (faster than AvgPrec function)
#does NOT takes care of ties
# Average Precision is an estimation of area under the PR curve
# y <- c(1,1,1,0,0,0,1,0,1,0)
# phat <- c(0.9,0.8,0.4,0.5,0.3,0.2,0.8,0.3,0.8,0.3)
# APk(phat, y)
# APk(runif(1:100), rep(y,10))
#*********************
APk <- function(phat, y, k = length(y)){
  require(PerfMeas)
  
  if(length(y) != length(phat))
    stop("y and phat have different length")
  
  if(!all( y %in% c(0,1) ))
    stop("y taking values different from 0 and 1")
  
  pr.levels <- precision.at.all.recall.levels(phat,y)
  P <- pr.levels$precision[1:k]
  R <- pr.levels$recall[1:k]
  DeltaR <- R - c(0, R[1: (k-1)])
  AP <- sum(P*DeltaR)
  
  return(AP)
}


#compute a bootstrap conf int using stratified bootstrap
# ciAPk(runif(1:1000), rep(c(1,1,1,0,0,0,1,0,1,0), 100))
# system.time(ciAPk(runif(1:1000), rep(c(1,1,1,0,0,0,1,0,1,0), 100)))
ciAPk <- function(phat, y, k=length(y), conf.level=0.95, replicates=1000) {
  
  apks <- rep(0, replicates)
  pos.values <- which(y == 1)
  neg.values <- which(y == 0)  
  
  #stratified bootstrap, keep the same number of instances in each class
  #Note that AP depens on the class imbalance ratio 
  
  ## SLOWER than for loop ##
  # p.vals <- sapply(1:replicates, function(i) sample(pos.values, replace=TRUE))
  # n.vals <- sapply(1:replicates, function(i) sample(neg.values, replace=TRUE))
  # samples <- rbind(p.vals, n.vals)
  # apks <- sapply(samples, function(id) APk(phat[id], y[id], k))
  
  for (i in 1:replicates) {
    p.v <- sample(pos.values, replace=TRUE)
    n.v <- sample(neg.values, replace=TRUE)
    id.sample <- c(p.v, n.v)
    pp <- phat[id.sample]
    yy <- y[id.sample]
    
    apks[i] <- APk(pp, yy, k)
  }
  
  q = quantile(apks, c((1-conf.level)/2, (1+conf.level)/2))
  ci = c(q[1],q[2])
  attr(ci,"conf.level") = conf.level
  attr(ci,"method") = "bootstrap"
  attr(ci,"median") = median(apks)
  attr(ci,"mean") = mean(apks)
  
  return(ci)
}




#*********************
#function tha compute the Average Precision using Mu Zhu's code from "lago" package
#takes care of ties
# #if for the same phat we have positive and negative cases then ties=T
#*********************
AvgPrec<-function(phat, y, nObsMax=length(y), ties=TRUE){
  if(is.null(phat)) 
    return(NA)
  
  if(!all( y %in% c(0,1) ))
    stop("y taking values different from 0 and 1")
  
  if(length(phat) != length(y))
    stop("AvgPrec: length(phat)!=length(y)")
  
  if(is.factor(y)) 
    y <- as.numeric(y)-1
  
  if(length(y) < nObsMax)
    nObsMax <- length(y)
  
  P <- sum(y)
  N <- length(y)-P
  if(P == 0){
    #cat("WARNING: no Positives cases, AvgPrec=0 \n")
    return(AP=0)
  }
  
  # #Understand if "ties" has to be TRUE or FALSE
  # ties=F
  # p.unique=unique(phat)
  # if (length(p.unique)!=length(phat)){
  # for (i in 1:length(p.unique)) {
  # this <- which(phat==p.unique[i])
  # if (any(y[this]==1) && any(y[this]==0))
  # ties=T
  # }
  # }
  
  sort.order<-sort(phat, decreasing=T, index.return=T)$ix
  pp <- phat[sort.order]
  yy <- y[sort.order]
  #keep only the first nObsMax observations with highest phat
  pp <- pp[1:nObsMax]
  yy <- yy[1:nObsMax]
  
  # this part takes care of ties
  #average over all possible permutations of the tied items
  if (ties) {
    anchor <- unique(pp)
    N <- length(anchor)
    
    this <- sapply(anchor, function(x, pp) which(pp == x), pp)
    yy.this <- lapply(this, function(x, yy) yy[x] <- rep(sum(yy[x])/length(x), length(x)), yy)
    yy <- unlist(yy.this)
    
    #     for (i in 1:N) {
    #       this <- which(pp == anchor[i])
    #       hits <- sum(yy[this])
    #       size <- length(this)
    #       yy[this] <- hits/size
    #     }
    
  }
  
  dr <- yy/P	
  p <- cumsum(yy)/(1:nObsMax)
  AP <- sum(p*dr)
  
  return(AP)
}	

#*********************	
#function tha compute the average of precision and cumulative recall within the first "nObsMax" observation with highest prob
#the better the ranking the higher the measure
#measure base on Average Precision (code from lago package)
#*********************
normCumPR<-function(phat,y,nObsMax,ties=T){
  if(is.null(phat)) 
    return(NA)
  
  if(length(phat)!=length(y)) {
    print(summary(phat))
    print(summary(y))
    stop("normCumPR: length(phat)!=length(y)")
  }
  if(is.factor(y)) y=as.numeric(y)-1
  
  if(length(y)<nObsMax)
    nObsMax=length(y)
  P=sum(y)
  N=length(y)-P
  if(P==0){
    #cat("WARNING: no Positives cases, cumPR=0 \n")
    return(normCumPR=0)
  }
  
  sort.order<-sort(phat, decreasing=T, index.return=T)$ix
  pp <- phat[sort.order]
  yy <- y[sort.order]
  
  minyy<-c(rep(0,length(yy)-P),rep(1,P)) #all TP at the bottom gives the lowest score
  maxyy<-c(rep(1,P),rep(0,length(yy)-P)) #all TP at the top the highest score
  
  cumPR<-cumPR(pp,yy,nObsMax,P,ties)
  minCumPR<-cumPR(pp,minyy,nObsMax,P,ties=FALSE) #to have the same min ties must be FALSE
  maxCumPR<-cumPR(pp,maxyy,nObsMax,P,ties=FALSE) #to have the same max ties must be FALSE
  
  normCumPR<-(cumPR-minCumPR)/(maxCumPR-minCumPR)
  
  return(normCumPR)
}	


#function used in normCumPR
cumPR<-function(pp,yy,nObsMax,P,ties){
  #keep only the first nObsMax observations with highest phat
  pp=pp[1:nObsMax]
  yy=yy[1:nObsMax]
  # this part takes care of ties
  #average over all possible permutations of the tied items
  if (ties) {
    anchor <- unique(pp); N <- length(anchor);
    for (i in 1:N) {
      this <- which(pp==anchor[i])
      hits <- sum(yy[this])
      size <- length(this)
      yy[this] <- hits/size
    }
  }
  
  cumRecall <- cumsum(yy)/P	
  Precision <- cumsum(yy)/(1:nObsMax)
  avgPR<- (Precision+cumRecall)/2
  cumPR<-sum(avgPR)
  return(cumPR)
}



#*********************
#define a cost of every entries of the confusion matrix
#compute the total cost of the confusion matrix
#costType: 1 in case use priop probability, 2 for fraud detection, 3 for claim predictions
#predictions and Yts have to take value 0 or 1
#TS_COST is a vector, FPcost a constant
#output: total cost
#*********************
CostMatrix<- function(Predicted,Actual,TS_COST,FPcost,costType,verbose=T){
  
  if(verbose) cat("costType:",costType,"\n")
  #switch(costType,"unbalance ratio","fraudCost","claimCost"),"\t"))
  
  if(!(costType %in% c("fraud","claim","noCost")))
    stop("costType not supported")      
  
  
  confMat <- confMatrix(Predicted,Actual,verbose=F)
  TP <- confMat$TP; FP <- confMat$FP; TN <- confMat$TN; FN <- confMat$FN;
  id.FN=which(Predicted==0 & Actual==1)
  if(!is.null(TS_COST))
    FNcost=sum(TS_COST[id.FN])
  
  i.1<-which(Actual==1);    N.1<-length(i.1)
  i.0<-which(Actual==0);		N.0<-length(i.0)
  unbalrate<-N.1/length(Actual)
  cost.1<-1-unbalrate
  cost.0<-unbalrate
  #if the cost is not given assume that the cost of missclassifing an observation is proportional to the 1-prior probability of the class
  if(costType=="noCost")	Tcost <- (cost.1*FN)+(cost.0*FP)
  
  #fraudCost
  #FPcost & TPcost is equal (cost of an investigartor which is constant)
  #FNcost is given by trx amount which is a vector
  if(costType=="fraud")	Tcost <- FNcost+(FP+TP)*FPcost
  
  #claimCost (claimData)
  #FPcost is given by the insurance premium which is constant
  #FNcost is given by claim amount which is a vector
  #TNcost is equal to -FPcost (earning not a cost)
  #TPcost is zero (no loss no gain)
  if(costType=="claim")	Tcost <- FNcost+(FP*FPcost)-(TN*FPcost) 
  
  return(Tcost)
}


#*********************
#find the threshold that minimize the cost of the classification
#phat = posteriori prob of class 1, P(y=1|x)
#y = real class (0 or 1)
#*********************
minCostThreshold<-function(phat,y,TS_COST,FPcost){
  if(length(phat)!=length(y)) {
    print(summary(phat))
    print(summary(y))
    stop("minCostThreshold: length(phat)!=length(y)")
  }
  T=seq(0,1,0.01)
  cost<-NULL
  for(Ttemp in T){
    predicted=as.numeric(phat>=Ttemp)
    resT<-array(NA,c(2,2))
    resT[1,1]<-length(which(predicted==0 & y==0))
    resT[1,2]<-length(which(predicted==0 & y==1))
    resT[2,1]<-length(which(predicted==1 & y==0))
    resT[2,2]<-length(which(predicted==1 & y==1))
    FP<-resT[2,1]
    TN<-resT[1,1]   
    FN<-resT[1,2]
    TP<-resT[2,2]
    
    id.FN=which(predicted==0 & y==1)
    FNcost=sum(TS_COST[id.FN])
    #fraudCost
    Tcost <- FNcost+(FP+TP)*FPcost
    cost<-c(cost,Tcost)
  }
  minCost<-min(cost)
  id.minCost=which(cost==minCost)
  if(length(id.minCost)>1) print("Warnings: more than one minimum cost threshold")
  minCostT<-T[id.minCost]
  print(paste("minCostT",minCostT))
  return(minCostT)
}	


#*********************
#compute the metrics for Atos detection system using data driven rules
#return the metric needed
#*********************
AtosDetection=function(SCORE,phat.1,Yts,TS_COST,FPcost,metric="ap",verbose=T){
  
  if(is.null(metric))
    return(NULL)
  
  #SCORE has value 0.5 if the trx is not associated to an Atos dataDriven rule
  id.dataDriven <- which(SCORE!=0.5)
  id.notDataDriven <- which(SCORE==0.5)
  N=length(Yts)
  N.dataDriven=length(id.dataDriven)
  N.notDataDriven=length(id.notDataDriven)
  
  id.FN=which(Yts[id.notDataDriven]==1)
  id.TN=which(Yts[id.notDataDriven]==0)
  id.TP=which(Yts[id.dataDriven]==1)
  id.FP=which(Yts[id.dataDriven]==0)
  TP=as.double(length(id.TP))
  FP=as.double(length(id.FP))
  FN=as.double(length(id.FN))
  TN=as.double(length(id.TN))
  
  recall <- TP/sum(TP+FN)
  precision <- TP/sum(TP+FP)
  Fmeasure <- 2*(precision*recall)/(precision+recall)
  Mcc <- ((TP*TN)-(FP*FN))/(((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))^0.5)
  FNcost <- sum(TS_COST[id.FN])
  Tcost <- FNcost+(FP+TP)*FPcost
  #AP <- AvgPrec(SCORE,Yts,length(Yts)) 
  AP <- APk(SCORE,Yts,length(Yts)) 
  cumPR <- normCumPR(SCORE,Yts,length(Yts))
  
  AtosMetrics<-c(recall,precision,Fmeasure,Mcc,FNcost,Tcost,AP,cumPR)
  names(AtosMetrics)<-c("recall","precision","Fmeasure","Mcc","FNcost","Tcost","AP","cumPR")
  
  res.metric<-NULL
  if(metric=="prec")  res.metric<-precision
  if(metric=="rec")   res.metric<-recall
  if(metric=="F")     res.metric<-Fmeasure
  if(metric=="mcc")   res.metric<-Mcc
  if(metric=="cost")  res.metric<-Tcost
  if(metric=="ap")    res.metric<-AP
  if(metric=="pr")    res.metric<-cumPR
  
  if(verbose){
    cat("ATOS Detection (metric returned:",metric,"):",res.metric,"\n")
    #print(AtosMetrics)
  }
  
  return(res.metric)
}


#*********************
#function tha shows the confusion matrix and its metrics using the first X numb of obs with highest probability (phat)
#return metrics for different number of top predictions considered (numPred2use)
#*********************
PredX<-function(phat,y,numPred2use,verbose=T){
  if(length(phat)!=length(y)) {
    print(summary(phat))
    print(summary(y))
    stop("PredX: length(phat)!=length(y)")
  }
  if(is.factor(y)) y=as.numeric(y)-1
  #set the theshold to use
  P=sum(y)
  N=length(y)-P
  if(P==0){
    if(verbose) cat("WARNING: no Positives cases, results = NULL \n")
    results=NULL
    return(results)
  }
  else {
    data=cbind(phat,y)
    data=data[order(data[,1],decreasing = TRUE),]
    results<-list()
    i=0
    for(j in numPred2use){
      if(length(y)<j) break()
      predicted<-data[1:j,1]
      HighestProbj<-predicted[1]
      LowestProbj=predicted[j]
      id<-which(data[,1]==LowestProbj)
      l<-length(id)
      lastObsLowestProbj<-id[l]
      if(verbose){
        if (sum(predicted)==0) print("All probabilities P(Y=1|X) equal to zero")
        if (j<lastObsLowestProbj)
          cat("Obs until",lastObsLowestProbj,"have probability ",LowestProbj,
              "; sorting within",j,"doesn't make sense \n")
        if (all(predicted==HighestProbj)) cat("All probabilities with first",j,"obs =",HighestProbj,"\n")
      }
      Label=data[1:j,2]
      TP=sum(Label)
      FP=j-TP
      FN=P-TP
      TN=N-FP
      Threshold=predicted[j]
      res<-getConfMatrixMetrics(TP,FP,FN,TN)
      #res<-c(N=j,TP=TP,FP=FP,FN=FN,TN=TN,Prob=Threshold,res)
      res<-c(N=j,Prob=Threshold,res)
      i=i+1
      results[[i]]<-unlist(res)
    }
    results<-do.call(rbind,results)
    results<-as.data.frame(results)
    return(results)
  }
}	

#*********************
#function that returns a metric using the first j numb of obs with highest probability (phat) 
#return the selected metrics
#*********************
RankingProbMetric<-function(phat,y,j,verbose=T){
  checkLength(phat,y) #form utiltyAndrea.R
  if(is.factor(y)) y=as.numeric(y)-1
  
  n.class<-length(unique(y))
  if(n.class==1) return(NULL)
  if(n.class>2) stop("RankingProbMetric only for binary classification problems")
  
  P=sum(y)
  N=length(y)-P
  if(P==0){
    if(verbose) cat("WARNING: no Positives cases, results = NULL \n")
    return(NULL)
  }
  else{
    data=cbind(phat,y)
    data=data[order(data[,1],decreasing = TRUE),]
    if(length(y)<j) {
      stop("RankingProbMetric: length(y)<j")
    }
    predicted=data[1:j,1]
    HighestProb=predicted[1]
    LowestProb=predicted[j]
    id=which(data[,1]==LowestProb)
    l=length(id)
    lastObsLowestProb=id[l]
    if(verbose){
      if (sum(predicted)==0) cat("All probabilities P(Y=1|X) equal to zero \n")
      if (j<lastObsLowestProb) cat("Obs until",lastObsLowestProb,"have the probability ",
                                   LowestProb,", sorting within",j,"doesn't make sense \n")
      if (all(predicted==HighestProb)) cat("All probabilities with first",j,"obs =",HighestProb,"\n")
    }
    Label=data[1:j,2]
    # write.csv(as.numeric(Label), file =paste("Ranking",round(j/length(y)*100,digit=2),"%.csv",sep=""))
    # write.csv(data[1:j,], file =paste("DataRanking",round(j/length(y)*100,digit=2),"%.csv",sep=""))
    write.csv(data[1:j,], file ="DataRanking.csv")
    TP=sum(Label)
    FP=j-TP
    FN=P-TP
    TN=N-FP
    Threshold=predicted[j]
    
    results<-getConfMatrixMetrics(TP,FP,FN,TN)
    p.1<-P/length(y)
    binom<-pbinom(q=TP, size =j, prob=p.1) #prob of having at least TP over j with prob p.1
    #use the prob from the binomial distribution to normalise the precision within the first j observations. 
    precBinom<-results$Precision/binom
    #in the random case i expect to have p.1 of TP within the first j observations
    normalizeTP<-TP/(p.1*j)		
    AucPr<-AUCPR(predicted, Label)
    
    results<-c(results,precBinom=precBinom,normalizeTP=normalizeTP,AucPr=AucPr)
  }
  
  return(results)		
}

#*********************
#normalizedGini is the gini index divided by the max gini index achivable
#it is a ranking measure, the better the rank the higher the score
#aa = actual; pp= predicted
#code from claim prediction kaggle competition
#*********************
normalizedGini <- function(pp,aa) {
  gini(pp,aa) / gini(aa,aa)
}

#function used in normalizedGini
gini <- function(p,a) {
  if (length(a) != length(p)) stop("Actual and Predicted need to be equal lengths!")
  temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
  temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
  population.delta <- 1 / length(a)
  total.losses <- sum(a)
  null.losses <- rep(population.delta, length(a)) 
  accum.losses <- temp.df$actual / total.losses 
  gini.sum <- cumsum(accum.losses - null.losses) 
  sum(gini.sum) / length(a)
}



#*********************
#stratified Brier score
#from Wallance's paper: Improving class probability estimates for imbalanced data
# phat = p( y = 1 | x )
# y = {0,1}
#*********************
stratBrierScore<-function(phat, Y){
  
  stopifnot(all(unique(Y) %in% c(0,1)), phat <= 1, phat >= 0, length(phat) == length(Y))
  
  i.1 <- which(Y==1)
  N.1 <- length(i.1)
  i.0 <- which(Y==0)
  N.0 <- length(i.0)
  prob.0 <- (1-phat)[i.0]
  prob.1 <- phat[i.1]
  BS.1 <- sum((1 - prob.1)^2)/N.1
  BS.0 <- sum((1 - prob.0)^2)/N.0
  #BS.0 <- sum((Y.0 - prob.0)^2)/N.0 this is what it is written in eq.4, but it must be wrong !!
  
  return(list(BS.pos=BS.1, BS.neg=BS.0))
}


#*********************
#Brier score (Mean Squared Error or MSE loss)
# phat = p( y = 1 | x )
# y = {0,1}
#*********************
brierScore<-function(phat, Y){
  stopifnot(all(unique(Y) %in% c(0,1)), phat <= 1, phat >= 0, length(phat) == length(Y))
  
  N <- length(Y)
  BS <- sum((Y - phat)^2) / N
  return(BS)
}


#*********************
#prob bias measured as average of stratified Brier score
# phat = p( y = 1 | x )
# y = {0,1}
#*********************
probBias <- function(phat, Y){
  stopifnot(all(unique(Y) %in% c(0,1)), phat <= 1, phat >= 0, length(phat) == length(Y))
  
  N <- length(Y)
  i.1 <- which(Y==1)
  N.1 <- length(i.1)
  i.0 <- which(Y==0)
  N.0 <- length(i.0)
  prob.0 <- (1-phat)[i.0]
  prob.1 <- phat[i.1]
  BS.1 <- sum((1 - prob.1)^2) / N.1
  BS.0 <- sum((1 - prob.0)^2) / N.0
  
  #BS <- sum((Y - phat)^2) / N
  #wBS <- (BS.1 * N.1 + BS.0 * N.0) / N
  # wBS == BS  
  
  #correspond to balanced error rate in classification
  bias <- (BS.1 + BS.0) / 2
  
  #wbias <- (BS.1 / N.1 + BS.0 / N.0) / (N.1 + N.0)
  #cat("bias", bias, "wbias", wbias, "BS.1", BS.1, "BS.1", BS.1, "\n")
  
  return(bias)
}
