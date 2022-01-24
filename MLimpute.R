setwd("D:\\יונתן\\תואר שני\\סמסטר ד\\סמינריון")
library(party)
library(AUC)
library(caret)
library(nnet)
library(forecast)
library(doParallel)
library(foreach)
library(tm)

###Allow parralel computing
cores <- detectCores()
cl <- makeCluster(7)
registerDoParallel(cl)
#getDoParWorkers()

###Loading all functions
##Functions - assistance
#Function to insert NAs into dataframe at a desired ratio
insertNA <- function(df,NAratio,seed) {
  set.seed(seed)
  sel <- sample( nrow(df)*ncol(df), size = NAratio*nrow(df)*ncol(df) )
  for (i in c(1:length(sel))) {
    a <- as.integer((sel[i]-1)/ncol(df)+1)
    b <- sel[i] - (a-1)*ncol(df)
    df[a,b] <- NA
  }
  return(df)
}
#Function for missing values grade by variables indexes
missing.values.grades <- function(df) {
  rows <- nrow(df)
  grades <- c()
  for (i in c(1:ncol(df))) {
    NAs <- sum(is.na(df[,i]))
    grades <- c(grades,NAs/rows)
  }
  return(grades)
}
#Function for sorting data frame by attributes' grade
sort.by.grade <- function(df,grades) {
  return(df[,order(grades)])
}
#Function for creating na indexes of a data frame
na.indexes <- function(df) {
  na.index <- list()
  for (i in c(1:ncol(df))) {
    na.index[[i]] <- c("NONAS")
    for (j in c(1:nrow(df))) {
      if(is.na(df[j,i])){
        if(class(na.index[[i]])=="character") na.index[[i]] <- c(j) else na.index[[i]] <- c(na.index[[i]],j)
      }
    }
  }
  return(na.index)
}
#Function that computes accuracy/RMSE for imputing numeric/categorical variables of a data frame
compute.acc.RMSE <- function(list0.1,list0.2,list0.3) {
  bias.list0.1 <- list()
  bias.list0.2 <- list()
  bias.list0.3 <- list()
  m <- length(list0.1)
  for (i in c(1:m)) {
    acc0.1 <- c()
    acc0.2 <- c()
    acc0.3 <- c()
    for (j in c(2,3,6,7,9,11,13)) {
      acc0.1 <- c(acc0.1,sum(list0.1[[i]][na.index0.1[[i]][[j]],j]==cleveland.df[na.index0.1[[i]][[j]],j])
                  /length(na.index0.1[[i]][[j]]))
      acc0.2 <- c(acc0.2,sum(list0.2[[i]][na.index0.2[[i]][[j]],j]==cleveland.df[na.index0.2[[i]][[j]],j])
                  /length(na.index0.2[[i]][[j]]))
      acc0.3 <- c(acc0.3,sum(list0.3[[i]][na.index0.3[[i]][[j]],j]==cleveland.df[na.index0.3[[i]][[j]],j])
                  /length(na.index0.3[[i]][[j]]))
    }
    acc0.1 <- rbind(acc0.1)
    acc0.2 <- rbind(acc0.2)
    acc0.3 <- rbind(acc0.3)
    colnames(acc0.1) <- names(cleveland.df)[c(2,3,6,7,9,11,13)]
    colnames(acc0.2) <- names(cleveland.df)[c(2,3,6,7,9,11,13)]
    colnames(acc0.3) <- names(cleveland.df)[c(2,3,6,7,9,11,13)]
    RMSE0.1 <- c()
    RMSE0.2 <- c()
    RMSE0.3 <- c()
    
    for (j in c(1,4,5,8,10,12)) {
      RMSE0.1 <- c(RMSE0.1,accuracy(list0.1[[i]][na.index0.1[[i]][[j]],j],cleveland.df[na.index0.1[[i]][[j]],j])[2])
      RMSE0.2 <- c(RMSE0.2,accuracy(list0.2[[i]][na.index0.2[[i]][[j]],j],cleveland.df[na.index0.2[[i]][[j]],j])[2])
      RMSE0.3 <- c(RMSE0.3,accuracy(list0.3[[i]][na.index0.3[[i]][[j]],j],cleveland.df[na.index0.3[[i]][[j]],j])[2])
    }
    
    RMSE0.1 <- rbind(RMSE0.1)
    RMSE0.2 <- rbind(RMSE0.2)
    RMSE0.3 <- rbind(RMSE0.3)
    colnames(RMSE0.1) <- names(cleveland.df)[c(1,4,5,8,10,12)]
    colnames(RMSE0.2) <- names(cleveland.df)[c(1,4,5,8,10,12)]
    colnames(RMSE0.3) <- names(cleveland.df)[c(1,4,5,8,10,12)]
    bias.list0.1[[i]] <- list(acc0.1,RMSE0.1)
    bias.list0.2[[i]] <- list(acc0.2,RMSE0.2)
    bias.list0.3[[i]] <- list(acc0.3,RMSE0.3)
    
  }
  mean0.1.bias <- bias.list0.1[[1]]
  mean0.2.bias <- bias.list0.2[[1]]
  mean0.3.bias <- bias.list0.3[[1]]
  for (i in c(1:7)) {
    temp0.1 <- c()
    temp0.2 <- c()
    temp0.3 <- c()
    for (j in c(1:m)) {
      temp0.1 <- c(temp0.1,bias.list0.1[[j]][[1]][,i])
      temp0.2 <- c(temp0.2,bias.list0.2[[j]][[1]][,i])
      temp0.3 <- c(temp0.3,bias.list0.3[[j]][[1]][,i])
    }
    mean0.1.bias[[1]][,i] <- mean(temp0.1)
    mean0.2.bias[[1]][,i] <- mean(temp0.2)
    mean0.3.bias[[1]][,i] <- mean(temp0.3)
  }
  for (i in c(1:6)) {
    temp0.1 <- c()
    temp0.2 <- c()
    temp0.3 <- c()    
    for (j in c(1:m)) {
      temp0.1 <- c(temp0.1,bias.list0.1[[j]][[2]][,i])
      temp0.2 <- c(temp0.2,bias.list0.2[[j]][[2]][,i])
      temp0.3 <- c(temp0.3,bias.list0.3[[j]][[2]][,i])
    }
    mean0.1.bias[[2]][,i] <- mean(temp0.1)
    mean0.2.bias[[2]][,i] <- mean(temp0.2)
    mean0.3.bias[[2]][,i] <- mean(temp0.3) 
  }
  res <- list(mean0.1.bias,mean0.2.bias,mean0.3.bias)
  return(res)
}
##Functions for running predictive models and returns it's accuracy,f-score,roc AUC
#Function for CART
CART.model <- function(train.df,valid.df){
  tr <- ctree(des ~ ., data = train.df)
  pred <- predict(tr, newdata = valid.df)
  accuracy <- sum(pred==valid.df$des)/nrow(valid.df)
  f_score <- F_meas(pred,valid.df[,14])
  r <- roc(pred, valid.df$des)
  roc_AUC <- auc(r)
  res <- c(accuracy,roc_AUC,f_score)
  return(res)
}
#Function for random forests
rf.model <- function(train.df,valid.df){
  rf <- cforest(des ~ ., data = train.df)
  pred <- predict(rf, newdata = valid.df)
  accuracy <- sum(pred==valid.df$des)/nrow(valid.df)
  f_score <- F_meas(pred,valid.df[,14])
  r <- roc(pred, valid.df$des)
  roc_AUC <- auc(r)
  res <- c(accuracy,roc_AUC,f_score)
  return(res)
}
#Function for nn
nn.model <- function(train.df,valid.df){
  set.seed(1)
  nn <- nnet(des ~ ., data = train.df,
             linout = F,
             size = 1, decay = 2.4,
             maxit = 600)
  pred <- predict(nn, newdata = valid.df)
  pred <- as.factor(ifelse(pred>=0.5,1,0))
  accuracy <- sum(pred==valid.df$des)/nrow(valid.df)
  f_score <- F_meas(pred,valid.df[,14])
  r <- roc(pred, valid.df$des)
  roc_AUC <- auc(r)
  res <- c(accuracy,roc_AUC,f_score)
  return(res)
}

length(unique(cleveland.df$cp))
#Function for logistic regression
lr.model <- function(train.df,valid.df){
  #if(as.factor(1) %in% train.df$restecg){
  bool <- TRUE
  for (i in c(2,3,6,7,9,11,13)) {
    if(length(unique(train.df[,i]))!=length(unique(valid.df[,i])))
      bool<-FALSE
  }
  if(bool){
    reg <- glm(des ~ ., data = train.df,
               family = "binomial")
    pred <- predict(reg, newdata = valid.df)
    pred <- as.factor(ifelse(pred>=0.5,1,0))
    accuracy <- sum(pred==valid.df$des)/nrow(valid.df)
    f_score <- F_meas(pred,valid.df[,14])
    r <- roc(pred, valid.df$des)
    roc_AUC <- auc(r)
    res <- c(accuracy,roc_AUC,f_score)
  } else{
    set.seed(1)
    nn <- nnet(des ~ ., data = train.df,
               linout = F,
               size = 1, decay = 2.4,
               maxit = 600)
    pred <- predict(nn, newdata = valid.df)
    pred <- as.factor(ifelse(pred>=0.5,1,0))
    accuracy <- sum(pred==valid.df$des)/nrow(valid.df)
    f_score <- F_meas(pred,valid.df[,14])
    r <- roc(pred, valid.df$des)
    roc_AUC <- auc(r)
    res <- c(accuracy,roc_AUC,f_score)
  }
  return(res)
}
#Function for n times call predictive models and returns their performance measures
get.predict.results <- function(df,n) {
  temp <- data.frame(accuracy = integer(4*n), AUC = integer(4*n), f_score = integer(4*n))
  for (i in c(1:n)) {
    set.seed(i)
    trainIndex <- sample(c(1:nrow(df)), nrow(df)*0.6)
    train.df <- df[trainIndex, ]
    valid.df <- df[-trainIndex, ]
    temp[i,] <- CART.model(train.df, valid.df)
    temp[n+i,] <- rf.model(train.df, valid.df)
    temp[2*n+i,] <- nn.model(train.df,valid.df)
    temp[3*n+i,] <- lr.model(train.df,valid.df)
  }
  #CART.acc <- temp[1:n,1]
  #CART.AUC <- temp[1:n,2]
  #CART.f <- temp[1:n,3]
  #rf.acc <- temp[(n+1):(2*n),1]
  #rf.AUC <- temp[(n+1):(2*n),2]
  #rf.f <- temp[(n+1):(2*n),3]
  #nn.acc <- temp[(2*n+1):(3*n),1]
  #nn.AUC <- temp[(2*n+1):(3*n),2]
  #nn.f <- temp[(2*n+1):(3*n),3]
  #lr.acc <- temp[(3*n+1):(4*n),1]
  #lr.AUC <- temp[(3*n+1):(4*n),2]
  #lr.f <- temp[(3*n+1):(4*n),3]
  #export<-data.frame(CART.acc,CART.AUC,CART.f,rf.acc,rf.AUC,rf.f,nn.acc,nn.AUC,nn.f,lr.acc,lr.AUC,lr.f)
  #write.csv(export,paste(removePunctuation(as.character(Sys.time())),".csv"))
  CART <- (c(mean(temp[1:n,1]),mean(temp[1:n,2]),mean(temp[1:n,3])))
  #CART.sd <- (c(sd(temp[1:n,1]),sd(temp[1:n,2]),sd(temp[1:n,3])))
  rf <- (c(mean(temp[(n+1):(2*n),1]),mean(temp[(n+1):(2*n),2]),mean(temp[(n+1):(2*n),3])))
  #rf.sd <- (c(sd(temp[n:(2*n),1]),sd(temp[n:(2*n),2]),sd(temp[n:(2*n),3])))
  
  nn <- (c(mean(temp[(2*n+1):(3*n),1]),mean(temp[(2*n+1):(3*n),2]),mean(temp[(2*n+1):(3*n),3])))
  #nn.sd <- (c(sd(temp[(2*n):(3*n),1]),sd(temp[(2*n):(3*n),2]),sd(temp[(2*n):(3*n),3])))
  
  lr <- (c(mean(temp[(3*n+1):(4*n),1]),mean(temp[(3*n+1):(4*n),2]),mean(temp[(3*n+1):(4*n),3])))
  #lr.sd <- (c(sd(temp[(3*n):(4*n),1]),sd(temp[(3*n):(4*n),2]),sd(temp[(3*n):(4*n),3])))
  res <- data.frame(CART,rf,nn,lr)
  row.names(res) <- c("accuracy","AUC","f-score")
  return(res)
}

##Functions - imputation
#Function for mean imputation for numeric, mode imputation for categorical
mean.mode.impute <- function(df) {
  for (i in c(1:(ncol(df)))) {
    if (class(df[,i]) == "factor") {
      val <- unique(df[!is.na(df[,i]),i])              
      mode <- val[which.max(tabulate(match(df[,i], val)))]
      df[is.na(df[,i]),i] <- mode
    } else if (class(df[,i]) == "integer") {
      df[,i] <- as.integer(round(ifelse(is.na(df[,i]),mean(df[,i],na.rm = TRUE),df[,i])))
    } else if (class(df[,i]) == "numeric") {
      df[,i] <- round(ifelse(is.na(df[,i]),mean(df[,i],na.rm = TRUE),df[,i]),digits = 1)
    }
  }
  return(df)
}

#Function for median imputation for numeric, mode imputation for categorical
median.mode.impute <- function(df) {
  for (i in c(1:(ncol(df)))) {
    if (class(df[,i]) == "factor") {
      val <- unique(df[!is.na(df[,i]),i])              
      mode <- val[which.max(tabulate(match(df[,i], val)))]
      df[is.na(df[,i]),i] <- mode
    } else if (class(df[,i]) == "integer") {
      df[,i] <- as.integer(round(ifelse(is.na(df[,i]),median(df[,i],na.rm = TRUE),df[,i])))
    } else if (class(df[,i]) == "numeric") {
      df[,i] <- round(ifelse(is.na(df[,i]),median(df[,i],na.rm = TRUE),df[,i]),digits = 1)
    }
  }
  return(df)
}

#Function for nn imputation - algorithm 1
nn.impute <- function(df) {
  vars <- df[,-14]
  na.index <- na.indexes(vars)
  grades <- missing.values.grades(vars)
  vars.imputed <- mean.mode.impute(vars)
  for (i in order(grades)) {
    sample.from <- c()
    if(grades[i] <= 0.4){
      if(class(na.index[[i]])=="character"){
        sample.from <- c(1:nrow(vars.imputed))
      } else{
        sample.from <- c(1:nrow(vars.imputed))[-na.index[[i]]]
      }
      trainIndex <- sample(sample.from, nrow(vars.imputed)*0.6)
    } else {
      sample.from <- c(1:nrow(vars.imputed))[-na.index[[i]]]
      trainIndex <- c(sample.from, sample(c(1:nrow(vars.imputed))[na.index[[i]]],
                                          (nrow(vars.imputed)*0.6 - length(sample.from))))
    }
    train.df <- vars.imputed[trainIndex, ]
    valid.df <- vars.imputed[-trainIndex, ]
    #NN model
    set.seed(1)
    is.num <- ifelse(class(vars.imputed[,i])=="integer"||class(vars.imputed[,i])=="numeric",T,F)
    nn <- nnet(as.formula(paste(names(vars.imputed)[i]," ~ .")), data = train.df,
               linout = is.num,
               size = 1, decay = 2.4,
               maxit = 600)
    pred1 <- predict(nn, newdata = valid.df)
    validIndex <- c(1:nrow(cleveland.df))[-trainIndex]
    if(class(vars.imputed[,i])=="factor"){
      if(length(unique(vars.imputed[,i])) == 2){
        pred1 <- as.factor(ifelse(pred1>=0.5,1,0))
        for (j in c(1:nrow(valid.df))) {
          if(is.element(validIndex[j],na.index[[i]])){
            vars.imputed[validIndex[j],i] <- pred1[j]
          }
        }
      } else{
        pred1 <- as.factor(colnames(pred1)[max.col(pred1)])
        for (j in c(1:nrow(valid.df))) {
          if(is.element(validIndex[j],na.index[[i]])){
            temp <- validIndex[j]
            vars.imputed[temp,i] <- pred1[j]
          }
        }
      }
    } else if(class(vars.imputed[,i])=="integer"){
      for (j in c(1:nrow(valid.df))) {
        vars.imputed[validIndex[j],i] <- ifelse(is.element(validIndex[j],na.index[[i]]),
                                                as.integer(round(pred1[j])),
                                                vars.imputed[validIndex[j],i])
      }
    } else if(class(vars.imputed[,i])=="numeric"){
      for (j in c(1:nrow(valid.df))) {
        vars.imputed[validIndex[j],i] <- ifelse(is.element(validIndex[j],na.index[[i]]),
                                                round(pred1[j],digits = 1),
                                                vars.imputed[validIndex[j],i])
      }
    }
  }
  df[,-14] <- vars.imputed
  return(df)
}
#Function for rf imputation - algorithm 1
rf.impute <- function(df) {
  vars <- df[,-14]
  na.index <- na.indexes(vars)
  grades <- missing.values.grades(vars)
  vars.imputed <- mean.mode.impute(vars)
  for (i in order(grades)) {
    sample.from <- c()
    if(grades[i] <= 0.4){
      if(class(na.index[[i]])=="character"){
        sample.from <- c(1:nrow(vars.imputed))
      } else{
        sample.from <- c(1:nrow(vars.imputed))[-na.index[[i]]]
      }
      trainIndex <- sample(sample.from, nrow(vars.imputed)*0.6)
    } else {
      sample.from <- c(1:nrow(vars.imputed))[-na.index[[i]]]
      trainIndex <- c(sample.from, sample(c(1:nrow(vars.imputed))[na.index[[i]]],
                                          (nrow(vars.imputed)*0.6 - length(sample.from))))
    }
    train.df <- vars.imputed[trainIndex, ]
    valid.df <- vars.imputed[-trainIndex, ]
    #rf model
    is.num <- ifelse(class(vars.imputed[,i])=="integer"||class(vars.imputed[,i])=="numeric",T,F)
    rf <- cforest(as.formula(paste(names(vars.imputed)[i]," ~ .")), data = train.df)
    pred1 <- predict(rf, newdata = valid.df)
    validIndex <- c(1:nrow(cleveland.df))[-trainIndex]
    if(class(vars.imputed[,i])=="factor"){
      if(length(unique(vars.imputed[,i])) == 2){
        for (j in c(1:nrow(valid.df))) {
          if(is.element(validIndex[j],na.index[[i]])){
            vars.imputed[validIndex[j],i] <- pred1[j]
          }
        }
      } else{
        for (j in c(1:nrow(valid.df))) {
          if(is.element(validIndex[j],na.index[[i]])){
            vars.imputed[validIndex[j],i] <- pred1[j]
          }
        }
      }
    } else if(class(vars.imputed[,i])=="integer"){
      for (j in c(1:nrow(valid.df))) {
        vars.imputed[validIndex[j],i] <- ifelse(is.element(validIndex[j],na.index[[i]]),
                                                as.integer(round(pred1[j])),
                                                vars.imputed[validIndex[j],i])
      }
    } else if(class(vars.imputed[,i])=="numeric"){
      for (j in c(1:nrow(valid.df))) {
        vars.imputed[validIndex[j],i] <- ifelse(is.element(validIndex[j],na.index[[i]]),
                                                round(pred1[j],digits = 1),
                                                vars.imputed[validIndex[j],i])
      }
    }
  }
  df[,-14] <- vars.imputed
  return(df)
}
##############################################Start the experiment code##############################################
#Read file
cleveland.df <- read.csv("cleveland.csv")

##Preprocess
#Nominal variables into factorial
for (i in c(2,3,6,7,9,11,13)) {
  cleveland.df[,i] <- as.factor(cleveland.df[,i])
}
#Transform ca to integer
cleveland.df$ca <- as.integer(cleveland.df$ca)
#Target attribute into factorial 0 (no heart disease) 1 (any heart disease presence)
cleveland.df$des <- as.factor(ifelse(cleveland.df$des == 0, 0, 1))

#List of m randomly removed values datasets (10,20,30 %)
m <- 40
vars <- cleveland.df[,-14]
datasets0.1 <- list()
datasets0.1 <- foreach(seed = 1:m, 
                    .packages = "forecast") %dopar% insertNA(vars,0.1, seed)
datasets0.2 <- list()
datasets0.2 <- foreach(seed = 1:m, 
                       .packages = "forecast") %dopar% insertNA(vars,0.2, seed)
datasets0.3 <- list()
datasets0.3 <- foreach(seed = 1:m, 
                       .packages = "forecast") %dopar% insertNA(vars,0.3, seed)
#Indexes of NAs in every data frame
na.index0.1 <- list()
na.index0.2 <- list()
na.index0.3 <- list()

for (i in c(1:m)) {
  na.index0.1[[i]] <- na.indexes(datasets0.1[[i]])
  na.index0.2[[i]] <- na.indexes(datasets0.2[[i]])
  na.index0.3[[i]] <- na.indexes(datasets0.3[[i]])
}

for (i in c(1:m)) {
  datasets0.1[[i]]$des <- cleveland.df[,14]
  datasets0.2[[i]]$des <- cleveland.df[,14]
  datasets0.3[[i]]$des <- cleveland.df[,14]
}

##Impute datasets
mean.imputed0.1 <- list()
mean.imputed0.2 <- list()
mean.imputed0.3 <- list()

median.imputed0.1 <- list()
median.imputed0.2 <- list()
median.imputed0.3 <- list()

nn.imputed0.1 <- list()
nn.imputed0.2 <- list()
nn.imputed0.3 <- list()

rf.imputed0.1 <- list()
rf.imputed0.2 <- list()
rf.imputed0.3 <- list()

#Mean imputation
mean.imputed0.1 <- foreach(i = 1:m) %dopar% mean.mode.impute(datasets0.1[[i]])
mean.imputed0.2 <- foreach(i = 1:m) %dopar% mean.mode.impute(datasets0.2[[i]])
mean.imputed0.3 <- foreach(i = 1:m) %dopar% mean.mode.impute(datasets0.3[[i]])

#Median imputation
median.imputed0.1 <- foreach(i = 1:m) %dopar% median.mode.impute(datasets0.1[[i]])
median.imputed0.2 <- foreach(i = 1:m) %dopar% median.mode.impute(datasets0.2[[i]])
median.imputed0.3 <- foreach(i = 1:m) %dopar% median.mode.impute(datasets0.3[[i]])

#NN imputation
nn.imputed0.1 <- foreach(i = 1:m,
                         .packages = c("forecast","party","AUC","nnet")) %dopar% nn.impute(datasets0.1[[i]])
nn.imputed0.2 <- foreach(i = 1:m,
                         .packages = c("forecast","party","AUC","nnet")) %dopar% nn.impute(datasets0.2[[i]])
nn.imputed0.3 <- foreach(i = 1:m,
                         .packages = c("forecast","party","AUC","nnet")) %dopar% nn.impute(datasets0.3[[i]])
#RF imputation
rf.imputed0.1 <- foreach(i = 1:m,
                         .packages = c("forecast","party","AUC","nnet")) %dopar% rf.impute(datasets0.1[[i]])
rf.imputed0.2 <- foreach(i = 1:m,
                         .packages = c("forecast","party","AUC","nnet")) %dopar% rf.impute(datasets0.2[[i]])
rf.imputed0.3 <- foreach(i = 1:m,
                         .packages = c("forecast","party","AUC","nnet")) %dopar% rf.impute(datasets0.3[[i]])
##Review Algorithms' performance
#Accuracy for imputing nominal attributes, RMSE for imputing numeric attributes
mean.mode.acc.RMSE <- compute.acc.RMSE(mean.imputed0.1,mean.imputed0.2,mean.imputed0.3)
median.mode.acc.RMSE <- compute.acc.RMSE(median.imputed0.1,median.imputed0.2,median.imputed0.3)
nn.acc.RMSE <- compute.acc.RMSE(nn.imputed0.1,nn.imputed0.2,nn.imputed0.3)
rf.acc.RMSE <- compute.acc.RMSE(rf.imputed0.1,rf.imputed0.2,rf.imputed0.3)


#Retrieve predictive models' performance
mean0.1.list <- list()
mean0.1.list <- foreach(seed = 1:m, 
                        .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(mean.imputed0.1[[i]],40)
mean0.2.list <- list()
mean0.2.list <- foreach(seed = 1:m, 
                       .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(mean.imputed0.2[[i]],40)
mean0.3.list <- list()
mean0.3.list <- foreach(seed = 1:m, 
                       .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(mean.imputed0.3[[i]],40)
median0.1.list <- list()
median0.1.list <- foreach(seed = 1:m, 
                        .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(median.imputed0.1[[i]],40)
median0.2.list <- list()
median0.2.list <- foreach(seed = 1:m, 
                        .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(median.imputed0.2[[i]],40)
median0.3.list <- list()
median0.3.list <- foreach(seed = 1:m, 
                        .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(median.imputed0.3[[i]],40)
nn0.1.list <- list()
nn0.1.list <- foreach(seed = 1:m, 
                        .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(nn.imputed0.1[[i]],40)
nn0.2.list <- list()
nn0.2.list <- foreach(seed = 1:m, 
                        .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(nn.imputed0.2[[i]],40)
nn0.3.list <- list()
nn0.3.list <- foreach(seed = 1:m, 
                        .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(nn.imputed0.3[[i]],40)
rf0.1.list <- list()
rf0.1.list <- foreach(seed = 1:m, 
                      .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(rf.imputed0.1[[i]],40)
rf0.2.list <- list()
rf0.2.list <- foreach(seed = 1:m, 
                      .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(rf.imputed0.2[[i]],40)
rf0.3.list <- list()
rf0.3.list <- foreach(seed = 1:m, 
                      .packages = c("forecast","party","AUC","nnet","caret","tm")) %dopar% get.predict.results(rf.imputed0.3[[i]],40)
org.res <- get.predict.results(cleveland.df,m)
mean0.1.res <- mean0.1.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,mean0.1.list[[k]][j,i])
    }
    mean0.1.res[j,i] <- mean(temp)
  }
}
mean0.2.res <- mean0.2.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,mean0.2.list[[k]][j,i])
    }
    mean0.2.res[j,i] <- mean(temp)
  }
}
mean0.3.res <- mean0.3.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,mean0.3.list[[k]][j,i])
    }
    mean0.3.res[j,i] <- mean(temp)
  }
}
median0.1.res <- median0.1.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,median0.1.list[[k]][j,i])
    }
    median0.1.res[j,i] <- median(temp)
  }
}
median0.2.res <- median0.2.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,median0.2.list[[k]][j,i])
    }
    median0.2.res[j,i] <- median(temp)
  }
}
median0.3.res <- median0.3.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,median0.3.list[[k]][j,i])
    }
    median0.3.res[j,i] <- median(temp)
  }
}
nn0.1.res <- nn0.1.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,nn0.1.list[[k]][j,i])
    }
    nn0.1.res[j,i] <- mean(temp)
  }
}
nn0.2.res <- nn0.2.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,nn0.2.list[[k]][j,i])
    }
    nn0.2.res[j,i] <- mean(temp)
  }
}
nn0.3.res <- nn0.3.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,nn0.3.list[[k]][j,i])
    }
    nn0.3.res[j,i] <- mean(temp)
  }
}
rf0.1.res <- rf0.1.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,rf0.1.list[[k]][j,i])
    }
    rf0.1.res[j,i] <- mean(temp)
  }
}
rf0.2.res <- rf0.2.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,rf0.2.list[[k]][j,i])
    }
    rf0.2.res[j,i] <- mean(temp)
  }
}
rf0.3.res <- rf0.3.list[[1]]
for (i in c(1:4)) {
  for (j in c(1:3)) {
    temp <- c()
    for (k in c(1:m)) {
      temp <- c(temp,rf0.3.list[[k]][j,i])
    }
    rf0.3.res[j,i] <- mean(temp)
  }
}

##Impute after splitting data to train validation
datasets.train0.3 <- list()
datasets.valid0.3 <- list()
set.seed(1)
trainIndex <- sample(c(1:nrow(datasets0.3[[1]])), nrow(datasets0.3[[1]])*0.6)
for (i in c(1:m)) {
  datasets.train0.3[[i]] <- datasets0.3[[i]][trainIndex, ]
  datasets.valid0.3[[i]] <- datasets0.3[[i]][-trainIndex, ]
}

rf.train.imputed0.3 <- foreach(i = 1:m,
                         .packages = c("forecast","party","AUC","nnet")) %dopar% rf.impute(datasets.train0.3[[i]])
rf.valid.imputed0.3 <- foreach(i = 1:m,
                               .packages = c("forecast","party","AUC","nnet")) %dopar% rf.impute(datasets.valid0.3[[i]])
temp <- data.frame(accuracy = integer(4*m), f_score = integer(4*m), AUC = integer(4*m))
for (i in c(1:m)) {
  train.df <- rf.train.imputed0.3[[i]]
  valid.df <- rf.valid.imputed0.3[[i]]
  temp[i,] <- CART.model(train.df, valid.df)
  temp[m+i,] <- rf.model(train.df, valid.df)
  temp[2*m+i,] <- nn.model(train.df,valid.df)
  temp[3*m+i,] <- lr.model(train.df,valid.df)
}
CART.res <- (c(mean(temp[1:m,1]),mean(temp[1:m,2]),mean(temp[1:m,3])))
rf.res <- (c(mean(temp[m:(2*m),1]),mean(temp[m:(2*m),2]),mean(temp[m:(2*m),3])))
nn.res <- (c(mean(temp[(2*m):(3*m),1]),mean(temp[(2*m):(3*m),2]),mean(temp[(2*m):(3*m),3])))
lr.res <- (c(mean(temp[(3*m):(4*m),1]),mean(temp[(3*m):(4*m),2]),mean(temp[(3*m):(4*m),3])))
res <- data.frame(CART.res,rf.res,nn.res,lr.res)
row.names(res) <- c("accuracy","AUC","f-score")

#####results
org.res
mean0.1.res
mean0.2.res
mean0.3.res
median0.1.res
median0.2.res
median0.3.res
nn0.1.res
nn0.2.res
nn0.3.res
rf0.1.res
rf0.2.res
rf0.3.res
res
