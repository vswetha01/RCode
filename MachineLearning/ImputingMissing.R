
library(ggplot2)
library(reshape2)


NAinsert <- function(df, prop = .1){
  n <- nrow(df)
  m <- ncol(df)
  num.to.na <- ceiling(prop*n*m)
  id <- sample(0:(m*n-1), num.to.na, replace = FALSE)
  rows <- id %/% m + 1
  cols <- id %% m + 1
  sapply(seq(num.to.na), function(x){
    df[rows[x], cols[x]] <<- NA
  }
  )
  return(df)
}

plot_imputation <- function(x, mean_y, knn_y, rf_y, str){
  df <- data.frame(x, mean_y, knn_y, rf_y)
  df2 <- melt(data = df, id.vars = "x")
  ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line() +
    labs(x="Percentatge of examples with Missing Values", y="Error Rate") + 
    ggtitle(str)
  
}
#####Function to impute missing with the mean value######
meanImputation <- function(df){
  df[is.na(df)] <- mean(df, na.rm=TRUE)
  
  return (df)
}
#######Function to insert NA in each column######
Vector_InsertNA <- function(input_array, prob=0.1){
  num_of_na <- floor(length(input_array) * prob)
  set.seed(100)
  var_index <- sample(1:length(input_array), num_of_na)
  for (i in var_index){
    input_array[i] <- NA
  }
  return (input_array)
}

#####Column2 and Column4 Missing data########
Bupa_Column_2_4 <- function(bupa_train_dt, bupa_test_dt){
  x <- c(0.10,0.20,0.30,0.40,0.60)
  mean_y<-NULL
  knn_y <-NULL
  rf_y <- NULL
  
  for(p in x){
  
    Col5_NA <- Vector_InsertNA(bupa_train_dt$V5, p)
    Col3_NA <- Vector_InsertNA(bupa_train_dt$V3, p)
    impute_df <- bupa_train_dt[,-7]
    impute_df$V5 <- Col5_NA
    impute_df$V3 <- Col3_NA
    ####DO mean imputation#####
    mean_df <- bupa_train_dt
    mean_df$V5 <- meanImputation(impute_df$V5)
    mean_df$V3 <- meanImputation(impute_df$V3)
    ###Apply to RandomForest
    mean_rf <- randomForest(mean_df$V7~., data=mean_df, mtry=2, importance=TRUE, ntree=500)
    mean_rf.pred <- predict(mean_rf, newdata = bupa_test_dt, type="class")
    table(mean_rf.pred, bupa_test_dt$V7)
    ###Calculate the error rate
    mn <- mean(mean_rf.pred == bupa_test_dt$V7)
    mean_y <- append(mean_y, mn)
    ####DO KNN IMPUTATION#######
    
    knn_impute_bupa_col_2 <- knnImputation(impute_df, k=10, scale=F, meth="median")
    all_knn<- knn_impute_bupa_col_2
    all_knn$V7 <- bupa_train_dt$V7
    knn_rf <- randomForest(all_knn$V7~., data=all_knn, mtry=2, importance=TRUE, ntree=500)
    knn_rf.pred <- predict(knn_rf, newdata = bupa_test_dt, type="class")
    table(knn_rf.pred, bupa_test_dt$V7)
    knn_mn <- mean(knn_rf.pred == bupa_test_dt$V7)
    
    knn_y <- append(knn_y, knn_mn)
    
    bupa_train_dt$V5 <- Col5_NA
    bupa_train_dt$V3 <- Col3_NA
    rf <- randomForest(bupa_train_dt$V7~., data=bupa_train_dt, ntree=500, na.action = na.omit)
    rf.pred <- predict(rf, newdata = bupa_test_dt, type="class")
    rf_mn <- mean(rf.pred == bupa_test_dt$V7)
    rf_y<-append(rf_y, rf_mn)
  }
  plot_imputation(x, mean_y, knn_y, rf_y, "Bupa: Missing Data Artificially Inserted into Attributes 2,4.")
  
}

#####Column_2_Missing_Data########
Bupa_Column_2 <- function(bupa_train_dt, bupa_test_dt){
  x <- c(0.10,0.20,0.30,0.40,0.60)
  mean_y<-NULL
  knn_y <-NULL
  rf_y <- NULL
  
  for(p in x){
    Col3_NA <- Vector_InsertNA(bupa_train_dt$V3, p)
    impute_df <- bupa_train_dt[,-7]
    impute_df$V3 <- Col3_NA
    ####DO mean imputation#####
    mean_df <- bupa_train_dt
    mean_df$V3 <- meanImputation(impute_df$V3)
    ###Apply to RandomForest
    mean_rf <- randomForest(mean_df$V7~., data=mean_df, mtry=2, importance=TRUE, ntree=500)
    mean_rf.pred <- predict(mean_rf, newdata = bupa_test_dt, type="class")
    table(mean_rf.pred, bupa_test_dt$V7)
    ###Calculate the error rate
    mn <- mean(mean_rf.pred == bupa_test_dt$V7)
    mean_y <- append(mean_y, mn)
    ####DO KNN IMPUTATION#######
    knn_impute_bupa_col_2 <- knnImputation(impute_df, k=10, scale=F, meth="median")
    all_knn<- knn_impute_bupa_col_2
    all_knn$V7 <- bupa_train_dt$V7
    knn_rf <- randomForest(all_knn$V7~., data=all_knn, mtry=2, importance=TRUE, ntree=500)
    knn_rf.pred <- predict(knn_rf, newdata = bupa_test_dt, type="class")
    table(knn_rf.pred, bupa_test_dt$V7)
    knn_mn <- mean(knn_rf.pred == bupa_test_dt$V7)
    
    knn_y <- append(knn_y, knn_mn)
    
    bupa_train_dt$V3 <- Col3_NA
    rf <- randomForest(bupa_train_dt$V7~., data=bupa_train_dt, ntree=500, na.action = na.omit)
    rf.pred <- predict(rf, newdata = bupa_test_dt, type="class")
    rf_mn <- mean(rf.pred == bupa_test_dt$V7)
    rf_y<-append(rf_y, rf_mn)
  }
  plot_imputation(x, mean_y, knn_y, rf_y, "Bupa: Missing Data Artificially Inserted into Attributes 2.")
}

#####Column_2_4_5_Missing_Data########
Bupa_Column_2_4_5 <- function(bupa_train_dt, bupa_test_dt){
  x <- c(0.10,0.20,0.30,0.40,0.60)
  mean_y<-NULL
  knn_y <-NULL
  rf_y <- NULL
  
  for(p in x){
    Col6_NA <- Vector_InsertNA(bupa_train_dt$V6, p)
    Col5_NA <- Vector_InsertNA(bupa_train_dt$V5, p)
    Col3_NA <- Vector_InsertNA(bupa_train_dt$V3, p)
    impute_df <- bupa_train_dt[,-7]
    impute_df$V6 <- Col6_NA
    impute_df$V5 <- Col5_NA
    impute_df$V3 <- Col3_NA
    ####Mean imputation#####
    mean_df <- bupa_train_dt
    mean_df$V5 <- meanImputation(impute_df$V5)
    mean_df$V3 <- meanImputation(impute_df$V3)
    mean_df$V6 <- meanImputation(impute_df$V6)
    ###Apply to RandomForest
    mean_rf <- randomForest(mean_df$V7~., data=mean_df, mtry=2, importance=TRUE, ntree=500)
    mean_rf.pred <- predict(mean_rf, newdata = bupa_test_dt, type="class")
    table(mean_rf.pred, bupa_test_dt$V7)
    ###Calculate the error rate
    mn <- mean(mean_rf.pred == bupa_test_dt$V7)
    mean_y <- append(mean_y, mn)
    
    ####KNN IMPUTATION#######
    knn_impute_bupa_col_2 <- knnImputation(impute_df, k=10, scale=F, meth="median")
    all_knn<- knn_impute_bupa_col_2
    all_knn$V7 <- bupa_train_dt$V7
    knn_rf <- randomForest(all_knn$V7~., data=all_knn, mtry=2, importance=TRUE, ntree=500)
    knn_rf.pred <- predict(knn_rf, newdata = bupa_test_dt, type="class")
    #Error rate calculate for knn imputation
    knn_mn <- mean(knn_rf.pred == bupa_test_dt$V7)
    knn_y <- append(knn_y, knn_mn)
    
    ####Random Forest ########
    bupa_train_dt$V5 <- Col5_NA
    bupa_train_dt$V3 <- Col3_NA
    bupa_train_dt$V6 <- Col6_NA
    rf <- randomForest(bupa_train_dt$V7~., data=bupa_train_dt, ntree=500, na.action = na.omit)
    rf.pred <- predict(rf, newdata = bupa_test_dt, type="class")
    rf_mn <- mean(rf.pred == bupa_test_dt$V7)
    rf_y<-append(rf_y, rf_mn)
  }
  plot_imputation(x, mean_y, knn_y, rf_y, "Bupa: Missing Data Artificially Inserted into Attributes 2,4,5.")
  
}
#####Column_0_Missing_Data########
Pima_Column_0 <- function(pima_train_dt, pima_test_dt){
  x <- c(0.10,0.20,0.30,0.40,0.60)
  mean_y<-NULL
  knn_y <-NULL
  rf_y <- NULL
  
  for(p in x){
    Col1_NA <- Vector_InsertNA(pima_train_dt[,1], p)
    impute_df <- pima_train_dt[,-9]
    impute_df$V1 <- Col1_NA
    ####DO mean imputation#####
    mean_df <- pima_train_dt
    mean_df$V1 <- meanImputation(impute_df$V1)
    ###Apply to RandomForest
    mean_rf <- randomForest(mean_df$V9~., data=mean_df, mtry=2, importance=TRUE, ntree=500)
    mean_rf.pred <- predict(mean_rf, newdata = pima_test_dt, type="class")
    table(mean_rf.pred, pima_test_dt$V9)
    ###Calculate the error rate
    mn <- mean(mean_rf.pred == pima_test_dt$V9)
    mean_y <- append(mean_y, mn)
    ####DO KNN IMPUTATION#######
    knn_impute_pima_col_2 <- knnImputation(impute_df, k=10, scale=F, meth="median")
    all_knn<- knn_impute_pima_col_2
    all_knn$V9 <- pima_train_dt$V9
    knn_rf <- randomForest(all_knn$V9~., data=all_knn, mtry=2, importance=TRUE, ntree=500)
    knn_rf.pred <- predict(knn_rf, newdata = pima_test_dt, type="class")
    table(knn_rf.pred, pima_test_dt$V9)
    knn_mn <- mean(knn_rf.pred == pima_test_dt$V9)
    
    knn_y <- append(knn_y, knn_mn)
    
    pima_train_dt$V1 <- Col1_NA
    rf <- randomForest(pima_train_dt$V9~., data=pima_train_dt, ntree=500, na.action = na.omit)
    rf.pred <- predict(rf, newdata = pima_test_dt, type="class")
    rf_mn <- mean(rf.pred == pima_test_dt$V9)
    rf_y<-append(rf_y, rf_mn)
    
  }
  plot_imputation(x, mean_y, knn_y, rf_y, "Pima: Missing Data Artificially Inserted into Attributes 0.")
}

Pima_Column_0_1 <- function(pima_train_dt, pima_test_dt){
  x <- c(0.10,0.20,0.30,0.40,0.60)
  mean_y<-NULL
  knn_y <-NULL
  rf_y <- NULL
  
  for(p in x){
    Col1_NA <- Vector_InsertNA(pima_train_dt[,1], p)
    Col2_NA <- Vector_InsertNA(pima_train_dt[,2], p)
    impute_df <- pima_train_dt[,-9]
    impute_df$V1 <- Col1_NA
    impute_df$V2 <- Col2_NA
    ####DO mean imputation#####
    mean_df <- pima_train_dt
    mean_df$V1 <- meanImputation(impute_df$V1)
    mean_df$V2 <- meanImputation(impute_df$V2)
    ###Apply to RandomForest
    mean_rf <- randomForest(mean_df$V9~., data=mean_df, mtry=2, importance=TRUE, ntree=500)
    mean_rf.pred <- predict(mean_rf, newdata = pima_test_dt, type="class")
    table(mean_rf.pred, pima_test_dt$V9)
    ###Calculate the error rate
    mn <- mean(mean_rf.pred == pima_test_dt$V9)
    mean_y <- append(mean_y, mn)
    ####DO KNN IMPUTATION#######
    knn_impute_pima_col_2 <- knnImputation(impute_df, k=10, scale=F, meth="median")
    all_knn<- knn_impute_pima_col_2
    all_knn$V9 <- pima_train_dt$V9
    knn_rf <- randomForest(all_knn$V9~., data=all_knn, mtry=2, importance=TRUE, ntree=500)
    knn_rf.pred <- predict(knn_rf, newdata = pima_test_dt, type="class")
    table(knn_rf.pred, pima_test_dt$V9)
    knn_mn <- mean(knn_rf.pred == pima_test_dt$V9)
    
    knn_y <- append(knn_y, knn_mn)
    
    pima_train_dt$V1 <- Col1_NA
    pima_train_dt$V2 <- Col2_NA
    rf <- randomForest(pima_train_dt$V9~., data=pima_train_dt, ntree=500, na.action = na.omit)
    rf.pred <- predict(rf, newdata = pima_test_dt, type="class")
    rf_mn <- mean(rf.pred == pima_test_dt$V9)
    rf_y<-append(rf_y, rf_mn)
    
  }
  plot_imputation(x, mean_y, knn_y, rf_y, "Pima: Missing Data Artificially Inserted into Attributes 0,1.")
}

Pima_Column_0_1_5 <- function(pima_train_dt, pima_test_dt){
  x <- c(0.10,0.20,0.30,0.40,0.60)
  mean_y<-NULL
  knn_y <-NULL
  rf_y <- NULL
  
  for(p in x){
    Col1_NA <- Vector_InsertNA(pima_train_dt[,1], p)
    Col2_NA <- Vector_InsertNA(pima_train_dt[,2], p)
    Col6_NA <- Vector_InsertNA(pima_train_dt[,6], p)
    impute_df <- pima_train_dt[,-9]
    impute_df$V1 <- Col1_NA
    impute_df$V2 <- Col2_NA
    impute_df$V6 <- Col6_NA
    ####DO mean imputation#####
    mean_df <- pima_train_dt
    mean_df$V1 <- meanImputation(impute_df$V1)
    mean_df$V2 <- meanImputation(impute_df$V2)
    mean_df$V6 <- meanImputation(impute_df$V6)
    ###Apply to RandomForest
    mean_rf <- randomForest(mean_df$V9~., data=mean_df, mtry=2, importance=TRUE, ntree=500)
    mean_rf.pred <- predict(mean_rf, newdata = pima_test_dt, type="class")
    table(mean_rf.pred, pima_test_dt$V9)
    ###Calculate the error rate
    mn <- mean(mean_rf.pred == pima_test_dt$V9)
    mean_y <- append(mean_y, mn)
    ####DO KNN IMPUTATION#######
    knn_impute_pima_col_2 <- knnImputation(impute_df, k=10, scale=F, meth="median")
    all_knn<- knn_impute_pima_col_2
    all_knn$V9 <- pima_train_dt$V9
    knn_rf <- randomForest(all_knn$V9~., data=all_knn, mtry=2, importance=TRUE, ntree=500)
    knn_rf.pred <- predict(knn_rf, newdata = pima_test_dt, type="class")
    table(knn_rf.pred, pima_test_dt$V9)
    knn_mn <- mean(knn_rf.pred == pima_test_dt$V9)
    
    knn_y <- append(knn_y, knn_mn)
    
    pima_train_dt$V1 <- Col1_NA
    pima_train_dt$V2 <- Col2_NA
    pima_train_dt$V6 <- Col6_NA
    rf <- randomForest(pima_train_dt$V9~., data=pima_train_dt, ntree=500, na.action = na.omit)
    rf.pred <- predict(rf, newdata = pima_test_dt, type="class")
    rf_mn <- mean(rf.pred == pima_test_dt$V9)
    rf_y<-append(rf_y, rf_mn)
    
  }
  plot_imputation(x, mean_y, knn_y, rf_y, "Pima: Missing Data Artificially Inserted into Attributes 0,1,5.")
}


################READING PIMA DATASET##########
read_data_pima <- function(filename){
  pima_dt = read.table(filename, header=FALSE, sep="," )
  
  pima_dt$V9 <- as.factor(pima_dt$V9)
  pima_predictors_dt <- pima_dt[,-9]
  scale_pima_dt <- scale(pima_predictors_dt)
  scale_pima_dt <- as.data.frame(scale_pima_dt)
  scale_pima_dt$V9 <- pima_dt$V9
  return(scale_pima_dt)
  
}
#####READ the BUPA data set##############
read_data_bupa <- function(filename){
  bupa_dt = read.table(filename, header=FALSE, sep="," )
  bupa_dt$V7[which(bupa_dt$V7 == 1)] <-0
  bupa_dt$V7[which(bupa_dt$V7 == 2)] <-1
  bupa_dt$V7 <- as.factor(bupa_dt$V7)
  new_dt <- bupa_dt[,-7]
  scale_bupa_dt <- scale(new_dt)
  scale_bupa_dt <- as.data.frame(scale_bupa_dt)
  scale_bupa_dt$V7 <- bupa_dt$V7
  return(scale_bupa_dt)
}

main_func<- function(){
  filename_pima <- "pima-indians-diabetes.data.txt"
  df_pima <- read_data_pima(filename_pima)
  set.seed(666)
  sample_size <- floor(0.80 * nrow(df_pima))
  train <- sample(seq_len(nrow(df_pima)), size=sample_size)
  pima_train_dt <- df_pima[train,]
  pima_test_dt <- df_pima[-train,]
  
  Pima_Column_0(pima_train_dt, pima_test_dt)
  Pima_Column_0_1(pima_train_dt, pima_test_dt)
  Pima_Column_0_1_5(pima_train_dt, pima_test_dt)
  
  filename_bp <- "bupa.data.txt"
  df_bupa <- read_data_bupa(filename_bp)
  
  set.seed(666)
  sample_size <- floor(0.80 * nrow(df_bupa))
  train <- sample(seq_len(nrow(df_bupa)), size=sample_size)
  bupa_train_dt <- df_bupa[train,]
  bupa_test_dt <- df_bupa[-train,]
  Bupa_Column_2(bupa_train_dt, bupa_test_dt)
  Bupa_Column_2_4(bupa_train_dt, bupa_test_dt)
  Bupa_Column_2_4_5(bupa_train_dt, bupa_test_dt)
}

main_func()
