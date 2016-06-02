install.packages("corrplot")
library(corrplot)

college_data <- read.csv("/MSAN_USF/courses_fall/621-MachineLearning/College.csv", header=TRUE)
y <- college_data$Room.Board
X_df <- data.frame(college_data$Accept, college_data$Enroll, 
                   college_data$Outstate, college_data$Books, 
                   college_data$Grad.Rate)
gdf <- data.frame(college_data$Accept, college_data$Enroll, 
                  college_data$Outstate, college_data$Books, 
                  college_data$Grad.Rate, y)
y <- college_data$Room.Board
df.1 <- data.frame(college_data$Accept, college_data$Enroll, 
                   college_data$Outstate, college_data$Books, 
                   college_data$Grad.Rate, college_data$Room.Board)

pairs(df.1, main="College data", pch=21, data=df.1)

corrplot(cor(df.1), method="number")
#k: integer number of disjoint sets
#seed: numeric value to set random number generator seed for repro- ducability
#X: n × p design matrix
# y: n × 1 numeric response
# which.betas : p × 1 logical specifying which predictors to be included in a regression
#
#function to do k-fold cross validation
kfold.cv.lm = function(k, seed, X, y, which.betas){
  #check the number of rows in X equal that in Y
  X <- as.data.frame(X)
  Y <- as.data.frame(y)
  n <- nrow(X)

  p <- ncol(which.betas)
  if (nrow(Y) != n)
    return
  if (nrow(which.betas) != p)
    return 
  if (!is.null(seed)) set.seed(seed)
  which.betas.T <- t(which.betas)
  select <- which.betas.T
  X_new = X[, select == 1]
  cnt <- length(which(select == 1))
  
  for(i in 1 : cnt){
    names(X_new)[i] <- paste('X',i,sep="")
  }
  df_all <- data.frame(X_new, Y)
  df_all <- df_all[sample.int(nrow(df_all)), ]
  rows <- nrow(df_all)
  kfolds <- cut(seq(1, rows), breaks= 10, labels=FALSE)
  lst_mse <- vector()
  lst_mspe <- vector()
  for (i in 1:10){
    testidx = which(kfolds == i, arr.ind = TRUE)
    test_set <- df_all[testidx, ]
    training_set <- df_all[-testidx,]
    
    lm.fit <- lm(y~.,data=training_set)
    mse <- mean((lm.fit$residuals)^2)
    mse_new <- mean((training_set$y - predict(lm.fit, training_set))^2)
    mspe <- mean((test_set$y - predict(lm.fit, test_set))^2)
    lst_mse <- append(lst_mse, mse)
    lst_mspe <- append(lst_mspe, mspe)
  }
  ave_mspe <- mean(lst_mspe)
  ave_mse <- mean(lst_mse)
  vec <- c(ave_mse, ave_mspe)
  return (vec)
}

#function taking which.betas, and maximum adjusted rsqaure for the model
#X input independent/predictor variables
#y response variable function to 
getAllAdjustedR2 = function(which.betas, X, y){
  select <- t(which.betas)
  X_new = X[, select == 1]
  cnt <- length(which(select == 1))
  
  for(i in 1 : cnt){
    names(X_new)[i] <- paste('X',i,sep="")
  }
  df_all <- data.frame(X_new, Y)
  lm.fit <- lm(y~.,data=df_all)
  return (summary(lm.fit)$adj.r.squared)
}

#Function creates a vector based on input variables 
createLogicalVectorFromCombn = function(values){
  vec <- c(0,0,0,0,0)
  for(val in values){
    vec[val] <- 1
  }
  return (vec)
}
#Function to get all combinations of the variables in the model
getAllCombinations = function(X_df){
  p <- ncol(X_df)
  all <- list()
  a <- vector()
  k <- 1
  for (i in 1:p){
    cb <- combn(p, i)
    cb_tr <- t(cb)
    r <- nrow(cb_tr)
    for(j in 1:r){
      a <- createLogicalVectorFromCombn(cb_tr[j,])
      all[[k]] <- a
      k <- k + 1
    }
  }
  return (all)
}
#gets a collection of MSE and MSPE from running k - fold regression on all possible models
#Runs k-fold cross validation and returns mspe and mse for each model
computeMSE <- function(all, X_df, y, selModel=FALSE){
  v1 <- vector()
  n <- length(all)
  vMse <- numeric(n)
  vMspe <- numeric(n)
  for (ik in 1: length(all)){
    m <- matrix( all[[ik]], nrow=5, ncol=1)#all[[ik]]
    df_Mod <- data.frame()
    for(v in 1:100){
      v1 <- kfold.cv.lm(10, v, X_df, y, m)
      df_Mod <- rbind(df_Mod, v1)
    }
    colnames(df_Mod)[1] <- "MSE"
    colnames(df_Mod)[2] <- "MSPE"
    if(selModel)
      return (df_Mod)

    vMse[ik] <- mean(unlist(df_Mod$MSE))
    vMspe[ik] <- mean(unlist(df_Mod$MSPE))
    
  }

  
  df <- data.frame(vMse, vMspe)
  #df <- cbind(df, v1)
  colnames(df)[1] <- "MSE"
  colnames(df)[2] <- "MSPE"
  #colnames(df)[3] <- "model"
  return (df)
}
#function to get all subsets and calculate the adjusted r-squares
getAllSubsets = function(cmb_all, X_df, y){
  
  v1 <- vector()
  v2 <- vector()
  
  for(i in 1: length(cmb_all)){
    ra2 <- getAllAdjustedR2(cmb_all[[i]], X_df, y)
    select <- cmb_all[[i]]
    v1 <- append(v1, paste(colnames(X_df)[which(select == 1)], collapse = " "))
    v2 <- append(v2, ra2)
  }
  df_r2 <- data.frame(v1, v2)
  return (df_r2)
}
#getwd()
#setwd('/MSAN_USF/courses_fall/621-MachineLearning')


cmb_all <- getAllCombinations(X_df)
d100 <- computeMSE(cmb_all, X_df, y, FALSE)
st <- all[[ik]]
st <- paste(st, collapse="")
v1 <- append(v1, st)
c1 <- cmb_all[[which.min(d100$MSE)]]
dk <- computeMSE(c1, X_df, y, TRUE)
new_d <- cbind(d100, v1)
df_res <- getAllSubsets(cmb_all, X_df, y)
df_res[which.min(df_res$v2)]
df_res[which.max(df_res$v2),]
df.1<- data.frame(df.1)
mod1 <- lm(y~., data=gdf)
summary(mod1)
all <- list()
all[[1]] <- c(1,1,1,1,1)
dk <- computeMSE(all[[1]], X_df, y, TRUE)
which.min(dk$MSPE)
hist(dk)
hist(unlist(dk$MSPE),main="Histogram for MSPE", 
     xlab="MSPE", 
     border="blue", 
     col="green",
     las=1, 
     breaks=15)

hist(unlist(dk$MSE),main="Histogram for MSE", 
     xlab="MSE", 
     border="blue", 
     col="green",
     las=1, 
     breaks=15)

mod_data <- data.frame(X_df, y)
lm.mod <- lm(y~.,data=mod_data)
shapiro.test(lm.mod$residuals)
plot(lm.mod$residuals, lm.mod$fitted)
#abline(lm.mod, col=2)
qqnorm(lm.mod$residuals)
qqline(lm.mod$residuals)
#Chosen model is 5th model then use that model
#to get Estimates and standard errors
install.packages("leaps")
library(leaps)
k <- leaps( x=X_df, y=y, names=names(X_df)[1:5], method="adjr2")
which.min(k$adjr2)


######Ridge regrssion code
install.packages("glmnet", repos='http://cran.us.r-project.org')
install.packages("ISLR", repos='http://cran.us.r-project.org')
library(glmnet, quietly = TRUE)
library(ISLR, quietly = TRUE)
college_data <- read.csv("/MSAN_USF/courses_fall/621-MachineLearning/College.csv", header=TRUE)
#y <- college_data$Room.Board
names(college_data)
sum(is.na(college_data))
coll.data <- subset(college_data, select=-c(X))

head(college_data)
ncol(coll.data)
x <- model.matrix(Room.Board~., data=coll.data)[,-1]
y <- coll.data$Room.Board
str(x)
grid.lambda <- 10^seq(8, -2, length=100)
ridge.model <- glmnet(x, y, alpha=0, lambda=grid.lambda)
plot(ridge.model)
#ridge.model$lambda[50]
#sqrt(sum(coef(ridge.model)[-1,50]^2))
#sqrt(sum(coef(ridge.model)[-1,50]^2))
ell2.norm <- numeric()
for(i in 1:length(grid.lambda)){
  ell2.norm[i] <- sqrt(sum(coef(ridge.model)[-1,i]^2))
}
plot(x= grid.lambda, y=ell2.norm, 
     xlab= expression(lambda),
     ylab="L2 Norm", ylim = c(40, 350), xlim=c(1,10000))

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.train <- y[train]
y.test <- y[test]
ridge.model.train <- glmnet(x[train, ], y.train, alpha = 0, lambda = grid.lambda)
set.seed(1)
cv.out <- cv.glmnet(x[train,], y.train, alpha=0, lambda = grid.lambda)
plot(cv.out)
best.lambda <- cv.out$lambda.min
#plot(cv.out)
abline(v=log(best.lambda),col="blue", lwd=2)

ridge.pred <- predict(ridge.model.train, s=best.lambda, newx=x[test,])
mspe.ridge <- mean((ridge.pred - y.test)^2)
mspe.ridge
best.lambda


#602557.9 for lambda between 5 and 1 min lambda is 112.3324
# 647625.7 for lambda between 5 and 2 min lambda 100
# 647627.3 2 and 1 lambda = 97.701
# 647610.5 for lambda = 100 10, -2
# 647527.2 for  8, -2 86.9749
#647528.2 fro 8 -4 100 86.9749
final.model <- glmnet(x,y, alpha=0, lambda = best.lambda)
Coef.Ridge <- coef(final.model)[1:18,]
Coef.Ridge





#######Lasso regression code

grid.lambda <- 10^seq(1, -7, length=100)
lasso.model <- glmnet(x, y, alpha = 1, lambda = grid.lambda)
plot(lasso.model, ylim = c(-20, 30), xlim=c(1,350))

lasso.model.train <- glmnet(x[train, ], y.train, alpha = 1, lambda = grid.lambda)
#Perform cross validation on the training set to select the best lambda
set.seed(1) #for reproducability
cv.out <- cv.glmnet(x[train, ], y.train, alpha = 1)
plot(cv.out)

#Find the best lambda value
best.lambda <- cv.out$lambda.min

plot(cv.out)
abline(v = log(best.lambda), col = "blue", lwd = 2)
lasso.pred <- predict(lasso.model.train, s = best.lambda, newx = x[test,])
mspe.lasso <- mean((lasso.pred - y.test)^2)
mspe.lasso
best.lambda
#660961.5 8, -1 22.52383
#660936.2 5 -5
#652312.5  1, -7 500
#660853.4  10, -5 22.52383
#660764.7 10 , -7 22.52383


final.model <- glmnet(x, y, alpha = 1, lambda = best.lambda)
Coef.Lasso <- coef(final.model)[1:18,]
Coef.Lasso



######ElasticNet regression Code
grid.lambda <- 10^seq(10, -2, length=100)
EN.model <- glmnet(x, y, alpha = 0.5, lambda = grid.lambda)
plot(EN.model, ylim = c(-20, 30), xlim=c(1,350))

#Now, fit a Lasso regression model to the training data
EN.model.train <- glmnet(x[train, ], y.train, alpha = 0.5, lambda = grid.lambda)
#Perform cross validation on the training set to select the best lambda
set.seed(1) #for reproducability
cv.out <- cv.glmnet(x[train, ], y.train, alpha = 0.5)
plot(cv.out)

#Find the best lambda value
best.lambda <- cv.out$lambda.min
best.lambda
plot(cv.out)
abline(v = log(best.lambda), col = "blue", lwd = 2)
#Calculate the MSPE of the model on the test set
EN.pred <- predict(EN.model.train, s = best.lambda, newx = x[test,])
mspe.EN <- mean((EN.pred - y.test)^2)
mspe.EN
best.lambda



#Fit the final model to the entire data set using the chosen lambda
final.model <- glmnet(x, y, alpha = 0.5, lambda = best.lambda)
Coef.EN <- coef(final.model)[1:18,]
Coef.EN
Coefficients <- data.frame(Ridge = Coef.Ridge, Lasso = Coef.Lasso, Elastic.Net = Coef.EN)
MSPE <- data.frame(Ridge = mspe.ridge, Lasso = mspe.lasso, Elastic.Net = mspe.EN)

Coefficients
MSPE
