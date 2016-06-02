getwd()
setwd("/MSAN_USF/courses_fall/621-MachineLearning/homework3")
news.data <-  read.csv("OnlineNewsPopularityTraining.csv", header=TRUE)
news.data.test <- read.csv("OnlineNewsPopularityTest.csv", header=TRUE)
nrow(news.data)
names(news.data)

nd <- subset(news.data, , -c(1,2, 61))
nd.pca <- subset(news.data, , -c(1,2,61,62))
nd.pca.t <- subset(news.data.test,,-c(1,2, 61, 62))
nd.t <- subset(news.data.test,,-c(1,2, 61))
names(nd)
pairs(nd)
str(nd)
####USING PCA
pr.out <- prcomp(nd.pca, scale = TRUE)
names(pr.out)
dim(pr.out$x)
pr.var <- pr.out$sdev^2
pve = pr.var / sum(pr.var)
par(mfrow = c(1,2))
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')

plot(cumsum(pve), xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')

ret = 0
for(i in 1:length(cumsum(pve))){
  if(round(cumsum(pve)[i], 5) >= 0.79){
    ret = i
    break
  }
}

###USING ELASTIC NET
library(glmnet)
sum(is.na(nd))
x <- model.matrix(popular~.,data=nd)[,-1]
y <- nd$popular
set.seed(1) 
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.train <- y[train]
y.test <- y[test]
grid.lambda <- 10^seq(10, -2, length = 100)
EN.model.train <- glmnet(x[train, ], y.train, alpha = 0.5, lambda = grid.lambda)


#Perform cross validation on the training set to select the best lambda
set.seed(1) #for reproducability
cv.out <- cv.glmnet(x[train, ], y.train, alpha = 0.5)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda
abline(v = log(best.lambda), col = "blue", lwd = 2)
#Find the best lambda value
EN.pred <- predict(EN.model.train, s = best.lambda, newx = x[test,])
mspe.EN <- mean((EN.pred - y.test)^2)
mspe.EN

final.model <- glmnet(x, y, alpha = 0.5, lambda = best.lambda)
Coef.EN <- coef(final.model)[1:20,]
pred_str <- 'popular~n_tokens_title+n_tokens_content+n_unique_tokens+n_non_stop_words+
n_non_stop_unique_tokens + num_hrefs+num_self_hrefs+num_imgs+num_videos
+ average_token_length + num_keywords +data_channel_is_lifestyle+
data_channel_is_entertainment+data_channel_is_bus+data_channel_is_socmed+
data_channel_is_tech+data_channel_is_world+kw_min_min+kw_max_min'
for(i in 1:length(Coef.EN)){
  st <- names(Coef.EN[i])
  pred_str 
}
n_tokens_title 
####USING LASSO
sum(is.na(nd))
x <- model.matrix(popular~.,data=nd)[,-1]
y <- nd$popular

x.t <- model.matrix(popular~.,data=nd.t)[,-1]
y.t <- nd.t$popular
install.packages("glmnet")
library(glmnet)
grid.lambda <- 10^seq(10, -2, length = 100)
lasso.model <- glmnet(x, y, alpha = 1, lambda = grid.lambda)
plot(lasso.model)
set.seed(1) 
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.train <- y[train]
y.test <- y[test]
lasso.model.train <- glmnet(x[train, ], y.train, alpha = 1, lambda = grid.lambda)

lasso.model.train <- glmnet(x, y, alpha = 1, lambda = grid.lambda)
#Perform cross validation on the training set to select the best lambda
set.seed(1) #for reproducability
cv.out <- cv.glmnet(x, y, alpha = 1)
plot(cv.out)

#Find the best lambda value
best.lambda <- cv.out$lambda.min
best.lambda
plot(cv.out)
abline(v = log(best.lambda), col = "blue", lwd = 2)
#Calculate the MSPE of the model on the test set
lasso.pred <- predict(lasso.model.train, s = best.lambda, newx = x.t)
mspe.lasso <- mean((lasso.pred - y.t)^2)
mspe.lasso#0.1554841
#Fit the final model to the entire data set using the chosen lambda
df1.x  <- rbind(x , x.t)
df1.y <- append(y, y.t)
final.model <- glmnet(df1.x, df1.y, alpha = 1, lambda = best.lambda)
Coef.Lasso <- coef(final.model)[1:20,]
Coef.Lasso


mat <- cor(nd)
library(corrplot)
corrplot(mat, method="number" )
names(Smarket)
names(nd)[1]
names(nd)[1]+names(nd)[2]+names(nd)[3]
attach(nd)
glm1 <- glm(pred_str, data = nd, family=binomial)
summary(glm1)
glm11 <- glm(popular~weekday_is_wednesday, data = nd, family=binomial)
glm.probs<- NULL
glm.probs <- predict(glm11, type = "response")
d1 <- length(glm.probs)
glm.pred.train <- rep(0, d1)
glm.pred.train[glm.probs > 0.2] = 1
table(glm.pred.train, nd$popular)
glm1.train<- mean(glm.pred.train == nd$popular)

d2 <- length(nd.t$popular)
glm.pred.test <- rep(0, d2)
 
#Fit the model on the Test data
glm.probs <- predict(glm11, nd.t, type = "response")
glm.pred.test[glm.probs > 0.2] = 1

table(glm.pred.test, nd.t$popular)

#Accuracy on the Test Set

mean(glm.pred.test == nd.t$popular)

glm2 <- glm(popular ~ data_channel_is_entertainment+data_channel_is_bus+
  kw_avg_max+self_reference_avg_sharess+
  weekday_is_tuesday+weekday_is_wednesday+weekday_is_thursday+
  weekday_is_friday+LDA_02, data = nd, family=binomial)

glm3 <- glm(popular~n_tokens_content+num_hrefs +num_self_hrefs+average_token_length+num_keywords
            +data_channel_is_lifestyle+data_channel_is_entertainment+ data_channel_is_bus
            + data_channel_is_world+kw_avg_max+kw_min_avg+kw_max_avg+kw_avg_avg
            + weekday_is_monday+weekday_is_tuesday+weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+global_subjectivity
            + title_subjectivity +title_sentiment_polarity+abs_title_subjectivity
            ,data = nd, family=binomial)
summary(glm3)
nm1


glm4 <- glm(popular~n_tokens_title+n_tokens_content+ n_unique_tokens
            + n_non_stop_words + n_non_stop_unique_tokens + num_hrefs + num_self_hrefs 
            + num_imgs + num_videos + average_token_length + num_keywords
            + data_channel_is_lifestyle + data_channel_is_entertainment
            + data_channel_is_bus + data_channel_is_socmed
            + data_channel_is_tech + data_channel_is_world 
            + kw_min_min + kw_max_min
            ,data = nd, family=binomial)
summary(glm4)
coef(glm3)
glm.probs <- predict(glm3, type = "response")
head(glm.probs)
d1 <- length(glm.probs)
glm.pred.train <- rep("Down", d1)
glm.pred.train[glm.probs > 0.5] = "Up"



#Let's look at our classification results on the training
table(glm.pred.train, nd$popular)
mean(glm.pred.train == nd$popular)

#####
vif(glm3)
library(leaps)
reg.fit <- regsubsets(popular~., data=nd, nvmax = 5, method = "backward")
reg.sum <- summary(reg.fit)
reg.sum$bic
length(reg.sum$which[5])
nm <- names(reg.sum$which[5,])[reg.sum$which[5,][] == TRUE]
nm1 <- nm[-1]
s1 <- paste(nm1, collapse ='+')
nm2 <- paste("popular","~",s1)
str1 <- 'popular ~ n_tokens_content+num_hrefs+num_self_hrefs+average_token_length+num_keywords+data_channel_is_lifestyle+data_channel_is_entertainment+data_channel_is_bus+data_channel_is_world+kw_min_avg+self_reference_avg_sharess+weekday_is_monday+weekday_is_tuesday+weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+LDA_01+LDA_02+LDA_03+LDA_04+global_subjectivity'
m2 <- glm(str1, data = nd, family=binomial)
summary(m2)
glm.probs<- NULL
glm.probs <- predict(m2, type = "response")
d1 <- length(glm.probs)
glm.pred.train <- rep(0, d1)
glm.pred.train[glm.probs > 0.2] = 1
table(glm.pred.train, nd$popular)
glm1.train<- mean(glm.pred.train == nd$popular)

d2 <- length(nd.t$popular)
glm.pred.test <- rep(0, d2)

#Fit the model on the Test data
glm.probs <- predict(m2, nd.t, type = "response")
glm.pred.test[glm.probs > 0.2] = 1

table(glm.pred.test, nd.t$popular)

#Accuracy on the Test Set

mean(glm.pred.test == nd.t$popular)
which.min(reg.sum$bic)

nm2 <- paste("popular","~",s1)

glm5 <- glm(mod.str, data=nd, family = binomial)
summary(glm5)#30280 30173
vif(glm5)

library(pROC)

#Fit three logistic models
m1 <- glm(case ~ age,data=infert,family=binomial())
m2 <- glm(case ~ age + parity + education + induced,
          data=infert,family=binomial())
m3 <- glm(case ~ age+parity+education+spontaneous+induced,
          data=infert,family=binomial())
install.packages("pROC")
library(pROC)
#Plot the three ROCs, adding the AUC for each
plot.roc(nd$popular,fitted(glm1),print.auc = TRUE)
plot.roc(nd$popular,fitted(glm2),add=TRUE,col = "blue",
         print.auc=TRUE,print.auc.y = 0.45)
plot.roc(nd$popular,fitted(glm3),add = TRUE,col = "red",
         print.auc = TRUE,print.auc.y = 0.4)
#removing kw_max_avg kw_avg_avg due to high colinearity

mod.str <- "popular ~ n_tokens_content+num_hrefs+num_self_hrefs+average_token_length+
num_keywords+data_channel_is_lifestyle+data_channel_is_entertainment+
data_channel_is_bus+data_channel_is_world+kw_min_avg+
self_reference_avg_sharess+weekday_is_monday+weekday_is_tuesday+
weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+
LDA_01+LDA_02+LDA_03+LDA_04+global_subjectivity"

coef(glm3)
glm.probs <- predict(glm4, type = "response")
head(glm.probs)
d1 <- length(glm.probs)
glm.pred.train <- rep(0, d1)
glm.pred.train[glm.probs > 0.5] = 1
table(glm.pred.train, nd$popular)
mean(glm.pred.train == nd$popular)

library(leaps)
reg.fit <- regsubsets(popular~., data=nd, nvmax = 58, method = "backward")
reg.sum <- summary(reg.fit)
length(reg.sum$which[25])
nm <- names(reg.sum$which[25,])[reg.sum$which[25,][] == TRUE]
nm1 <- nm[-1]
s1 <- paste(nm1, collapse ='+')

nm2 <- paste("popular","~",s1)
m1 <- glm(nm2,data=nd,family=binomial())
summary(m1)


vif(m1)
names(reg.sum)
which.max(reg.sum$adjr2)
which.min(reg.sum$bic)
which.min(reg.sum$cp)
regfit.best=regsubsets(popular~., data=nd, nvmax = 59)
for (i in 2 : length(Coef.Lasso))
if(names(nd) == Coef.Lasso)
#names(nd) in Coef.Lasso()
  library(pROC)

#Fit three logistic models
m1 <- glm(case ~ age,data=infert,family=binomial())
m2 <- glm(case ~ age + parity + education + induced,
          data=infert,family=binomial())
m3 <- glm(case ~ age+parity+education+spontaneous+induced,
          data=infert,family=binomial())

#Plot the three ROCs, adding the AUC for each
plot.roc(infert$case,fitted(m1),print.auc = TRUE)
plot.roc(infert$case,fitted(m2),add=TRUE,col = "blue",
         print.auc=TRUE,print.auc.y = 0.45)
plot.roc(infert$case,fitted(m3),add = TRUE,col = "red",
         print.auc = TRUE,print.auc.y = 0.4)

####LDA ####

#For each of the methods, carefully describe how you choose any thresholds or tuning parameters. 
#Use appropriate plots and documentation to describe your results. 
#Feel free to remove variables or perform dimension reduction if you think it will help your predictions. 
#I am being intentionally vague here because I want to see how you would handle such a data set in practice.
library(MASS)
par(mfrow = c(2,2))
hist(nd.t$self_reference_avg_sharess[nd.t$popular == 1], n = 100, main = "shares | 1", xlab = "")
hist(nd.t$self_reference_avg_sharess[nd.t$popular == 0], n = 100, main = "shares | Down", xlab = "")
str(Smarket)
str(nd)
par(mfrow = c(2,2))
hist(nd.t$LDA_02[nd.t$popular == 1], n = 100, main = "LDA_02 | 1", xlab = "")
hist(nd.t$LDA_02[nd.t$popular == 0], n = 100, main = "LDA_02 | 0", xlab = "")
formula <- mod.str
lda.fit1 <- lda(formula, data=nd)
lda.fit1 <- lda(popular ~ data_channel_is_entertainment+data_channel_is_bus+
                  kw_avg_max+self_reference_avg_sharess+
                  weekday_is_tuesday+weekday_is_wednesday+weekday_is_thursday+
                  weekday_is_friday+LDA_02, data=nd)

lda.fit1
plot(lda.fit1)
lda.pred <- predict(lda.fit1, nd.t)

names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, nd.t$popular)
mean(lda.class == nd.t$popular)#0.7927598

lda.pred$posterior[1:20, 1]
lda.class[1:20]
################

#run QDA on the training data
qda.fit <- qda(popular ~ data_channel_is_entertainment+data_channel_is_bus+
                 kw_avg_max+kw_max_avg+kw_avg_avg+self_reference_avg_sharess+
                 weekday_is_tuesday+weekday_is_wednesday+weekday_is_thursday+
                 weekday_is_friday+LDA_02, data = nd)

#look at summary of fit
qda.fit

#predict the market Direction in 2005
qda.class <- predict(qda.fit, nd.t)$class

#look at the confusion matrix
table(qda.class, nd.t$popular)
mean(qda.class == nd.t$popular)
########KNN

library(class)

train.X <-nd[,-59]
test.X <- nd.t[,-59]
train.popular <- nd$popular
?knn
set.seed(1) #for reproducability for the randomly breaking of ties
knn.pred <- knn(train.X, test.X, train.popular, k = 19)# 100 shows slight improvemnt but incorrect

#look at the confusion matrix
table(knn.pred, nd.t$popular)
mean(knn.pred == nd.t$popular)
#9 got 78.43
#11 0.7868315
#19 0.7912462
######NaiveBayes####
install.packages("e1071")

install.packages("klaR")
library(class) 
library(klaR)
nm2
mod <- naiveBayes(popular ~ data_channel_is_entertainment+data_channel_is_bus+
                    kw_avg_max+kw_max_avg+kw_avg_avg+self_reference_avg_sharess+
                    weekday_is_tuesday+weekday_is_wednesday+weekday_is_thursday+
                    weekday_is_friday+LDA_02, data = nd)
table(predict(mod, nd, threshold =0.2), nd$popular)

