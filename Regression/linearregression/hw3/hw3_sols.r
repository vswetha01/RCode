library("car")
install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(gtable)
library(gridExtra)
#getwd()
setwd('/MSAN_USF/courses_fall/601_Linear_regression/homework/w3')

###QUestion 3
summary(anscombe)
View(anscombe)

lm_1 <- lm(y1 ~ x1, data = anscombe)
summary(lm_1)
anova(lm_1)
coef(lm_1)
y_1_hat <- coef(lm_1)[1]+ coef(lm_1)[2]*anscombe$x1
residuals(lm_1)

scatterplot(anscombe$x1 , anscombe$y1, xlab ="x-value"
            , ylab="y value", main="anscombe 1")
scatterplot(residuals(lm_1), fitted(lm_1), xlab= 'fitted', 
            ylab='residuals', main= 'plot1')
#R-squared 0.6665

lm_2 <- lm(y2 ~ x2, data = anscombe) #0.6662
summary(lm_2)
coef(lm_2)
anova(lm_2)
scatterplot(anscombe$x2 , anscombe$y2, xlab ="x-value"
            , ylab="y-value", main="anscombe 2")
y_2_hat <- coef(lm_2)[1]+ coef(lm_2)[2]*anscombe$x2
scatterplot(residuals(lm_2), fitted(lm_2),
            xlab= 'fitted', ylab='residuals', main='plot2')

lm_3 <- lm(y3 ~ x3, data = anscombe)#0.6663
coef(lm_3)
y_3_hat <- coef(lm_3)[1]+ coef(lm_3)[2]*anscombe$x3
summary(lm_3)
anova(lm_3)
scatterplot(residuals(lm_3), fitted(lm_3), xlab= 'fitted',
            ylab='residuals', main='plot3')

scatterplot(anscombe$x3 , anscombe$y3, xlab ="x-value"
            , ylab="y-value", main="anscombe 3")

lm_4 <- lm(y4 ~ x4, data = anscombe)#0.6667
coef(lm_4)
y_4_hat <- coef(lm_4)[1]+ coef(lm_4)[2]*anscombe$x4
summary(lm_4)
anova(lm_4)
scatterplot(anscombe$x4 , anscombe$y4, xlab ="x-value"
            , ylab="y-value", main="anscombe 4")
scatterplot(residuals(lm_4), fitted(lm_4), xlab= 'fitted', 
            ylab='residuals', main='plot4')

abline(a = coef(lm_4)[1], b = coef(lm_4)[2], col = 'blue')


#Question 4
#read extraColumnsOfRandomData.csv
#ncol(d)
names(d) <- make.names(names(d))


#y <- "BODYFAT"
#x <- names(d)[!names(d) %in% y]
#x
#y
#len_d <- length(names(d))
#col_formula <- names(d)[2]
#reading data from data source
d <- read.csv('extraColumnsOfRandomData.csv', header = TRUE)
x_num <- c()
R_sq <- c()
R_sq_adj <- c()
# looping all the elements to calculate the r_sq and rsq_adj for each model
for(i in 2:(ncol(d))){
  df <- d[, c(1:i)]
  lm_mod<- lm(df$BODYFAT~., data=df)
  x_num <- append(x_num, ncol(df)-1) 
  R_sq <- append(R_sq , summary(lm_mod)$r.squared) #0.6569396
  R_sq_adj<- append(R_sq_adj, summary(lm_mod)$adj.r.squared)
}
#data frame with x, r_sq, r_sq_adj
df1 <- data.frame(x_num, R_sq, R_sq_adj)
#plotting code 
opar <- par(mar=c(5,4,4,5)+0.1)
par(new=TRUE)
#axis(side=4)
#mtext(side=4,line=3.8,"R_sq_adj")
#legend("bottomright", legend=c("R_sq","R_sq_adj"), pch=c(1,3))opar <- par(mar=c(5,4,4,5)+0.1)
plot(df1$x_num,df1$R_sq, xlab = "Number of variables",ylab = "R_sq",main = "R vs Radj")
par(new=T)
plot(df1$x_num,df1$R_sq_adj,pch=3, axes = F,ylab="",xlab="")
axis(side=4)
mtext(side=4,line=3.8,"R_sq_adj")
legend("bottomright", legend=c("R_sq","R_sq_adj"), pch=c(1,3))

###END question 4
###QUestion 5
el_data <- read.table('electricBillData.txt', header= FALSE)
df_e_bill<- data.frame(Y = el_data$V4, avg_temp = el_data$V5, hdd = el_data$V6, 
                       cdd = el_data$V7, nfm = el_data$V8, nem = el_data$V9, 
                       hp1 = el_data$V10, hp2 = el_data$V11)

df_e_bill<- df_e_bill[!df_e_bill$Y == '*',]
df_e_bill$Y <- as.numeric(df_e_bill$Y)

lm_electric <- lm(Y~., data=df_e_bill)
summary(lm_electric)
anova(lm_electric)
Anova(lm_electric)
lm_electric_1 <- lm(Y~.-hp2, data=df_e_bill)
summary(lm_electric_1)

lm_electric_2 <- lm(Y~.-nfm, data=df_e_bill)
summary(lm_electric_2)
lm_electric_3 <- lm(Y~.-hdd, data=df_e_bill)
summary(lm_electric_3)
cor(df_e_bill)
cor(df_e_bill[sapply(df_e_bill, is.numeric)])

corrplot(cor(df_e_bill), method="number")
pairs(~., data=df_e_bill)
df_cor<- data.frame(x1 = df_e_bill$avg_temp,
                    x2 = df_e_bill$hdd,
                    x3 = df_e_bill$cdd)

cor(df_cor)
corrplot(cor(df_cor), method = c("circle"))
df_cor_1<- data.frame(x1 = df_e_bill$avg_temp,
                    x2 = df_e_bill$hdd,
                    x3 = df_e_bill$cdd,
                    Y = df_e_bill$Y)
lm_electric_4 <- lm(Y~., data=df_cor_1)
summary(lm_electric_4)

lm_electric5 <- lm(Y~.-avg_temp, data=df_e_bill)
summary(lm_electric5)

lm_electric5 <- lm(Y~.-avg_temp-hdd, data=df_e_bill)
summary(lm_electric5)
#0.778 is the r2 for this model. its teh co-efficient of determination
#and measure the proportionate reduction in of total variation in in montly bill
#associated with the use of the average temperature, heating degree days, 
#cooling degree days, number of family members, 
#whether or not there is a new electric meter, 
#whether or not the home’s first heat pump has been replaced, 
#and whether or not the home’s second heat pump has been replaced


####Question 6
scenic_data <- read.csv('SENIC_data.csv', header= FALSE)
df_sd <- data.frame(afs = scenic_data$V12, nn = scenic_data$V11)
library(lmtest)
summary(df_sd)
lm_sd_2 <- lm(nn ~ afs + I(afs^2), data= df_sd)
summary(lm_sd_2) #0.6569
summary(lm_sd_2)$r.squared #0.6569396
summary(lm_sd_2)$adj.r.squared #0.6507022
summary(lm_sd_2$residuals) 
Anova(lm_sd_2)
sse_1_2 <- sum((lm_sd_2$residuals)^2)
sse_1 <- sum((lm_sd_1$residuals)^2)
(sse_1 -sse_1_2)
r_1_2 <- (sse_1 -sse_1_2)/sse_1
r_2_1 <- (sse_2 - sse_1_2)/sse_2
lm_sd_1 <- lm(nn ~ afs, data= df_sd)
summary(lm_sd_1)$r.squared #0.6138809
summary(lm_sd_1)$adj.r.squared #0.6104023
resettest(lm_sd_2, power=2, type="fitted")
#p-value is high...quadratic is needed
residuals(lm_sd)
fitted(lm_sd)
scatterplot(fitted(lm_sd_2) , residuals(lm_sd_2), xlab ="fitted"
            , ylab="Residuals", main="Residual plot Scenic data")
##End






