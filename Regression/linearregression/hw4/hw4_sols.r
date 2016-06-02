install.packages('alr4')
combinations(5,2)
install.packages("corrplot")
library(corrplot)
library(lmtest)
library(alr4)
install.packages("MASS")
library(MASS)
library(car)
library(corrplot)
library(gtools)

#setwd('/MSAN_USF/courses_fall/601_Linear_regression/homework/w4')

###Question 1


df_trans <- data.frame(Y=Transact$time, 
                       t1=Transact$t1, 
                       t2=Transact$t2, 
                       a=(Transact$t1 + Transact$t2)/2,
                       d=(Transact$t1 - Transact$t2))
lm_trans_1 <- lm(Y~t1+t2, data=df_trans)
summary(lm_trans_1)
b_01 <- coef(lm_trans_1)[1]
b_11 <- coef(lm_trans_1)[2]
b_21 <- coef(lm_trans_1)[3]
plot(df_trans$t1+df_trans$t2, df_trans$Y)
abline(lm_trans_1)


lm_trans_2 <- lm(Y~a+d, data=df_trans)
summary(lm_trans_2)
plot(df_trans$a+df_trans$d, df_trans$Y)

abline(lm_trans_2)
b_02<- coef(lm_trans_2)[1]
b_32 <- coef(lm_trans_2)[2]
b_42 <- coef(lm_trans_2)[3]


lm_trans_3 <- lm(Y~t2+d, data=df_trans)
summary(lm_trans_3)
plot(df_trans$t2+df_trans$d, df_trans$Y)
abline(lm_trans_3)
b_03<- coef(lm_trans_3)[1]
b_23 <- coef(lm_trans_3)[2]
b_43 <- coef(lm_trans_3)[3]

lm_trans_4 <- lm(Y~., data=df_trans)
summary(lm_trans_4)
plot(df_trans$t1+df_trans$t2+df_trans$a+df_trans$d, df_trans$Y)
abline(lm_trans_4)
b_04<- coef(lm_trans_4)[1]
b_14 <- coef(lm_trans_4)[2]
b_24 <- coef(lm_trans_4)[3]
b_34 <- coef(lm_trans_4)[4]
b_44 <- coef(lm_trans_4)[5]
###Question 2

View(UN11)
lm_un <- lm(log(fertility)~pctUrban, data=UN11)
summary(lm_un)
(exp(-0.010163 ) -1 )* 100
log(101/100)*1.16
lm_un2 <- lm(log(fertility) ~ log(ppgdp) + lifeExpF, data=UN11)
summary(lm_un2)
(((1.25)^-0.06544 ) -1)*100

###Question3

View(cakes)
df_ck <- data.frame(Y= cakes$Y, X1 = cakes$X1, X2 = cakes$X2)
cor(df_ck)
plot(df_ck)
lm_ck_1 <- lm(Y~., data=df_ck)
summary(lm_ck_1)
plot(lm_ck_1)
residualPlots(lm_ck_1)
shapiro.test(lm_ck_1$residuals)
plot(fitted(lm_ck_1), residuals(lm_ck_1),xlab="Fitted ",
     ylab="Residuals", main="Residual Plot")
abline(h=0)
abline(lm_ck_1)
leveneTest(lm_ck_1, center = median)
bptest(lm_ck_1)
ncvTest(lm_ck_1)

boxcox(lm_ck_1)
boxcox(lm_ck_1, lambda = seq(-1.0, 1, 0.1))
df_ck <- cbind(df_ck, df_ck$Y^(-0.45))
names(df_ck)[4] <- 'Y_tr'
residualPlots(lm_ck_1)
lm_ck_t_1 <- lm(Y_tr~log(X1)+X2, data=df_ck)

bptest(lm_ck_t_1)
ncvTest(lm_ck_t_1)

residualPlots(lm_ck_t_1)
lm_ck_2 <- lm(Y~X1*X2, data=df_ck)
summary(lm_ck_2)

plot(lm_ck_2)

abline(lm_ck_2)
residualPlots(lm_ck_2)

lm_ck_3 <- lm(Y~log(X1)+X2, data=df_ck)
summary(lm_ck_3)
plot(lm_ck_3)
abline(lm_ck_3)
residualPlots(lm_ck_3)

lm_ck_4 <- lm(Y~X1+X2+I(X1^2), data=df_ck)
summary(lm_ck_4)
plot(lm_ck_4)
residualPlots(lm_ck_4)


resettest(lm_ck_1, power=2, type="fitted")
lm_ck_5 <- lm(Y~X1+X2+I(X1^2)+I(X2^2), data=df_ck)
summary(lm_ck_5)
residualPlot(lm_ck_5)

lm_ck_6 <- lm(Y~X1+X2+I(X2^2), data=df_ck)
summary(lm_ck_6)
residualPlot(lm_ck_6)

lm_ck_7 <- lm(Y~X1+X2+I(X2^2)+X1:X2, data=df_ck)
summary(lm_ck_7)
residualPlots(lm_ck_7)
bptest(lm_ck_7)
ncvTest(lm_ck_7)

anova(lm_ck_7)
Anova(lm_ck_7)


lm_ck_8 <- lm(Y~X1+X2+I(X2^2)+I(X1^2)+X1:X2, data=df_ck)
summary(lm_ck_8)
residualPlots(lm_ck_8)
bptest(lm_ck_8) #best one p-value
ncvTest(lm_ck_8)

lm_ck_9 <- lm(Y_tr~X1+X2+I(X2^2)+I(X1^2)+X1:X2, data=df_ck)
summary(lm_ck_9)# good r2 and r2adj
residualPlots(lm_ck_9)
bptest(lm_ck_9) #pvalue increases
ncvTest(lm_ck_9)

####Question 3 part b
View(cakes)
df_ck_1 <- data.frame(Y= cakes$Y, X1 = cakes$X1, X2 = cakes$X2, block= cakes$block)
str(df_ck_1$block)

cor(df_ck_1) 
plot(df_ck_1)
lm_ck_11 <- lm(Y~., data=df_ck_1)
summary(lm_ck_11)
Anova(lm_ck_11)
residualPlots(lm_ck_11)
plot(df_ck_1$X1+df_ck_1$X2+df_ck_1$block, df_ck_1$Y)
abline(lm_ck_11) 


lm_ck_12 <- lm(Y~X1*X2*block, data=df_ck_1)
summary(lm_ck_12)
residualPlots(lm_ck_12)

lm_ck_13 <- lm(Y~X1+X2+block+X2:block, data=df_ck_1)
summary(lm_ck_13)
lm_ck_14 <- lm(Y~X1+X2+block+X1:block, data=df_ck_1)
summary(lm_ck_14)
residualPlots(lm_ck_14)

resettest(lm_ck_11, power=2, type="fitted")

lm_ck_15_1 <- lm(Y~X1+X2+block+I(X2^2), data=df_ck_1)
summary(lm_ck_15_1)
residualPlots(lm_ck_15_1)
Anova(lm_ck_15_1)
lm_ck_15_2 <- lm(Y~X1+X2+block+I(X1^2), data=df_ck_1)
summary(lm_ck_15_2)
residualPlots(lm_ck_15_2)
Anova(lm_ck_15_2)

lm_ck_15_3 <- lm(Y~X1+X2+block+I(X1^2)+I(X2^2), data=df_ck_1)
summary(lm_ck_15_3)
residualPlots(lm_ck_15_3)
Anova(lm_ck_15_3)

lm_ck_16 <- lm(Y~X1+X2+block+I(X2^2)+X2:X1, data=df_ck_1)
summary(lm_ck_16)
residualPlots(lm_ck_16)
Anova(lm_ck_16)

lm_ck_16_1 <- lm(Y~X1+X2+block+I(X2^2)+X2:X1:block, data=df_ck_1)
summary(lm_ck_16_1)
residualPlots(lm_ck_16_1)
Anova(lm_ck_16_1)

#this is a good one
lm_ck_16_2 <- lm(Y~X1+X2+I(X2^2)+I(X1^2)+X2:X1, data=df_ck_1)
summary(lm_ck_16_2)
residualPlots(lm_ck_16_2)
Anova(lm_ck_16_2)
shapiro.test(lm_ck_16_2$residuals)
leveneTest(lm_ck_16_2$residuals, center = median, group = df_plastic$group)


#question 4
library(alr4)
View(fuel2001)
names(fuel2001)
df_fuel <- data.frame(Y=fuel2001$FuelC,
                      X1=fuel2001$Drivers,
                      X2= fuel2001$Income,
                      X3=fuel2001$Miles,
                      X4=fuel2001$MPC,
                      X5=fuel2001$Pop,
                      X6=fuel2001$Tax)

lm_fuel <- lm(Y~., data=df_fuel)
summary(lm_fuel)
Y_hat =  coef(lm_fuel)[1] + coef(lm_fuel)[2]*X1+ 
          coef(lm_fuel)[3]*X2 + coef(lm_fuel)[4]*X3+
          coef(lm_fuel)[5]*X4 + coef(lm_fuel)[6]*X5+
          coef(lm_fuel)[7]*X6
anova(lm_fuel)


#question 5 
View(cakes)
nrow(cakes)
df_cake <- data.frame(Y= cakes$Y, X1 = cakes$X1, X2 = cakes$X2)
lm_cake <- lm(Y~X1+X2+I(X1^2)+I(X2^2)+X1:X2, data = df_cake)
summary(lm_cake)
###Beta_5 = 0 
#F* = (SSR(X5| X1,X2,X3,X4)/(N-5) - (N-6))/(SSE(X1,X2,X3,X4,X5)/N-6)
anova(lm_cake)
#from the anova(lm_cake) we get SSE = 1.4707 with n-6 = 8 MSE = 0.1838
Anova(lm_cake)
SSR(X5| X1,X2,X3,X4) = 2.7722
F_stat <- (2.7722/1) /(1.4707/8) # F_stat= 15.07962 
qf(0.95, 1, 8) ##5.317655 F-value 
#F-stat > f-value conclude alternate hypothesis
pf(q=15.07962, df1=1, df2=8,lower.tail=FALSE)
qf(0.95, 2, 16) 
#p-value is 0.004186155 > alpha hence conclude alternate hypothesis

##QUestion part beta2 =0
#F* = (SSR(X3| X1,X2,X4,X5)/(N-5) - (N-6))/(SSE(X1,X2,X3,X4,X5)/N-6)
Anova(lm_cake)
F_stat_beta_2 <- 2.9077/(1.4707/8)##15.81669
qf(0.95,1,8) # 5.317655
p_value <- pf(15.81669, 1, 8, lower.tail = FALSE) #0.004078661
# reject null hypothesis

###β1 =β2 =β5 =0
#F_stat_beta125 = SSR(X1|X3X4) + SSR(X2|X1X3X4) + SSR(X5|X1X2X3X4)/3/SSE(X1X2X3X4X%)/df_total
lm_cake_1 <- lm(Y~X2+I(X2^2)+X1+I(X1^2)+X1:X2, data = df_cake)
summary(lm_cake_1)
anova(lm_cake_1)
F_stat_beta235 <- ((4.3232 + 2.9077+ 2.7722)/3)/ (1.4707/8) #18.13758
qf(0.95,1,8) # 5.317655
p_value <- pf(18.13758, 1, 8, lower.tail = FALSE) #0.002765803
#reject null hypothesis.


###Question 6

View(landrent)
plot(landrent)
cor(landrent)

corrplot(cor(landrent), method="number")
lm_landrent <- lm(Y~.,data=landrent)
summary(lm_landrent)
residualPlots(lm_landrent)
shapiro.test(lm_landrent$residuals)
#the coefficient of x4 is not that significant. hence drop it

mod1 <- lm(Y~X1+X2+X3, data=landrent)
summary(mod1)
residualPlots(mod1)
ncvTest(mod1)
shapiro.test(mod1$residuals)
mod2 <- lm(Y~X1*X2*X3,data=landrent)
summary(mod2)
mod3 <- lm(Y~X1+X2+X3+X1:X2,data=landrent)
summary(mod3)
ncvTest(mod3)
shapiro.test(mod3$residuals)
resettest(mod1, power=2, type="regressor")
mod4 <- lm(Y~X1+X2+X3+I(X1^2),data=landrent)
summary(mod4)
ncvTest(mod4)
shapiro.test(mod4$residuals)
residualPlots(mod4)

mod5 <- lm(Y~X1+X2+I(X2^2),data=landrent)
summary(mod5)
ncvTest(mod5)
shapiro.test(mod5$residuals)
residualPlots(mod5)


mod6 <- lm(Y~X1+X2+I(X2^2),data=landrent)
summary(mod6)
ncvTest(mod6)

residualPlots(mod6)

##calculate sse each case
lm_cake_2 <- lm(Y~X2+I(X2^2), data = df_cake)
anova(lm_cake_2)

lm_cake_3 <- lm(Y~X2+I(X2^2)+X1, data = df_cake)
anova(lm_cake_3)

#Question 7
msan601backwardStepwise = function(dataSet) {
  alpha_level = 0.05
  df_all <- data.frame()
  df_rp <- data.frame(dataSet)
  nums <- sapply(df_rp, is.numeric)
  df_rp <- df_rp[ , nums]
  df_all <- df_rp
  df_removed <- data.frame()
  initial_fit <- lm(quality~., data = df_all)
  df_removed <- data.frame(df_rp$quality)
  names(df_removed)[1] = 'quality'
  values <- (coef(summary(initial_fit))[,4])
  idx <- which.max(values[2:length(values)])
  current_p_value <- values[idx+1]
  while (current_p_value > alpha_level){
    s <- names(values[idx+1])#read the name
    cat("Column Removed = ", s, "\n")

    df_removed[,s] <- df_rp[s]# add to the removed items
    #remaining model evaluate the p-value of the remaining things
    it <- which(names(df_rp) == s)
    k <- names(df_rp)[it]
    #remove from original
    df_rp <- subset(df_rp, select=-it)
    # go through the forward iteration
    name_col_min <- ''
    vec_colNames <- c()
    vec_p_values <- c()
    found <- FALSE
    if (ncol(df_removed) >= 3){
      #add each removed to df_temp and check model values
      
      df_temp <- df_rp
      
      for(i in 2: length(names(df_removed))){

          df_temp <- data.frame()
          df_temp <- df_rp
          name_col_add<- names(df_removed)[i]
          df_temp[,name_col_add] <-df_removed[name_col_add]
          names(df_temp)
          lm_forw <- lm(quality~., data=df_temp)
          forw_vals <- (coef(summary(lm_forw))[,4])
          arr_forw <- forw_vals[2:length(forw_vals)]
          curr_pvalue <- arr_forw[which.max(arr_forw)]
          if( curr_pvalue < alpha_level){
              #save dataframe and lm
              vec_p_values <- append(vec_p_values, curr_pvalue)
              vec_colNames <- append(vec_colNames, name_col_add) 
              found =TRUE
          }
      }
      if(found == TRUE){
        ix <- which.min(vec_p_values)
        name_col_min <- vec_colNames[ix]
        min_p_value <- vec_p_values[ix]
      }
    }
    
    if(found ==TRUE && name_col_min != '' && min_p_value < current_p_value){
      found <- FALSE
      df_rp[,name_col_min] <- df_all[s]# add to the model
      ik <- which(names(df_removed) == name_col_min)
      k <- names(df_removed)[ik]
      #remove from removed items
      df_removed <- subset(df_removed, select=-k)
    }
    
    lm_new <- lm(df_rp$quality~., data=df_rp)
    values <- (coef(summary(lm_new))[,4])
    idx <- which.max(values[2:length(values)])
    current_p_value <- values[idx+1]
  }
  
  cat("\n","Final list of columns retianed:" ,names(df_rp))
}
msan601backwardStepwise(Rateprof)

#question 8
SetColumnNames = function(numOfCols){
  nameVector <- c()
  nameVector <- append(nameVector, 'Y')
  for(i in 1:numOfCols){
    nameVector <- append(nameVector, paste('X', i, sep=''))
  }
  return(nameVector)
}
msan601residualTest = function(dataSet) {
  df_frame <- dataSet
  num <- ncol(df_frame)-1
  names(df_frame) <- SetColumnNames(num)
  colIndex <- c(1:num)
  pow <- c(1.2, 1.5, 1.7)
  cat('','power','Test stat','Pr(>|t|)','\n')
  for(i in 1:num){
    for(j in 1:3){
      formula <- paste ('Y~.+','I(X',colIndex[i],'^',pow[j],')', sep='')
      lm_fit <- lm(formula, data=df_frame)
      str_1 <- paste('X',i, sep='')
      p_val <- coef(summary(lm_fit))[,"Pr(>|t|)"][4]
      t_val <- coef(summary(lm_fit))[,"t value"][4]
      cat(str_1, " ", pow[j], "  " , t_val, "  ", p_val, "\n")
    }
  }
}

msan601residualTest(landrent)


#Question 9



msan601partials = function(dataSet){
  df_ld <- data.frame(dataSet)
  num_x_cols <- ncol(df_ld) - 1
  names(df_ld) <- SetColumnNames(num_x_cols)
  #Type 1 Print the single values 
  for( i in 1:num_x_cols){
    formula = paste('Y~','X',i, sep='')
    fit <- lm(formula, data=df_ld)
    summary(fit)$r.squared
    str = paste('X',i,' = ',summary(fit)$r.squared,sep='')
    print(str)
  }
  
  
  #iterate through all the possible combinations
  for(k in 1:num_x_cols){
    num_sel_cols <- k
    comb = combinations(num_x_cols, num_sel_cols)
    n_comb_rows <- nrow(comb)
    comb_vec<- NULL
    #create a vector of combination values
    for(h in 1:nrow(comb)){
      st <- paste(comb[h,], collapse = '#')
      comb_vec <- append(comb_vec , st)
    }
    #This iterates through all the possible columns selected
    for(i in 1:num_x_cols){
      for(j in 1:n_comb_rows){
        found = FALSE
          vec <- strsplit(comb_vec[j], "#")
          vec <- unlist(vec)
          for(elem in vec){
            if(i == elem){
              found = TRUE
              break;
            }
          }
        #Get the vector of variables for the formula
        if(!found){
          col_vec <- vector('character')
          col1 <- paste('X', i, sep='')
          col_vec[1] <- col1
          vec1 <- strsplit(comb_vec[j], "#")
          vec1 <- unlist(vec)
          ik<-2
          for(ele in vec1){
            col_vec[ik] <- paste('X', ele, sep='')
            ik <- ik+1
          }
          #Construct the formula
          str_1 = 'Y~'
          str_2 <- paste(col_vec, collapse='+')
          formula_str <- paste(str_1, str_2, sep='')

          #Calculate the partial coefficient of determination 
          fit <- lm(formula_str, data=df_ld)
          numer = Anova(fit)$'Sum Sq'[1]
          denom = Anova(fit)$'Sum Sq'[length(Anova(fit)$'Sum Sq')]
          val = numer/denom
          str_comb_vec <- paste(vec1, collapse='')
          str <- paste('X', i, '|', str_comb_vec ,' = ', val,sep='')
          print(str)
        }
      }
    }
  }
}
msan601partials(landrent)
