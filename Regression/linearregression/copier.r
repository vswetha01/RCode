copier_data <- read.csv('copierMaintenanceData.csv', header = FALSE)
colnames(copier_data)[1] <- 'Y'
colnames(copier_data)[2] <- 'X'
head(copier_data)
copier_data_swap <- copier_data[, c('X', 'Y')]
S_xy <- cov(copier_data_swap$X, copier_data_swap$Y)
S_xx <- var(copier_data_swap$X)
S_yy <- var(copier_data_swap$Y)
b1 = S_xy/S_xx

b0 = mean(copier_data_swap$Y) - b1 * mean(copier_data_swap$X)
################################################################################################

yhat = b0 + b1*copier_data_swap$X
copier_data_swap$Y_hat <- yhat
ei <- copier_data_swap$Y - copier_data_swap$Y_hat
copier_data_swap$ei <- ei
copier_data_swap$ei_sq <- copier_data_swap$ei^2 ## * copier_data_swap$ei
#y_hat = -.5801567 + 15.03525x = b0 + b1x= ^y
sum_ei_sq <- sum(copier_data_swap$ei_sq)
################################################################################################



# test hypothesis
################################################################################################
SSE = sum((copier_data_swap$Y)^2) - b0 * sum(copier_data_swap$Y) - b1 * sum(copier_data_swap$Y * copier_data_swap$X)
n <- length(copier_data_swap$Y)
#n = 45
MSE = SSE/(n-2)
#79.45063
X_bar = mean(copier_data_swap$X)
Denom <- sum((copier_data_swap$X - X_bar) *(copier_data_swap$X - X_bar))
MSE/Denom
sb1 <- sqrt(MSE/Denom)
t_value <- qt(.975, (n-2))
################################################################################################
t_stat <- b1/sb1 
n <- length(copier_data_swap$Y)
p_value <- pt(-abs(t_stat),df=n-2)
p_value <- 2 * p_value
################################################################################################

alpha <- 0.05
t_valu_1_sided <- qt(.95, n-2)
#test_stat  > t_valu_1_sided hence reject H0 and conclude Ha taht Beat 1 is positive
################################################################################################
t_stat_1 <- (b1 - 1)/sb1 
p_value_1 <- pt(-abs(t_stat_1),df=n-1)
################################################################################################
##Expected mean service time of the 
Y_h_hat_5 <- b0 + b1* 5
X_h <- 5
n<- length(copier_data_swap$X)
S2_Y_h<- MSE * (1/n + (X_h - X_bar)^2/sum((copier_data_swap$X - X_bar)^2))
S_Y_H <- sqrt(S2_Y_h)
################################################################################################
#1.768451
t_value <- qt(.975, (n-2))
ci_low <- Y_h_hat_5 - t_value*S_Y_H
ci_hi <- Y_h_hat_5 + t_value*S_Y_H
------
#####################################################################################
f_value <- qf(0.95, 1, n-2)
copier_data_swap$X_i_X_bar <- (copier_data_swap$X - X_bar)^2
SSR <- b1^2 * sum(copier_data_swap$X_i_X_bar)
MSR<- SSR
F_stat <- MSR/MSE
#####################################################################################
#Plot
t_stat^2
F_stat

#####################################################################################
with(copier_data_swap, {plot(X, Y, xlab="Number of photocopiers",
                    ylab = "Total minutes spent", main = "Copier plot data")})
abline(a = b0, b = b1, col = 'blue')
b0_1 <- 2* b0
b1_1 <- 13.00
abline(a = b0_1, b = b1_1, col = 'red')

b0_2 <- 8
b1_2 <- 16.00
abline(a = b0_2, b = b1_2, col = 'green')
#####################################################################################

################################################################################################

yhat = b0_1 + b1_1*copier_data_swap$X
copier_data_swap$Y_hat_1 <- yhat
copier_data_swap$ei_sq_1  <- (copier_data_swap$Y - copier_data_swap$Y_hat_1)^2
sum_ei_sq <- sum(copier_data_swap$ei_sq_1)
################################################################################################

################################################################################################

yhat = b0_2 + b1_2*copier_data_swap$X
copier_data_swap$Y_hat_2 <- yhat
copier_data_swap$ei_sq_2  <- (copier_data_swap$Y - copier_data_swap$Y_hat_2)^2
sum_ei_sq <- sum(copier_data_swap$ei_sq_2)
################################################################################################

