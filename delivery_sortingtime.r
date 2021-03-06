library(readr)
delivery <- read.csv("E:\\data sets\\deliverytime.csv")
View(delivery)
# Exploritary data analysis
summary(delivery)
# scatter diagram # plot(X,Y)
# y<- delivery time, X<- sorting time
plot(delivery$Delivery.Time,delivery$Sorting.Time)
attach(delivery)
# correlation coefficient
cor(Delivery.Time,Sorting.Time)
# simple linear regression # lm(y ~ X)
reg <- lm(Delivery.Time ~ Sorting.Time)
summary(reg)
pred<- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(mean(reg$residuals^2))/nrow(delivery)
confint(reg,level=0.95)
predict(reg, interval = "predict")
attach(delivery)
library(ggplot2)
ggplot(delivery,aes(Sorting.Time,Delivery.Time))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')
# Logarithm model
plot(log(Sorting.Time),Delivery.Time)
cor(log(Sorting.Time),Delivery.Time)
reg_log <- lm(Delivery.Time ~ Sorting.Time)
summary(reg_log)
predict(reg_log)
reg_log$residuals
mean(reg_log$residuals)
sqrt(mean(reg_log$residuals)^2)/nrow(delivery)
confint(reg,level=0.95)
predict(reg, interval= "predict")
# Exponential model # Best #
plot(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time, log(Delivery.Time))
reg_exp <- lm(log(Delivery.Time) ~ Delivery.Time) 
summary(reg_exp)
sqrt(mean(reg_exp$residuals^2))/nrow(delivery)
confint(reg_exp,level=0.95)
predict(reg_exp, interval = "predict")
# polynomial model with 2 degree
plot(Sorting.Time,Delivery.Time)
plot(Sorting.Time*Sorting.Time,Delivery.Time)
cor(Sorting.Time*Sorting.Time,Delivery.Time)  
plot(Sorting.Time*Sorting.Time, log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time*Sorting.Time, log(Delivery.Time))
reg2degree <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time*Sorting.Time))
summary(reg2degree)
sqrt(sum(mean(reg2degree$residuals^2)))/nrow(delivery)
logpol <- predict(reg2degree)
expy <- exp(logpol)
expy <- exp(logpol)
err= sal$Salary - expy
err
sqrt(sum(err^2)/nrow(sal))
confint(reg2degree, level= 0.95)
predict(reg2degree, interval = "predict")
