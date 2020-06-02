install.packages("readr")
library(readr)
cal <-read.csv("E:\\data sets\\cal.csv")
View(cal)
# EDA(Exploritary Data Analysis)
summary(cal)
# scatter plot# plot(X,Y)
plot(cal$Weight.gained..grams.,cal$Calories.Consumed)
attach(cal)
# correlation coefficient
cor(Weight.gained..grams.,Calories.Consumed)
#  simple linear regression
reg <- lm(Calories.Consumed ~ Weight.gained..grams.)
summary(reg)
pred <- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(mean(reg$residuals^2))/nrow(cal)
confint(reg,level=0.95)
predict(reg,interval = "predict")
attach(cal)
library(ggplot2)
ggplot(cal,aes(Weight.gained..grams.,Calories.Consumed))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')
# Logrithamic Model
# Logarithm model
plot(log(cal$Weight.gained..grams.,cal$Calories.Consumed))
cor(log(cal$Weight.gained..grams., cal$Calories.Consumed))
reg_log <-lm(cal$Calories.Consumed ~ cal$Weight.gained..grams.)
summary(reg_log)
predict(reg_log)
reg_log$residuals
mean(reg_log$residuals)
sum(reg_log$residuals)
sqrt(mean(reg_log$residuals^2))/nrow(cal)
confint(reg_log,level=0.95)
predict(reg_log,interval = "predict")
# polynomial model with 2 degree
plot(Weight.gained..grams.,Calories.Consumed)
plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed)
cor(Weight.gained..grams.*Weight.gained..grams., Calories.Consumed)
plot(Weight.gained..grams., log(Calories.Consumed))
cor(Weight.gained..grams., log(Calories.Consumed))
cor(Weight.gained..grams.*Weight.gained..grams.,log(Calories.Consumed))
reg2degree <- lm(log(Calories.Consumed)) ~ Weight.gained..grams. + I(Weight.gained..grams.*Weight.gained..grams.)
summary(reg2degree)
logpol <- predict(reg2degree)
expy <- exp(logpol)
err= Calories.Consumed - expy 

sqrt(sum(err^2)/nrow(cal))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

