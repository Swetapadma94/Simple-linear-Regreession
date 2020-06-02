library(readr)
emp <- read.csv("E:\\data sets\\empdata.csv")
View(emp)
# EDA
summary(emp)
# scatter diagram # plot(X,Y)
# y<- churn_out_rate, X<- salary_hike
plot(emp$Salary_hike,emp$Churn_out_rate)
attach(emp)
# correlation coefficient
cor(Churn_out_rate,Salary_hike) #cor(y,x)
# simple linear regression # lm(y ~ X)
reg <- lm(Churn_out_rate ~ Salary_hike)
summary(reg)
pred<- predict(reg)
reg$residuals
sum(reg$residuals)
sqrt(mean(reg$residuals))
sqrt(mean(reg$residuals)^2)/nrow(emp)
confint(reg, level= 0.95)
predict(reg, interval = "predict")
attach(emp)
library(ggplot2)
ggplot(emp,aes(Salary_hike,Churn_out_rate))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')
# Logrithamic Model
# x = log(salary_hike); y = churn_out_rate
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)
reg_log <- lm(Churn_out_rate ~ log(Salary_hike))
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(mean(reg_log$residuals)^2)/nrow(emp)
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
# Polynomial model with 2 degree (quadratic model)

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate)

plot(Salary_hike*Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = emp$Churn_out_rate - expy
err
sqrt(sum(err^2)/nrow(emp))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")


