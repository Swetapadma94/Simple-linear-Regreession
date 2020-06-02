library(readr)
sal<- read.csv("E:\\data sets\\salarydata.csv")
View(sal)
# Exploratory Data analysis
summary(sal)
attach(sal)
#Scatter plot
attach(sal)
plot(sal$YearsExperience,sal$Salary)
#Correlation Coefficient (r)
cor(YearsExperience,Salary)
# Simple Linear Regression model
reg <- lm(sal$Salary ~ sal$YearsExperience )
summary(reg)
pred <- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(mean(reg$residuals)^2)
confint(reg, level = 0.95)
predict(reg,interval = "predict")
attach(sal)
library(ggplot2)
ggplot(data = sal, aes(x = sal$YearsExperience, y = sal$Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal, aes(x=sal$YearsExperience, y=pred))

ggplot(sal,aes(YearsExperience,Salary))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')
# Logrithamic Model
# x = log(yearsofexp); y = salary
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)
reg_log <- lm(Salary ~ log(YearsExperience))
summary(reg_log)
predict(reg_log)
reg_log$residuals
sum(reg_log$residuals)
sqrt(sum(reg_log$residuals)^2)/nrow(sal)
sqrt(sum(mean(reg_log$residuals))^2)
confint(reg_log,level= 0.95)
predict(reg_log,interval = "confidence")
# Exponential Model
# x = yearsofexp and y = log(sal)
plot(sal$YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))
reg_exp <- lm(log(Salary) ~ YearsExperience)
summary(reg_exp)
reg_exp$residuals
sum(reg_exp$residuals)
sqrt(sum(mean(reg_exp$residuals)^2))
sqrt(sum(reg_exp$residuals)^2)/nrow(sal)
logat <- predict(reg_exp)
salr <- exp(logat)
error = sal$Salary - salr
error
sqrt(sum(error)^2)/nrow(sal)
confint(reg_exp,level= 0.95)
predict(reg_exp,interval= "confidence")
# Polynomial model with 2 degree (quadratic model)
plot(sal$YearsExperience,sal$Salary)
plot(YearsExperience*YearsExperience, sal$salary)
cor(sal$YearsExperience*sal$YearsExperience,sal$Salary)
plot(sal$YearsExperience*sal$YearsExperience, log(sal$Salary))
cor(sal$YearsExperience, log(sal$Salary))
cor(sal$YearsExperience*sal$YearsExperience, log(sal$Salary))
reg2degree <- lm(log(sal$Salary) ~ sal$YearsExperience + I(sal$YearsExperience*sal$YearsExperience))
summary(reg2degree)
logpol <- predict(reg2degree)
expy <- exp(logpol)
err= sal$Salary - expy
err
sqrt(sum(err^2)/nrow(sal))
confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")
# visualization
ggplot(data = sal, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal, aes(x=YearsExperience+I(YearsExperience^2), y=logpol))
#  Polynomial model with 3 degree

reg3degree<-lm(log(Salary)~YearsExperience + I(YearsExperience*YearsExperience) + I(YearsExperience*YearsExperience*YearsExperience))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
library(ggplot2)
ggplot(data = sal, aes(x = YearsExperience + I(YearsExperience^2) + I(YearsExperience^3), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal, aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3), y=expy3))


