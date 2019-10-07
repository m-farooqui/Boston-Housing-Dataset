#Load the libraries
library("MASS", lib.loc="/usr/lib/R/library")
library("ISLR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
#using the command summary to see the summary of the dataset
summary(Boston)
#Using the command head to view some obervation from the Boston Housing dataset
head(Boston)
#plot a graph of the medican value of homes vs lower status of the population and the using the pair function to return scatterplot of each varaible in the dataset
plot(medv~lstat, Boston)
pairs(~ medv + ptratio + black + lstat + dis + rm + crim, data = Boston, main = "Boston Data")
#setting the variable fit1 to a linear model that shows the relationship between the mdeian values of homes vs lower status of population and using the summary function to print out the statistics
fit1=lm(medv~lstat,data=Boston)
summary(fit1)
#plot the linear function
plot(medv~lstat,Boston)
#Adding a regression line to the graph choosing the colour as red
abline(fit1,col="red")
#Confidence level of the first model
confint(fit1)
#using the lstat function to setup a confidence interval and using the predict to predict values within the given interval  
predict(fit1,data.frame(lstat=c(10,20,30)),interval="confidence")
data.frame(lstat=c(10,20,30))
#creating a personalized plot
par(mfrow=c(1,1))
plot(fit1)
#creating a second linear model by adding the variable age to the model
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
#Third model with only the median values of the houses with a customized plot
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(1,1))
plot(fit3)
#using multi linear regression to see which variables are significant 
fit4=update(fit3,~.-age-indus)
summary(fit4)
#Finding non-linear terms in the model and making a plot of the model
fit6=lm(medv~lstat +I(lstat^2),Boston)
summary(fit6)
#drawing differnet regression lines bewteen the graphs in differnet colours 
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat, Boston)
points(lstat,fitted(fit6), col="red",pch=20)
fit7=lm(medv~poly(lstat,4))
plot(medv~lstat)
points(lstat,fitted(fit7),col="blue",pch=20)
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
#final model with all the parameters
fit6=lm(medv~lstat+crim+rm+dis+black+chas+nox+rad+tax+ptratio+I(lstat^2)+I(rm^2))
summary(fit6)
par(mfrow=c(1,1))
plot(fit6)
