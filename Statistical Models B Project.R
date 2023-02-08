setwd("C:/Users/sr115/Documents/Uni/Stats Models B")
sugar=read.table("sugar.txt",header=TRUE)
yield=sugar[,1] ###Defining Column Variables
area=sugar[,2]
duration=sugar[,3]
rainfall=sugar[,4]
sugar #Sugar Table
#
summary(yield) ##Summary of data variables and Standard deviations
sd(yield)
summary(area)
sd(area)
summary(duration)
sd(duration)
summary(rainfall)
sd(rainfall)
par(mfrow=c(2,2)) ##Plotting Histograms of variables
hist(yield)#Histogram of yield
hist(area)#Histogram of area
hist(duration)#Histogram of duration
hist(rainfall)#Histogram of rainfall
#
par(mfrow=c(1,1)) ##Boxplot of the data set to show range and quartiles
boxplot(sugar)
#
pairs(sugar)#Graphs to show any potential relationship between variables
#
regression=lm(formula = log(yield)~log(area)+rainfall+duration) #Initial Regression Model
summary(regression)
summary.aov(regression)
anova(regression)
#
regression2=lm(formula = log(yield)~log(area)) #Simplified Regression Model
summary(regression2)
summary.aov(regression2)
#
par(mfrow=c(2,2))
plot(regression2,which=1:4) ## Checking Validity of Model with quartile analysis and checking for outliers in nodes
#
confint(regression)
confint(regression2)
