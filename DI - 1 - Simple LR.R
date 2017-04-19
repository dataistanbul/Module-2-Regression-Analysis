#get working directory
getwd()

#change working directory
setwd("/Users/Ozgur/Dropbox/0 - Current Semester/Bike-Sharing-Dataset")

#check to see if the needed file is there
list.files()

#read the daily bike rental file in
daily <- read.csv('day.csv')
names(daily)

#Install Packages ggplot2 and GGally
install.packages('ggplot2')
library(ggplot2)
install.packages('GGally')
library(GGally)

#Create a scatterplot of temperature vs total renters per day
with(daily, plot(temp, cnt)) + 
  title(main="Temperature vs No of Bike Rentals (per day)")

#Calculate the correlation between temperature (temp) vs total renters per day (cnt)
with(daily, cor(temp,cnt))

#Correlation Matrix for the continuous variables
cor(daily[10:16]) 
#pairs(daily[10:16])


#Scatterplot Matrix for the continuous variables
ggpairs(daily,columns=10:16)

#Model1 is the linear regression model between cnt and temp
model1 <- lm(cnt ~ temp,data=daily)

#Make sure that reporting is done in four digit precision
options(digits=4)

#Summary Statistics for the Regression Model
summary(model1)
names(model1)
model1$coefficients[1]

daily$cnt[700]
model1$residuals[700]
model1$fitted.values[700]

coef(model1)
resid(model1)
fitted(model1)

confint(model1,level=.95)
anova(model1)


#Let's the data and the regression line together
plot(cnt~temp, data=daily)
abline(model1,col='red')

#Let's examine the residuals for each x variable
plot(daily$temp, resid(model1)) +
  abline(0,0,col='red')



