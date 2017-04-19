#How to create a piecewise linear regression line

#Install the needed package
install.packages('segmented')
library(segmented)

#Read the sample data set 

#Plot the data set
with(sample, plot(x, y))

#Create a linear regression model
lin.mod <- lm(y~x,data=sample)
summary(lin.mod)

#Specify the breaking points of the model
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=15)

#Get Summary Information
summary(segmented.mod)
plot(segmented.mod)
intercept(segmented.mod)
slope(segmented.mod)


getwd()
setwd("/Users/Ozgur/Dropbox/0 - Current Semester/Bike-Sharing-Dataset")
list.files()
daily <- read.csv('day.csv')
library(ggplot2)

with(daily, plot(temp, cnt)) + 
  title(main="Temperature vs No of Bike Rentals (per day)")

options(digits=4)

#Original plot of temp vs casual with the regression line
ggplot(daily,aes(x = temp, y = casual)) + 
  geom_point() +
  geom_smooth(method="lm")

model_no_t <- lm(casual ~ temp, data=daily)
summary(model_no_t)

new_temps <- data.frame(temp=c(.2,.5,.8))

predict(model_no_t, newdata=new_temps)

predict(model_no_t, newdata=new_temps, interval='confidence')

predict(model_no_t, newdata=new_temps, interval='confidence', level=0.99)


#Fitting a smooth curve to the data set
ggplot(daily,aes(x = temp, y = casual)) + 
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1)

model_loess <- loess(casual ~ temp, data=daily)
#summary(model_loess)
predict(model_loess, newdata=new_temps)

#Log Transformation
ggplot(daily,aes(temp, log(casual))) + 
  geom_point()

ggplot(daily,aes(log(temp), log(casual))) + 
  geom_point()


summary(model_no_t)

model_w_t <- lm(log(casual) ~ temp, data=daily)
summary(model_w_t)

model_w_tt <- lm(log(casual) ~ log(temp), data=daily)
summary(model_w_tt)
