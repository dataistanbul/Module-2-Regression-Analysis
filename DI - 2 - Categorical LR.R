getwd()
setwd("/Users/Ozgur/Dropbox/0 - Current Semester/Bike-Sharing-Dataset")
list.files()
daily <- read.csv('day.csv')
install.packages('ggplot2')
library(ggplot2)

# Categorical Variables

#Summarize the variable
summary(daily$season)

#display unique values of a column/attribute/variable
unique(daily$season)

#Is it a categorical variable or not?
is.factor(daily$season)

qplot(x=season, y=cnt,data=daily)
qplot(x=season, y=cnt, data=daily, geom='boxplot') 

#Convert season to a truely categorical variable
daily$season.f <- factor(daily$season)
is.factor(daily$season.f)
summary(daily$season.f)

qplot(x=season.f, y=cnt, data=daily, geom='boxplot') 

with(daily,levels(season.f))

#Change the order of the categorical variables
daily$season.f = factor(daily$season.f,levels(daily$season.f)[c(4,3,2,1)])
with(daily,levels(season.f))
qplot(x=season.f, y=cnt, data=daily, geom='boxplot') 

#Restore the order of the categorical variables
daily$season.f = factor(daily$season.f,levels(daily$season.f)[c(4:1)])


catmodel1 <- lm(casual ~ temp + hum + windspeed + holiday.f, data=daily)
summary(catmodel1)

catmodel11 <- lm(cnt~temp+season.f, data=daily)
summary(catmodel11)

ggplot(daily,aes(temp, cnt)) + 
  geom_point(aes(color=season.f))


#How to deal with interaction terms
summary(daily$holiday)
unique(daily$holiday)
daily$holi.f <- factor(daily$holiday)
qplot(x=holi.f, y=cnt, data=daily, geom='boxplot') 
summary(daily$holi.f)

catmodel2 <- lm(cnt~holi.f, data=daily)
summary(catmodel2)

catmodel3 <- lm(cnt~temp + holi.f, data=daily)
summary(catmodel3)

catmodel4 <- lm(cnt ~ temp + holi.f + temp*holi.f, data=daily)
summary(catmodel4)
