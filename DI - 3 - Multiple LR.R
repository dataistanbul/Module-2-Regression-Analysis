#Transform the appropriate factors
day$weekd.f <- factor(day$weekday)
day$seas.f <- factor(day$season)
day$workingd.f <- factor(day$workingday)
day$holi.f <- factor(day$holiday)


max_model <- lm( cnt ~ temp + atemp + hum + windspeed  + seas.f + holi.f + workingd.f + weekd.f, data = day )

#Backward Selection
stepmodel1 <- step(max_model, direction="backward")

summary(stepmodel1)

#Forward Selection
min_model <- lm( cnt ~ 1 , data =  day)

stepmodel3 <- step(min_model, direction="forward", scope = list(lower=min_model, upper=max_model))

#Forward and Backward Selection
stepmodel2 <- step(max_model, direction="both")

summary(stepmodel1)
summary(stepmodel2)
summary(stepmodel3)