library(MASS)
library(ISLR)


fix(Boston)
names(Boston)


lm.fit=lm(medv~lstat,data=Boston)

lm.fit
summary(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=(c(5,10,15))),interval="confidence")

plot(Boston$lstat,Boston$medv)
abline(lm.fit)


par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))



lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)





par(mfrow=c(3,3))
plot(Boston$crim,Boston$medv)
plot(Boston$zn,Boston$medv)
plot(Boston$nox,Boston$medv)
plot(Boston$crim,Boston$medv)
plot(Boston$nox,Boston$medv)
plot(Boston$dis,Boston$medv)
plot(Boston$rad,Boston$medv)
plot(Boston$ptratio,Boston$medv)
plot(Boston$black,Boston$medv)

par(mfrow=c(2,2))
plot(lm.fit)
