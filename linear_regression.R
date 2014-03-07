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

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
