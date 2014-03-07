library(MASS)
library(ISLR)


fix(Boston)
names(Boston)


lm.fit=lm(medv~lstat,data=Boston)

summary(lm.fit)
