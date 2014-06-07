#Loading Data
prostate_cancer <- read.table("~/Documents/github/stat_learning/data/prostate_caner.dat", quote="\"")
colnames(prostate_cancer)[1] <- "idCode"
colnames(prostate_cancer)[2] <- "tumor"
colnames(prostate_cancer)[3] <- "age"
colnames(prostate_cancer)[4] <- "race"
colnames(prostate_cancer)[5] <- "rectalExamResult"
colnames(prostate_cancer)[6] <- "capsularInvolvement"
colnames(prostate_cancer)[7] <- "antigenValue"
colnames(prostate_cancer)[8] <- "tumorVolume"
colnames(prostate_cancer)[9] <- "gleasonScore"

prostate_cancer$race <- as.factor(prostate_cancer$race)
prostate_cancer$rectalExamResult <- as.factor(prostate_cancer$rectalExamResult)


attach(prostate_cancer)


##Validation Set
train=sample(380,200)
glm.fit=glm(tumor~age+race+rectalExamResult+capsularInvolvement+antigenValue+gleasonScore,data=prostate_cancer,family = binomial,subset=train)
mean((tumor-predict(glm.fit,prostate_cancer, type="response"))[-train]^2)

##LOOCV
library(boot)
glm.fit=glm(tumor~age+race+rectalExamResult+capsularInvolvement+antigenValue+gleasonScore,data=prostate_cancer,family = binomial)
coef(glm.fit)
kfCV <- cv.glm(data=prostate_cancer, glmfit=glm.fit)
kfCV$delta

i <-  2
#Polynomial Logistic Regression LOOCV
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(tumor~age+race+rectalExamResult+capsularInvolvement+poly(antigenValue,i)+poly(gleasonScore,i),data=prostate_cancer,family = binomial)
  cv.error[i]=cv.glm(data=prostate_cancer, glmfit=glm.fit)$delta[1]
}

cv.error


#Polynomial Logistic Regresion CV K=5
cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(tumor~poly(age,i)+race+rectalExamResult+capsularInvolvement+antigenValue+gleasonScore,data=prostate_cancer,family = binomial)

  cv.error[i]=cv.glm(data=prostate_cancer, glmfit=glm.fit,K=5)$delta[1]
}



#Estimate model parameters via boostrapping
library(boot)
boot.fn=function(data,index)
 coefficients(glm(tumor~age+capsularInvolvement+antigenValue+gleasonScore,data=prostate_cancer,family = binomial,subset=index)) 
set.seed(1)
boot(prostate_cancer,boot.fn,1000)
