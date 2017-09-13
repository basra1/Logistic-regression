setwd("C:\\Users\\MYPC\\Desktop\\edx")
framingham<-read.csv("framingham.csv")
head(framingham)
summary(framingham)
framingham<-subset(na.omit(framingham))

summary(framingham)
str(framingham)
install.packages("caTools")
library(caTools)
set.seed(1000)
sample<-sample.split(framingham$TenYearCHD,0.65)
sample
fram_train<-subset(framingham,sample=="TRUE")
fram_test<-subset(framingham,sample=="FALSE")
str(fram_train)
str(fram_test)
head(framingham)
library(ggplot2)
table(framingham$male,framingham$TenYearCHD)
## Male count is higher in CHD as compare to female.
summary(framingham$age)
table(framingham$age,framingham$TenYearCHD)
## People having age between 51 to 59 are having higher count of CHD; This is somewhat true because after age of 50 body immunity tend to change.
table(framingham$currentSmoker,framingham$TenYearCHD)
table(framingham$currentSmoker)
## this emplies that smoking is not only factor for CHD
table(framingham$prevalentStroke,framingham$TenYearCHD)
## approx 44% people are prone to CHD who have got previous heart stroke.


##Building a model
##multicollinearity
cor(fram_train)
corrplot::corrplot(cor(fram_train), method="circle")
## there is collinearity between independent variable-cigsPerDay~currentSmoker, sysBP~diaBP
framingham_log<-glm(TenYearCHD~.,data = fram_train,binomial)
summary(framingham_log)
predicttest<-predict(framingham_log,type = "response",newdata = fram_test)
table(fram_test$TenYearCHD,predicttest>0.5)
## accuracy of model using confusion matrix
(1081+18)/(1081+4+18+177)
##comparing with base line model
(1081+4)/(1081+4+177+18)
##by comparing the model the logistics model is not significant as it is predicting the result near to base line model.
##improving the model
##selecting variable using backward approch
step(glm(TenYearCHD~.-currentSmoker-diaBP,data = fram_train,binomial),direction = "backward")
framingham_log_impr<-glm(TenYearCHD~male+age+education+cigsPerDay+totChol+sysBP+glucose,data = fram_train,family = binomial)
summary(framingham_log_impr1)
## every variable is now significant.

##confusion matrix at threshold 0.5
predict_testdata<-predict(framingham_log_impr1,type="response", newdata= fram_test)
table(fram_test$TenYearCHD,predict_testdata>0.5)

install.packages("ROCR")
library(ROCR)
hist(predict_testdata)
rocr_prediction<-prediction(predict_testdata,fram_test$TenYearCHD)
eval<-performance(rocr_prediction,"tpr","fpr")

roc_curve<-plot(eval,main="ROC CURVE")
abline(h=0.65,v=0.35)
abline(a=0,b=1)

##Indentify beat slot
max<-which.max(slot(eval,"y.values")[[1]])
max
slot(eval,"y.values")[[1]][max]
slot(eval,"x.values")[[1]][max]
#AUC
auc<-performance(rocr_prediction,"auc")
auc<-unlist(slot(auc,"y.values"))
round(auc,3)
legend(.6,.2,round(auc,3),title = "AUC")
table(fram_test$TenYearCHD,predict_testdata>0.2)
