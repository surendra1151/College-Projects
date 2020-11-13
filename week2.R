library(ISLR)
data(Carseats)
fit3 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit3)
sales = Carseats$Sales
 price = Carseats$Price     
 cor(sales, price)
 require(dplyr)
 Carseats <- Carseats %>%
   mutate(US= ifelse(US == "No",0,1))
  us=Carseats$US
 cor(sales,us)
 library(ISLR)
 library(MASS)
 attach(Default)
 set.seed(1)
 lr<-glm(default~income+balance,family = binomial,data=Default)
 summary(lr)
 
 
 subset<-sample(nrow(Default),nrow(Default)*0.9)
 default.train<-Default[subset,]
 default.test<-Default[-subset,]
 lr.90<-glm(default~income+balance,family = binomial,data=default.train)
 predict.90<-predict(lr.90,default.test,type="response")
 class.90<-ifelse(predict.90>0.5,"Yes","No")
 table(default.test$default,class.90,dnn=c("Actual","Predicted"))
 round(mean(class.90!=default.test$default),4)
 
 set.seed(1)
 subset<-sample(nrow(Default),nrow(Default)*0.7)
 default.train<-Default[subset,]
 default.test<-Default[-subset,]
 lr.70<-glm(default~income+balance+student,family = binomial,data=default.train)
 summary(lr.70)
 predict.70<-predict(lr.70,default.test,type="response")
 class.70<-ifelse(predict.70>0.7,"Yes","No")
 table(default.test$default,class.70,dnn=c("Actual","Predicted"))
 round(mean(class.70!=default.test$default),4)