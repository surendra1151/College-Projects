data <- read.csv("/Users/kishoresmacbook/Desktop/R Projects/Bias_correction_ucl.csv")

data[is.na(x = data)] <- 0

library(Metrics)
library(dplyr)
library(class)
library(ggplot2)

set.seed(100)
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))  # row indices for training data
trainingData <- data[trainingRowIndex, ]  # model training data
testData  <- data[-trainingRowIndex, ]   # test data

#Multiple Linear Regression

#Tmax Prediction

trainingModel <- lm(Next_Tmax ~ Present_Tmax + LDAPS_RHmax + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + lat + lon + Slope , data = trainingData)
NextTmaxPredict <- predict(trainingModel, testData)  # predict distance
NextTmaxPredict

summary(trainingModel)

AIC (trainingModel)

actuals_preds <- data.frame(cbind(actuals=testData$Next_Tmax, predicteds=NextTmaxPredict))  # make actuals_predicteds dataframe.

correlation_accuracy <- cor(actuals_preds)  # 82.7%

head(actuals_preds)

MSETmax <- mean((testData$Next_Tmax-NextTmaxPredict)^2, na.rm = TRUE)

MSETmax



MSE_Tmax_Individual = (testData$Next_Tmax - as.numeric(NextTmaxPredict))

plot_Tmax <- data.frame(testData$Next_Tmax,MSE_Tmax_Individual)
ggplot(plot_Tmin,aes(x= testData$Next_Tmax,y= MSE_Tmax_Individual))+
  geom_point() +
  geom_line(color="blue") +
  xlab("Next_Tmin") + 
  ylab("MSE") +
  ggtitle("Multiple regression - error vs Next_Tmax")

#Tmin Prediction

trainingModelTmin <- lm(Next_Tmin ~ Present_Tmin + LDAPS_RHmin + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + lat + lon + Slope , data = trainingData)
NextTminPredict <- predict(trainingModelTmin, testData)  # predict distance
NextTminPredict

summary(trainingModelTmin)

AIC (trainingModelTmin)

actuals_preds_Tmin <- data.frame(cbind(actuals=testData$Next_Tmin, predicteds=NextTminPredict))  # make actuals_predicteds dataframe.

correlation_accuracy <- cor(actuals_preds_Tmin)  # 82.7%

head(actuals_preds_Tmin)

MSETmin <- mean((testData$Next_Tmin-NextTminPredict)^2, na.rm = TRUE)

MSETmin



MSE_Tmin_Individual = (testData$Next_Tmin - as.numeric(NextTminPredict))

plot_Tmin <- data.frame(testData$Next_Tmin,MSE_Tmin_Individual)
ggplot(plot_Tmin,aes(x= testData$Next_Tmin,y= MSE_Tmin_Individual))+
  geom_point() +
  geom_line(color="blue") +
  xlab("Next_Tmin") + 
  ylab("MSE") +
  ggtitle(" Multiple regression - error vs Next_Tmin")


#Simple Linear Regression

#Tmax Prediction

slr_trainingModel <- lm(Next_Tmax ~ Present_Tmax, data = trainingData)
slr_NextTmaxPredict <- predict(slr_trainingModel, testData)  # predict distance
slr_NextTmaxPredict

summary(slr_trainingModel)

AIC (slr_trainingModel)

slr_actuals_preds <- data.frame(cbind(actuals=testData$Next_Tmax, predicteds=slr_NextTmaxPredict))  # make actuals_predicteds dataframe.

slr_correlation_accuracy <- cor(slr_actuals_preds)  # 82.7%

head(slr_actuals_preds)

slr_MSE <- mean((testData$Next_Tmax-slr_NextTmaxPredict)^2, na.rm = TRUE)

slr_MSE

slr_MSE_Tmax_Individual = (testData$Next_Tmax - as.numeric(slr_NextTmaxPredict))

plot_Tmax <- data.frame(testData$Next_Tmax,slr_MSE_Tmax_Individual)
ggplot(plot_Tmin,aes(x= testData$Next_Tmax,y= slr_MSE_Tmax_Individual))+
  geom_point() +
  geom_line(color="blue") +
  xlab("Next_Tmax") + 
  ylab("MSE") +
  ggtitle("simple regression - error vs Next_Tmax")

#Tmin Prediction

slr_trainingModelTmin <- lm(Next_Tmin ~ Present_Tmin , data = trainingData)
slr_NextTminPredict <- predict(slr_trainingModelTmin, testData)  # predict distance
slr_NextTminPredict

summary(slr_trainingModelTmin)

AIC (slr_trainingModelTmin)

slr_actuals_preds_Tmin <- data.frame(cbind(actuals=testData$Next_Tmin, predicteds=slr_NextTminPredict))  # make actuals_predicteds dataframe.

correlation_accuracy <- cor(slr_actuals_preds_Tmin)  # 82.7%

head(slr_actuals_preds_Tmin)

slr_MSETmin <- mean((testData$Next_Tmin-slr_NextTminPredict)^2, na.rm = TRUE)

slr_MSETmin

slr_MSE_Tmin_Individual = (testData$Next_Tmin - as.numeric(slr_NextTminPredict))

plot_Tmin <- data.frame(testData$Next_Tmin,slr_MSE_Tmin_Individual)
ggplot(plot_Tmin,aes(x= testData$Next_Tmin,y= slr_MSE_Tmin_Individual))+
  geom_point() +
  geom_line(color="blue") +
  xlab("Next_Tmin") + 
  ylab("MSE") +
  ggtitle("simple regression - error vs Next_Tmin")



#KNN

x_trn_bias = trainingData$Present_Tmax
y_trn_bias = trainingData$Next_Tmax
x_tst_tmax_bias = testData$Present_Tmax
y_tst_tmax_bias = testData$Next_Tmax
x_tst_tmin_bias = testData$Present_Tmin
y_tst_tmin_bias = testData$Next_Tmin

x_trn_bias_min = min(x_trn_bias, na.rm = TRUE)
x_trn_bias_max = max(x_trn_bias, na.rm = TRUE)

lstat_grid = data.frame(lstat = seq(x_trn_bias, x_trn_bias, 
                                    by = 0.01))

pred_tmax_005 = knn(data.frame(x_trn_bias), data.frame(y_tst_tmax_bias), y_trn_bias, k=5)

pred_tmin_005 = knn(data.frame(x_trn_bias), data.frame(y_tst_tmin_bias), y_trn_bias, k=5)

pred_tmax_005 <- as.numeric(pred_tmax_005)

pred_tmax_005

pred_tmin_005 <- as.numeric(pred_tmin_005)

pred_tmin_005

MSE_KNN_Tmax = mean((testData$Next_Tmax-as.numeric(pred_tmax_005))^2, na.rm = TRUE)

MSE_KNN_Tmin = mean((testData$Next_Tmin-as.numeric(pred_tmin_005))^2, na.rm = TRUE)



MSE_KNN_Tmax_Individual = (testData$Next_Tmax - as.numeric(pred_tmax_005))
plot_Tmax <- data.frame(y_tst_tmax_bias,MSE_KNN_Tmax_Individual)
ggplot(plot_Tmax,aes(x = y_tst_tmax_bias,y = MSE_KNN_Tmax_Individual)) + 
  geom_point() +
  geom_line(color="blue") +
  xlab("Next_Tmax") + 
  ylab("MSE") +
  ggtitle("KNN regression - error vs Next_Tmax")


library(dplyr)
library(class)
library(ggplot2)

MSE_KNN_Tmin_Individual = (testData$Next_Tmin - as.numeric(pred_tmin_005))

plot_Tmin <- data.frame(y_tst_tmin_bias,MSE_KNN_Tmin_Individual)
ggplot(plot_Tmin,aes(x= y_tst_tmin_bias,y= MSE_KNN_Tmin_Individual))+
  geom_point() +
  geom_line(color="blue") +
  xlab("Next_Tmin") + 
  ylab("MSE") +
  ggtitle("KNN regression - error vs Next_Tmin")
  


