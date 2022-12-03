##### START Random Forest modeling #####
library('pacman')
library('dplyr')
library('ggplot2')
p_load('tidyverse')
require('lubridate')
library('caret')
library('randomForest')
library('party')
library('precrec')
library('devtools')
library('reprtree')
library('ROCR')

##### START SVM modeling #####
data <- read_csv('data/in_out_patient_data_agg.csv',
                 col_types = cols(.default = 'n',
                                  Provider = 'c',
                                  PotentialFraud = 'f')) %>% drop_na()
data <- data %>% select(-contains("County"))
data <- data %>% select(-contains("State"))
data <- data %>% select(-c(Provider))

dt = sort(sample(nrow(data), nrow(data)*.8))
train<-data[dt,]
test<-data[-dt,]

library(e1071)
set.seed(1234)
#svm_tune <- tune.svm(PotentialFraud ~ ., 
#                     data = APDtrain,
#                     gamma = c(0.5,5,10,15,25,50,100,250,500),
#                     cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100, 500))
#
#svm_tune$best.parameters


svmfit = svm(PotentialFraud ~ ., data = train, kernel = "radial", gamma = 0.5, cost=5)
print(svmfit)

predict_svm <- predict(svmfit, test)
confusionMatrix(data=predict_svm, reference = test$PotentialFraud, positive="Yes")
##### END SVM modeling #####

##### START Ensemble Learning #####
set.seed(1234)
ensemble_data <- read_csv('data/in_out_patient_data_agg.csv',
                          col_types = cols(.default = 'n',
                                           Provider = 'c',
                                           PotentialFraud = 'f')) %>% drop_na()

dt = sort(sample(nrow(ensemble_data), nrow(ensemble_data)*.8))
train<-ensemble_data[dt,]%>% select(!c(Provider))
test<-ensemble_data[-dt,]%>% select(!c(Provider))

# Random Forest 
rf <- randomForest(PotentialFraud ~ ., data=train, importance=TRUE, proximity=TRUE,
                   mtry = 33, ntree = 300, maxnodes = 20) 

# Neural Network
fit.mlp <- train(f, data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 5, decay = 1),
                 metric = "Accuracy",
                 na.action=na.exclude)

# SVM 
svm_train <- train %>% select(-contains("County"))
svm_train <- svm_train %>% select(-contains("State"))
svm_test <- test %>% select(-contains("County"))
svm_test <- svm_test %>% select(-contains("State"))
svmfit = svm(PotentialFraud ~ ., data = svm_train, kernel = "radial", gamma = 0.5, cost=5)

#Predicting the out of fold prediction probabilities for training data
train$OOF_pred_rf <- as.dataf.frame(predict(rf, train, type = 'prob'))$Yes
train$OOF_pred_nn <- predict(fit.mlp, train, type= 'prob') # Accuracy of training
train$OOF_pred_svm <- predict(svmfit, svm_train, type = 'prob')

#Predicting probabilities for the test data
test$OOF_pred_rf <- as.data.frame(predict(rf, test, type = 'prob'))$Yes
test$OOF_pred_nn <- predict(fit.mlp, test, type= 'prob') # Accuracy of training
test$OOF_pred_svm <- predict(svmfit, svm_test, type = 'prob')

#Taking average of predictions
test$pred_avg<-(test$OOF_pred_rf+test$OOF_pred_nn$Yes+as.numeric(test$OOF_pred_svm))/3

#Splitting into binary classes at 0.5
test$pred_avg<-as.factor(ifelse(test$pred_avg>0.5,'Yes','No'))

confusionMatrix(data=test$pred_avg, reference = test$PotentialFraud, positive="Yes")

##### END Ensemble Learning #####

##### START Model Comparison #####
nn_pred <-  predict(fit.mlp,test) # Accuracy of training
confusionMatrix(data = nn_pred, reference = test$PotentialFraud, positive = "Yes", mode = "everything")
rf_pred <-  predict(rf,test) # Accuracy of training
confusionMatrix(data = rf_pred, reference = test$PotentialFraud, positive = "Yes", mode = "everything")
svm_pred <-  predict(svmfit,svm_test) # Accuracy of training
confusionMatrix(data = svm_pred, reference = test$PotentialFraud, positive = "Yes", mode = "everything")

# It is observed that model accuracy stays almost the same when using Ensemble. However, a much better sensitivity 
# is obtained, with the ensemble identifying fraudulent cases more accurately 
##### END Model Comparison #####
