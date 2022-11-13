p_load(caret)
p_load(ggplot2)
p_load(NeuralNetTools)
p_load(NeuralSens)
p_load(MLmetrics)


data <- read_csv('train_iobp_df.csv',
               col_types = cols(.default = 'n',
                                Provider = 'c',
                                PotentialFraud = 'f'))
#data <- data %>%
#  mutate_if(is.numeric, scale)
levels(data$PotentialFraud) <- c("No", "Yes")

train_test_split_index <- createDataPartition(data$PotentialFraud,p=0.8,list=FALSE)
data.trn <- data[train_test_split_index,-1]
data.tst <- data[-train_test_split_index,-1]

set.seed(123456)
ctrl  <- trainControl(method  = "cv",number  = 10, 
                      summaryFunction = multiClassSummary, 
                      classProbs=T,
                      savePredictions = T) 
f <- formula(paste("PotentialFraud ~", "ATT_PHY_Claim_Duration + BENE_OP_Annual_Ded_Amt + ClmCount_Provider_ClmProcedureCode_1 + Claim_DiagCode10_CoPayment + ClmCount_Provider_BeneID_OtherPhysician_ClmDiagnosisCode_7 + ClmCount_Provider_OtherPhysician + Oth_Phy_tot_claims + Claim_DiagCode10_IP_Annual_Ded_Amt + PRV_Tot_DGrpCodes + Claim_DiagCode8_OP_Annual_ReImb_Amt + PRV_Bene_Age_Sum + Claim_DiagCode10_OP_Annual_ReImb_Amt + ClmCount_Provider_DiagnosisGroupCode + PRV_CoPayment + ClmCount_Provider_ClmProcedureCode_2 + Claim_DiagCode10_Claim_Duration + ClmCount_Provider_BeneID + PRV_Tot_Unq_DOB_Years + Admitted_Duration + InscClaimAmtReimbursed"))

fit.mlp <- train(f, data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 5, decay = 1),
                 
                 metric = "ROC")
plotnet(fit.mlp$finalModel)

SensAnalysisMLP(fit.mlp)
var_importance <- varImp(fit.mlp)$importance
plot(varImp(fit.mlp), top = 20 )

train_pred <-  predict(fit.mlp,data.trn) # Accuracy of training
confusionMatrix(data = train_pred, reference = data.trn$PotentialFraud, positive = "Yes")

test_pred <-  predict(fit.mlp,data.tst) # Accuracy of training
confusionMatrix(data = test_pred, reference = data.tst$PotentialFraud, positive = "Yes")

Gfit.mlp <- train(f, data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 #tuneGrid = data.frame(size = 1, decay = 0),
                 tuneGrid = expand.grid(size = seq(3,15), decay=10^seq(-9,0,by=1)),
                 metric = "Accuracy")

Gfit.mlp #information about the resampling settings
ggplot(Gfit.mlp) + scale_x_log10()

Gfit.mlp$finalModel #information about the model trained
summary(Gfit.mlp$finalModel) #information about the network and weights
plotnet(Gfit.mlp$finalModel) #Plot the network

