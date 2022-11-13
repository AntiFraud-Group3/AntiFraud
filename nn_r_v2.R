## Creating index variable 

# Read the Data
df <- read_csv('train_iobp_df.csv',
               col_types = cols(.default = 'n',
                                Provider = 'c',
                                PotentialFraud = 'f'))


colnames(df)

df <- df %>%
  mutate_if(is.numeric, scale)


train_test_split_index <- 0.8 * nrow(df)
train <- df[1:train_test_split_index,]
test <- df[(train_test_split_index+1): nrow(df),]

train_matrix <- model.matrix(~ATT_PHY_Claim_Duration + BENE_OP_Annual_Ded_Amt + ClmCount_Provider_ClmProcedureCode_1 + Claim_DiagCode10_CoPayment + ClmCount_Provider_BeneID_OtherPhysician_ClmDiagnosisCode_7 + ClmCount_Provider_OtherPhysician + Oth_Phy_tot_claims + Claim_DiagCode10_IP_Annual_Ded_Amt + PRV_Tot_DGrpCodes + Claim_DiagCode8_OP_Annual_ReImb_Amt + PRV_Bene_Age_Sum + Claim_DiagCode10_OP_Annual_ReImb_Amt + ClmCount_Provider_DiagnosisGroupCode + PRV_CoPayment + ClmCount_Provider_ClmProcedureCode_2 + Claim_DiagCode10_Claim_Duration + ClmCount_Provider_BeneID + PRV_Tot_Unq_DOB_Years + Admitted_Duration + InscClaimAmtReimbursed + PotentialFraud, data=df)
#remote intercept and label
col_list <- paste(c(colnames(train_matrix[,-c(1,22)])),collapse="+")
col_list <- paste(c("PotentialFraud1 ~",col_list),collapse="")
f <- formula(col_list)

set.seed(123456)
nmodel <- neuralnet(formula = f,
                    data = train_matrix,
                    hidden = 12,
                    stepmax = 30000,
                    threshold = 0.13,
                    learningrate.limit = NULL,
                    learningrate.factor = list(minus = 0.5, plus = 1.2),
                    algorithm = "rprop+",
                    linear.output = FALSE,
                    lifesign = 'full' )


prediction = compute(nmodel, train_matrix[,-c(1,22)])
prediction$net.result

plot(nmodel, rep = 'best')

plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5



#------ new ----
indepedent_vars <- colnames(df[,-c(1,2)])
paste(indepedent_vars, collapse=" + ")
f <- formula(paste("PotentialFraud ~", paste(indepedent_vars, collapse=" + ")))
f <- formula(paste("PotentialFraud ~", "ATT_PHY_Claim_Duration + BENE_OP_Annual_Ded_Amt + ClmCount_Provider_ClmProcedureCode_1 + Claim_DiagCode10_CoPayment + ClmCount_Provider_BeneID_OtherPhysician_ClmDiagnosisCode_7 + ClmCount_Provider_OtherPhysician + Oth_Phy_tot_claims + Claim_DiagCode10_IP_Annual_Ded_Amt + PRV_Tot_DGrpCodes + Claim_DiagCode8_OP_Annual_ReImb_Amt + PRV_Bene_Age_Sum + Claim_DiagCode10_OP_Annual_ReImb_Amt + ClmCount_Provider_DiagnosisGroupCode + PRV_CoPayment + ClmCount_Provider_ClmProcedureCode_2 + Claim_DiagCode10_Claim_Duration + ClmCount_Provider_BeneID + PRV_Tot_Unq_DOB_Years + Admitted_Duration + InscClaimAmtReimbursed"))

nmodel <- neuralnet(formula = f,
                    data = df,
                    hidden=c(7,1),
                    stepmax = 30000,
                    threshold = 0.12,
                    learningrate.limit = NULL,
                    learningrate.factor = list(minus = 0.5, plus = 1.2),
                    algorithm = "rprop+",
                    linear.output = FALSE,
                    lifesign = 'full' )
