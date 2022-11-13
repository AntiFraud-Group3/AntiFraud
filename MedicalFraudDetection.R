setwd("C:/Users/shour/Desktop/project/AntiFraud")
#setwd("~/Downloads/HKU/M1/FITE7410 Financial Fraud Detection/GitHub/AntiFraud")
getwd()

library('pacman')
library('dplyr')
library('ggplot2')

p_load('tidyverse')
require('lubridate')
library('caret')

provider_data <- read_csv('data/Train.csv')
provider_data <- provider_data %>%
  mutate(PotentialFraud = factor(ifelse(PotentialFraud == "Yes", 1, 0)))
table(provider_data$PotentialFraud) # 4904 non-fraud & 506 fraud


beneficiary_data <- read_csv('data/Train_Beneficiarydata.csv',
                             col_types = cols(.default = 'n',
                                              BeneID = 'c',
                                              DOB = 'D',
                                              DOD = 'D',
                                              # Factorized Gender
                                              Gender = 'f',
                                              Race = 'f',
                                              State = 'f',
                                              # kept the type as chr for now
                                              RenalDiseaseIndicator = 'c',
                                              County = 'f'))
beneficiary_data <- beneficiary_data %>%
  mutate_at(vars(matches("ChronicCond")), ~ifelse(. == "1", 0, 1)) %>%
  mutate_at(vars(matches("ChronicCond")), as.factor) %>%
  # changed 'Y' to 1 and factorize
  mutate_at("RenalDiseaseIndicator", ~ifelse(. == 'Y', 1, 0)) %>% 
  mutate_at("RenalDiseaseIndicator", as.factor) %>% 
  # Removed NoOfMonths_PartACov & NoOfMonths_PartBCov
  select(-c("NoOfMonths_PartACov", "NoOfMonths_PartBCov")) %>%
  # dropped neg IP & OP annual reimbursement amount
  subset(IPAnnualReimbursementAmt >= 0 & OPAnnualReimbursementAmt >= 0)

beneficiary_data$alive = factor(ifelse(is.na(beneficiary_data$DOD), 1, 0))
beneficiary_data$age = trunc((beneficiary_data$DOB  %--% Sys.Date()) / years(1))


inpatient_data <- read_csv('data/Train_Inpatientdata.csv',
                           # Updated col_types to keep the possible letter in codes
                           col_types = cols(.default = 'c',
                                            ClaimStartDt = 'D',
                                            ClaimEndDt = 'D',
                                            InscClaimAmtReimbursed = 'n',
                                            AdmissionDt = 'D',
                                            DeductibleAmtPaid = 'n',
                                            DischargeDt = 'D'
                           ))
inpatient_data <- inpatient_data %>%
  mutate_at(vars(matches("Code")), ~replace_na(., "Non-Exist"))  %>%
  mutate_at(vars(matches("Code")), as.factor) %>%
  mutate_at(vars(matches("Physician")), ~replace_na(., "Non-Exist")) %>%
  # Should we assume NA in deductibleAmtPaid to be 0 ???
  mutate_at("DeductibleAmtPaid", ~replace_na(., 0)) 

length(inpatient_data$ClaimStartDt == inpatient_data$AdmissionDt) == nrow(inpatient_data)
length(inpatient_data$ClaimEndDt == inpatient_data$DischargeDt) == nrow(inpatient_data)
# Both return TRUE, dropped AdmissionDt & DischargeDt

inpatient_data <- inpatient_data %>% 
  select(-c("AdmissionDt", "DischargeDt"))

outpatient_data <- read_csv('data/Train_Outpatientdata.csv',
                            # Updated col_types to keep the possible letter in codes
                            col_types = cols(.default = 'c',
                                             ClaimStartDt = 'D',
                                             ClaimEndDt = 'D',
                                             InscClaimAmtReimbursed = 'n',
                                             DeductibleAmtPaid = 'n'
                            ))
outpatient_data <- outpatient_data %>% 
  mutate_at(vars(matches("Code")), ~replace_na(., "Non-Exist"))  %>%
  mutate_at(vars(matches("Code")), as.factor) %>%
  mutate_at(vars(matches("Physician")), ~replace_na(., "Non-Exist"))

# Separate provider data into train validation and test using 60:20:20
set.seed(3451)
dt = sort(sample(nrow(provider_data), nrow(provider_data)*.6))
train<-provider_data[dt,]
test<-provider_data[-dt,]
dt = sort(sample(nrow(test), nrow(test)*.5))
valid<-test[dt,]
test<-test[-dt,]

# outer merge inpatient and outpatient data
AllPatientData <- merge(x = outpatient_data, y = inpatient_data, 
                        by = c('BeneID', 'ClaimID', 'ClaimStartDt', 'ClaimEndDt', 
                               'Provider', 'InscClaimAmtReimbursed', 'AttendingPhysician', 
                               'OperatingPhysician', 'OtherPhysician', 'ClmDiagnosisCode_1', 
                               'ClmDiagnosisCode_2', 'ClmDiagnosisCode_3', 'ClmDiagnosisCode_4', 
                               'ClmDiagnosisCode_5', 'ClmDiagnosisCode_6', 'ClmDiagnosisCode_7', 
                               'ClmDiagnosisCode_8', 'ClmDiagnosisCode_9', 'ClmDiagnosisCode_10', 
                               'ClmProcedureCode_1', 'ClmProcedureCode_2', 'ClmProcedureCode_3', 
                               'ClmProcedureCode_4', 'ClmProcedureCode_5', 'ClmProcedureCode_6', 
                               'DeductibleAmtPaid', 'ClmAdmitDiagnosisCode'), all = TRUE)
# left join patient data with beneficiary details
AllPatientData <- AllPatientData %>% left_join(beneficiary_data, by="BeneID")
# dropped 57 BeneIDs in patients data but not in beneficiary data
AllPatientData <- AllPatientData %>% filter(if_all(c("DOB", "Gender", "Race"), ~ !is.na(.x)))

AllPatientData$ClaimLength = as.numeric(difftime(AllPatientData$ClaimEndDt,
                                                 AllPatientData$ClaimStartDt, 
                                                 units = "days"))
AllPatientData$TotalAmt = AllPatientData$InscClaimAmtReimbursed + AllPatientData$DeductibleAmtPaid
AllPatientData$AvgPerDay = ifelse(AllPatientData$ClaimLength != 0, 
                                  round(AllPatientData$TotalAmt/AllPatientData$ClaimLength,2),
                                  AllPatientData$TotalAmt)

# merge with fraudulent provider details as separate train, test, and validation datasets
AllPatientDatatrain <- merge(AllPatientData, train, by='Provider')
AllPatientDatavalid <- merge(AllPatientData, valid, by='Provider')
AllPatientDatatest <- merge(AllPatientData, test, by='Provider')

# Use downsample to training data since it is hard to make up info like ProviderID, BeneID
APDtrain_downsample <- downSample(AllPatientDatatrain %>% select(-c(PotentialFraud)), AllPatientDatatrain$PotentialFraud, yname="PotentialFraud")

# InscClaimAmtReimbursed of inpatient & outpatient of one BeneID should equal to the total reimbursement amount (IPAnnualReimbursementAmt + OPAnnualReimbursementAmt) of the BeneID

AllPatientData$TotalReimbursementAmount = AllPatientData$IPAnnualReimbursementAmt + AllPatientData$OPAnnualReimbursementAmt 

InscClaimAmtReimbursed_ByBeneID <- setNames(aggregate(AllPatientData$InscClaimAmtReimbursed, by=list(BeneID=AllPatientData$BeneID), FUN=sum), c("BeneID", "InscClaimAmtReimbursed"))
TotalAnnualReimbursementAmount_ByBeneID <- distinct(AllPatientData, BeneID, .keep_all= TRUE) %>% select(BeneID, TotalReimbursementAmount)

all(InscClaimAmtReimbursed_ByBeneID$InscClaimAmtReimbursed == TotalAnnualReimbursementAmount_ByBeneID$TotalReimbursementAmount) # not all are equals 

length(which(InscClaimAmtReimbursed_ByBeneID$InscClaimAmtReimbursed == TotalAnnualReimbursementAmount_ByBeneID$TotalReimbursementAmount))
length(which(InscClaimAmtReimbursed_ByBeneID$InscClaimAmtReimbursed != TotalAnnualReimbursementAmount_ByBeneID$TotalReimbursementAmount))
# 77033 BeneID have equal amount, 61496 are not equal


# Correlation between amount and no. of days admitted

summary(AllPatientData$AvgPerDay)
ggplot(AllPatientData, aes(x=AvgPerDay)) + geom_histogram() + labs(title="Distribution of Average Amount Per Day", x="Amount")
ggplot(AllPatientData, aes(x=AvgPerDay)) + geom_histogram() + xlim(0, 1000) + labs(title="Distribution of Average Amount Per Day Under 1,000", x="Amount") 

ggplot(aes(x=TotalAmt, y=ClaimLength), data=AllPatientData) + geom_count() + labs(title="Total Amount VS No. of Days Admitted", x="Total Amount", y = "No. of Days Admitted")


# InscClaimAmtReimbursed involving OperatingPhysician and not involving OperatingPhysician

AllPatientData_ByClaimID <- distinct(AllPatientData, ClaimID, .keep_all= TRUE) %>% select(ClaimID, InscClaimAmtReimbursed, OperatingPhysician)

AllPatientData_ByClaimID$Operation <- NA
AllPatientData_ByClaimID$Operation[AllPatientData_ByClaimID$OperatingPhysician=="Non-Exist"] <- "No"
AllPatientData_ByClaimID$Operation[AllPatientData_ByClaimID$OperatingPhysician!="Non-Exist"] <- "Yes"

summary(subset(AllPatientData_ByClaimID, AllPatientData_ByClaimID$Operation=="No")$InscClaimAmtReimbursed)
summary(subset(AllPatientData_ByClaimID, AllPatientData_ByClaimID$Operation=="Yes")$InscClaimAmtReimbursed)

ggplot(AllPatientData_ByClaimID, aes(x=InscClaimAmtReimbursed, fill=Operation)) + geom_histogram() + ylim(0, 150000) + xlim(0, 10000)+ labs(title="Distribution of Amount Involving OperatingPhysician and Not Involving OperatingPhysician Between 0 and 5,000", x="Amount")
ggplot(AllPatientData_ByClaimID, aes(x=InscClaimAmtReimbursed, fill=Operation)) + geom_histogram() + xlim(10000, 125000)+ labs(title="Distribution of Amount Involving OperatingPhysician and Not Involving OperatingPhysician Between 5,000 and 125,000", x="Amount")


p_load('cowplot')

plot_all_columns <- function(data_frame) 
{
  plot_list <- lapply(names(data_frame), function(var_x){
    p <- 
      ggplot(data_frame) +
      aes_string(var_x)
    
    if(is.numeric(data_frame[[var_x]])) {
      p <- p + geom_density()
      
    } else if(is.factor(data_frame[[var_x]])) {
      p <- p + geom_bar()
    } 
    
  })
  plot_grid(plotlist = plot_list)
}

plot_all_columns(provider_data)
plot_all_columns(beneficiary_data)

# Grouping BeneID by State, County, Race, Gender

AllPatientData_ByBeneID <- distinct(AllPatientData, BeneID, .keep_all= TRUE) %>% select(BeneID, State, County, Race, Gender)
plot_all_columns(AllPatientData_ByBeneID)


#START ----- distribution of diagnosisCode -----
diagnosis_codes <- read_csv('data/CMS32_DESC_LONG_SHORT_DX.csv')
inpatient_pivot_long <- inpatient_data %>%
  select(contains('DiagnosisCode')) %>%
  pivot_longer(cols = contains("DiagnosisCode"), 
               names_to = "DiagnosisType", 
               values_to = "DiagnosisCode") %>%
  mutate(DiagnosisGroupCode = ifelse(startsWith(DiagnosisCode, 'E'), substr(DiagnosisCode,0,4), substr(DiagnosisCode,0,3)) )

inpatient_pivot_long_with_group1 <-  left_join(inpatient_pivot_long, diagnosis_codes, by = c('DiagnosisGroupCode' = 'GROUP_L3_CODE')) %>%
  mutate(GROUP_L1_CODE = as.factor(as.integer(as.factor(GROUP_L1_NAME))))

inpatient_pivot_long_with_group1 %>%
  ggplot() + 
  geom_bar(aes(x = fct_infreq(GROUP_L1_NAME))) +
  theme(axis.text.x = element_text(angle = 90))
#END ----- distribution of diagnosisCode -----


#START ----- network analysis ----- 
#network among beneficiary, physician and provider

inpatient_pivot_physician <- inpatient_data %>%
  pivot_longer(cols = contains("Physician"), 
               names_to = "PhysicianType", 
               values_to = "PhysicianID") %>%
  select(c(BeneID, Provider, PhysicianType, PhysicianID))

bene_to_provider <- inpatient_data %>%
  select(c(BeneID, Provider))

physician_to_provider <- inpatient_pivot_physician %>%
  select(c(PhysicianID, Provider)) %>%
  drop_na(PhysicianID) %>%
  distinct()

#histogram of physicians per provider
number_of_physician_per_provider <- physician_to_provider %>% 
  group_by(Provider) %>%
  summarise(n = n())

number_of_physician_per_provider %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)

#draw graph
inpatient_data <- inpatient_data %>% sample_n(1000)

bene <- inpatient_data %>% 
  distinct(BeneID) %>% 
  rename(label = BeneID)
provider <- inpatient_data %>% 
  distinct(Provider) %>% 
  rename(label = Provider)

nodes <- full_join(bene, provider, by = "label")
nodes <- rowid_to_column(nodes, "id")

per_route <- inpatient_data %>%  
  group_by(BeneID, Provider) %>%
  summarise(weight = n()) %>% 
  ungroup()

edges <- per_route %>% 
  left_join(nodes, by = c("BeneID" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Provider" = "label")) %>% 
  rename(to = id)
edges <- select(edges, from, to, weight)

network <- network(edges,
                          vertex.attr = nodes,
                          matrix.type = "edgelist",
                          ignore.eval = FALSE)


p_load(visNetwork)
visNetwork(nodes, edges)


#inpatient_data - beneid_freq
beneid_freq <- inpatient_data %>%
  group_by(BeneID) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

beneid_freq %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)

#inpatient_data - provider_freq, provider on the long tail is suspicious?
provider_freq <- inpatient_data %>%
  group_by(Provider) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

provider_freq %>%
  filter(n > 20) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)

#inpatient_data - physician_freq, physician on the long tail is suspicious?
physician_freq <- inpatient_pivot_physician %>%
  drop_na(PhysicianID) %>%
  group_by(PhysicianID) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

physician_freq %>%
  filter(n > 10) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)

#END ----- network analysis ----- 


# Random forest

library('randomForest')
library('party')
library('precrec')
library('devtools')
library('reprtree')
library('ROCR')

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

if(!('reprtree' %in% installed.packages())){
  install_github('munoztd0/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

trial_data <- read_csv('data/in_out_patient_data_agg.csv')
trial_data$PotentialFraud <- as.factor(trial_data$PotentialFraud)
trial_data[is.na(trial_data)] <- 0

set.seed(3451)
dt = sort(sample(nrow(trial_data), nrow(trial_data)*.6))
trial_data_train<-trial_data[dt,]
trial_data_test<-trial_data[-dt,]
dt = sort(sample(nrow(trial_data_test), nrow(trial_data_test)*.5))
trial_data_valid<-trial_data_test[dt,]
trial_data_test<-trial_data_test[-dt,]

trial_data_train <- trial_data_train %>% select(!c(Provider)) %>% sample_n(2000)
trial_data_valid <- trial_data_valid %>% select(!c(Provider))

# train_data <- APDtrain_downsample  %>%
#   select(!contains("County")
#          & !contains("ID")
#          & !contains("Code")
#          & !c(Provider, DOB, DOD, ClaimStartDt, ClaimEndDt)
#          & !contains("Physician")) %>%
#   sample_n(10000)

best_mtry <- tuneRF(trial_data_train,trial_data_train$PotentialFraud,stepFactor = 1.2, improve = 0.001, ntree=300, trace=T, plot=T) 

set.seed(100)
rf <- randomForest(PotentialFraud ~ ., data=trial_data_train, importance=TRUE, proximity=TRUE,
                   mtry = 33, ntree = 300, maxnodes = 20) 
print(rf)

reprtree:::plot.getTree(rf)

rf_pred_train <- predict(rf, trial_data_train)
confusionMatrix(rf_pred_train, trial_data_train$PotentialFraud)

# creating prediction against validation data
rf_pred <- predict(rf, newdata = trial_data_valid)
confusionMatrix(data=rf_pred, reference=trial_data_valid$PotentialFraud)

# finding ROC and PRC values
rf_pred <- predict(rf, newdata = trial_data_valid, type = 'prob')
rf_prc <- evalmod(scores = rf_pred[, 2], labels = trial_data_valid$PotentialFraud, mode="rocprc")
rf_prc

# Plotting model
plot(rf)

# Importance plot
importance(rf)

# Variable importance plot
varImpPlot(rf,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")

# ROC curve for random forest
prediction_for_roc_curve <- predict(rf,trial_data_valid,type="prob")
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes <- levels(trial_data_valid$PotentialFraud)

for (i in 1:2)
{
  true_values <- ifelse(trial_data_valid[, 404]==classes[i],1,0)
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  auc <- performance(pred, measure = "auc")
  print(auc@y.values)
}
