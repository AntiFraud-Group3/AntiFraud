p_load('tidyverse')
p_load('cowplot')
p_load('ggthemes')
p_load(eeptools)
#load provider
provider_data <- read_csv('data/Train.csv',
                          col_types = cols( Provider = 'c',
                          PotentialFraud = 'f'))

#load beneficiary
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
  subset(IPAnnualReimbursementAmt >= 0 & OPAnnualReimbursementAmt >= 0) %>%
  mutate(Alive = factor(ifelse(is.na(DOD), 1, 0))) %>%
  mutate(YearOfDeath_Or_Now = ifelse(is.na(DOD), 2009, year(DOD))) %>%
  mutate(Age = YearOfDeath_Or_Now - year(DOB))


#plog_distribution <- function(data_frame, col_names)
#{
#  if(is.numeric(data) {
#    p <- p + geom_density() 
#    
#  } else if(is.factor(data)) {
#    p <- p + geom_bar()
#  } 
#}
table(beneficiary_data$Age)
table(beneficiary_data$DOB)
table(beneficiary_data$DOD)
table(is.na(beneficiary_data$DOD))
table(is.na(beneficiary_data$DOB))

tmp <- as.Date(ifelse(is.na(beneficiary_data$DOD), as.Date('2009-12-31'), beneficiary_data$DOD), origin = "1970-01-01")

table(ifelse(is.na(beneficiary_data$DOD), as.Date('2009-12-31'), beneficiary_data$DOD))
table(beneficiary_data$Age)

#plot provider
provider_data %>%
  ggplot(aes(x = PotentialFraud, fill = PotentialFraud)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#plot beneficiary
colnames(beneficiary_data)
plot_columns(beneficiary_data, c('Gender', 'Alive', 'Race'))
plot_columns(beneficiary_data, 'State')
plot_columns(beneficiary_data, 'County')
plot_columns(beneficiary_data, 'RenalDiseaseIndicator')
plot_columns(beneficiary_data, c('IPAnnualReimbursementAmt', 'IPAnnualDeductibleAmt', 'OPAnnualReimbursementAmt', 'OPAnnualDeductibleAmt'))

beneficiary_data %>%
  pivot_longer(cols = contains("Amt"), 
               names_to = "AmtType", 
               values_to = "Amt") %>%
  ggplot(aes(x = Amt)) + 
  #geom_histogram(aes(y = ..density..)) + 
  geom_density(col = 3, fill = 4) + 
  facet_wrap(~ AmtType, scales = "free")

beneficiary_data %>%
  pivot_longer( cols = c(RenalDiseaseIndicator, contains("ChronicCond")),
               names_to = "ChronicCondType", 
               values_to = "ChronicCond") %>%
  ggplot(aes(x = ChronicCond, col = ChronicCond, fill = ChronicCond)) + 
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1, size=2) +
  facet_wrap(~ ChronicCondType, scales = "fixed")


plot_columns <- function(data_frame, col_names) 
{
  plot_list <- lapply(col_names, function(var_x){
    p <- ggplot(data_frame, aes(fill = .data[[var_x]])) + aes_string(var_x)
    
    if(is.numeric(data_frame[[var_x]])) {
      p <- p + 
        #geom_histogram(aes(y = ..density..), col = 3, fill = 3, bins=50) + 
        geom_density(col = 4, fill = 4) 
      
    } else if(is.factor(data_frame[[var_x]])) {
      p <- p + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)
    } 
    #p + theme_economist()
    
  })
  plot_grid(plotlist = plot_list)
}



#inpatient/outpatient data


inpatient_data <- read_csv('data/Train_Inpatientdata.csv',
                           col_types = cols(.default = 'c',
                                            ClaimStartDt = 'D',
                                            ClaimEndDt = 'D',
                                            InscClaimAmtReimbursed = 'n',
                                            AdmissionDt = 'D',
                                            DeductibleAmtPaid = 'n',
                                            DischargeDt = 'D'
                           )) %>%
  mutate_at(vars(matches("Code")), ~replace_na(., "Non-Exist"))  %>%
  mutate_at(vars(matches("Code")), as.factor) %>%
  mutate_at(vars(matches("Physician")), ~replace_na(., "Non-Exist")) %>%
  mutate_at("DeductibleAmtPaid", ~replace_na(., 0)) %>%
  mutate(ClaimDuration = as.numeric(difftime(ClaimEndDt, ClaimStartDt), units="days")) %>%
  mutate(InhospitalDuration =  as.numeric(difftime(DischargeDt, AdmissionDt), units="days")) %>%
  left_join(provider_data, by = "Provider")

colnames(inpatient_data)
plot_columns(inpatient_data, c('ClaimDuration', 'InhospitalDuration'))
inpatient_data %>%
  pivot_longer(cols = contains("Duration"), 
               names_to = "DurationType", 
               values_to = "Duration") %>%
  ggplot(aes(x = Duration, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..count..), binwidth = 1) + 
  #geom_density(col = 3, fill = 4) + 
  facet_wrap(~ DurationType, scales = "fixed")


inpatient_data %>%
  pivot_longer(cols = contains("Duration"), 
               names_to = "DurationType", 
               values_to = "Duration") %>%
  ggplot(aes(x = Duration, group = PotentialFraud, color = PotentialFraud)) + 
  geom_density() + 
  facet_wrap(~ DurationType, scales = "fixed")

inpatient_data %>%
  pivot_longer(cols = contains("Amt"), 
               names_to = "AmtType", 
               values_to = "Amt") %>%
  ggplot(aes(x = Amt, group = PotentialFraud, color = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..density..)) + 
  #geom_density(col = 3, fill = 4) + 
  #scale_x_log10() +
  facet_wrap(~ AmtType, scales = "free")

inpatient_data %>%
  select(InscClaimAmtReimbursed, PotentialFraud) %>%
  ggplot(aes(x = InscClaimAmtReimbursed, group = PotentialFraud, color = PotentialFraud)) + 
  #geom_histogram(aes(y = ..density..))
  geom_density()


inpatient_data %>%
  select(InscClaimAmtReimbursed, PotentialFraud) %>%
  ggplot(aes(x = InscClaimAmtReimbursed, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..count..), binwidth = 500) +
  scale_y_log10()
  #geom_density()
#--------------------------------------------------------------------------------
#only 2 values, 0 or 39575
#inpatient_data %>%
#  select(DeductibleAmtPaid, PotentialFraud) %>%
#  ggplot(aes(x = DeductibleAmtPaid, group = PotentialFraud, fill = PotentialFraud)) + 
#  geom_histogram(aes(y = ..density..), binwidth = 10)
#geom_density()

table(inpatient_data$DeductibleAmtPaid)

inpatient_data %>%
  select(InscClaimAmtReimbursed, PotentialFraud) %>%
  filter(InscClaimAmtReimbursed > 4500) %>%
  ggplot(aes(x = InscClaimAmtReimbursed, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1000)

inpatient_data %>%
  select(InscClaimAmtReimbursed, PotentialFraud) %>%
  filter(InscClaimAmtReimbursed > 55000) %>%
  ggplot(aes(x = InscClaimAmtReimbursed, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1000)



summary(inpatient_data$InscClaimAmtReimbursed)
inpatient_data$InscClaimAmtReimbursed

#---------------------------------------------


inpatient_data <- read_csv('data/Train_Inpatientdata.csv',
                           col_types = cols(.default = 'c',
                                            ClaimStartDt = 'D',
                                            ClaimEndDt = 'D',
                                            InscClaimAmtReimbursed = 'n',
                                            AdmissionDt = 'D',
                                            DeductibleAmtPaid = 'n',
                                            DischargeDt = 'D'
                           )) %>%
  mutate_at("DeductibleAmtPaid", ~replace_na(., 0)) %>%
  mutate(ClaimDuration = as.numeric(difftime(ClaimEndDt, ClaimStartDt), units="days")) %>%
  mutate(InhospitalDuration =  as.numeric(difftime(DischargeDt, AdmissionDt), units="days")) %>%
  mutate(ClaimType = factor("InPatient")) %>%
  left_join(provider_data, by = "Provider")



outpatient_data <- read_csv('data/Train_Outpatientdata.csv',
                           col_types = cols(.default = 'c',
                                            ClaimStartDt = 'D',
                                            ClaimEndDt = 'D',
                                            InscClaimAmtReimbursed = 'n',
                                            DeductibleAmtPaid = 'n',
                           )) %>%
  mutate_at("DeductibleAmtPaid", ~replace_na(., 0)) %>%
  mutate(ClaimDuration = as.numeric(difftime(ClaimEndDt, ClaimStartDt), units="days")) %>%
  mutate(InhospitalDuration =  0) %>%
  mutate(ClaimType = factor("OutPatient")) %>%
  left_join(provider_data, by = "Provider")

in_out_patient_data <- bind_rows(inpatient_data, outpatient_data)


colnames(in_out_patient_data)


in_out_patient_data %>%
  select(InscClaimAmtReimbursed, PotentialFraud, ClaimType) %>%
  ggplot(aes(x = InscClaimAmtReimbursed, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..count..), binwidth = 500) +
  scale_y_log10() +
  facet_wrap(~ ClaimType, scales = "free")


in_out_patient_data %>%
  select(InscClaimAmtReimbursed, PotentialFraud, ClaimType)

## NEW FEATURES ####
in_out_patient_data %>%
  group_by(Provider, PotentialFraud) %>%
  summarise(Claim_Count = n()) %>%
  ggplot(aes(x = Claim_Count, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..count..), binwidth = 10)  +
  scale_y_log10()

in_out_patient_data %>%
  group_by(Provider, PotentialFraud) %>%
  summarise(Claim_Count = n()) %>%
  filter(Claim_Count > 500) %>%
  ggplot(aes(x = Claim_Count, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..count..), binwidth = 10)  #+
  #scale_y_log10()


in_out_patient_data %>%
  pivot_longer(cols = contains("Physician"), 
               names_to = "PhysicianType", 
               values_to = "Physician") %>%
  group_by(Provider, PotentialFraud, ClaimType) %>%
  summarise(Physician_Count = n_distinct(Physician)) %>%
  ggplot(aes(x = Physician_Count, group = PotentialFraud, fill = PotentialFraud)) + 
  geom_histogram(aes(y = ..count..), binwidth = 1) +
  scale_y_log10() +
  facet_wrap(~ ClaimType, scales = "free")


#
diagnosis_code_all <- in_out_patient_data %>%
  select(contains('DiagnosisCode'), PotentialFraud) %>%
  pivot_longer(cols = contains("DiagnosisCode"), 
               names_to = "DiagnosisType", 
               values_to = "DiagnosisCode") %>%
  mutate(across('DiagnosisCode', str_replace, 'V', '')) %>%
  mutate(across('DiagnosisCode', str_replace, 'E', '')) %>%
  mutate(DiagnosisCode = as.numeric(DiagnosisCode))

diagnosis_code_fraud <- diagnosis_code_all %>%
  filter(PotentialFraud == "Yes") 

diagnosis_code_non_fraud <- diagnosis_code_all %>%
  filter(PotentialFraud == "No") 
#both don't conform to Bondford law. but fraud has higher frequence on digit 4, 8, 9
benford_result_fraud <- benford(diagnosis_code_fraud$DiagnosisCode, number.of.digits = 1, discrete = T, sign = "positive") 
plot(benford_result_fraud)


benford_result_non_fraud <- benford(diagnosis_code_non_fraud$DiagnosisCode, number.of.digits = 1, discrete = T, sign = "positive") 
plot(benford_result_non_fraud)



table(diagnosis_code_all$DiagnosisCode)
table(tmp$Physician_Count)



tmp <- in_out_patient_data %>%
  pivot_longer(cols = contains("Physician"), 
               names_to = "PhysicianType", 
               values_to = "Physician")


provider_data_new <-provider_data

colnames(in_out_patient_data)

in_out_patient_data_agg <- in_out_patient_data %>%
  group_by(Provider) %>%
  summarise(InhospitalDuration_sum = sum(InhospitalDuration),
            ClaimDuration_sum = sum(ClaimDuration),
            ClaimID_count = n_distinct(ClaimID),
            BeneID_count = n_distinct(BeneID),
            InscClaimAmtReimbursed_sum = sum(InscClaimAmtReimbursed),
            AttendingPhysician_count = n_distinct(AttendingPhysician),
            OperatingPhysician_count = n_distinct(OperatingPhysician),
            OtherPhysician_count = n_distinct(OtherPhysician),
            ClmAdmitDiagnosisCode_count = n_distinct(ClmAdmitDiagnosisCode),
            DeductibleAmtPaid_sum = sum(DeductibleAmtPaid),
            DiagnosisGroupCode_count = n_distinct(DiagnosisGroupCode),
            )


in_out_patient_data_agg <- in_out_patient_data %>%
  select(contains('DiagnosisCode'),  Provider) %>%
  pivot_longer(cols = contains("DiagnosisCode"), 
               names_to = "DiagnosisType", 
               values_to = "DiagnosisCode") %>%
  group_by(Provider) %>%
  summarise(DiagnosisCode_couut = n_distinct(DiagnosisCode)) %>%
  left_join(in_out_patient_data_agg, by = "Provider")

in_out_patient_data_agg <- in_out_patient_data %>%
  select(contains('ProcedureCode'),  Provider) %>%
  pivot_longer(cols = contains("ProcedureCode"), 
               names_to = "ProcedureCodeType", 
               values_to = "ProcedureCode") %>%
  group_by(Provider) %>%
  summarise(ProcedureCode_couut = n_distinct(ProcedureCode)) %>%
  left_join(in_out_patient_data_agg, by = "Provider")

colnames(beneficiary_data)

beneficiary_data_factors <- beneficiary_data %>%
  select(BeneID, Gender, Race, State, County)
dmy <- dummyVars("BeneID ~ .", data = beneficiary_data_factors)
trsf <- data.frame(predict(dmy, newdata = beneficiary_data_factors))
beneficiary_data_one_hot <- bind_cols(beneficiary_data, trsf)

colnames(beneficiary_data_one_hot)
race_agg <- in_out_patient_data %>%
  left_join(beneficiary_data_one_hot, by = 'BeneID') %>%
  group_by(Provider) %>%
  summarize(across(contains("Race."), sum, na.rm=T))

gender_agg <- in_out_patient_data %>%
  left_join(beneficiary_data_one_hot, by = 'BeneID') %>%
  group_by(Provider) %>%
  summarize(across(contains("Gender."), sum, na.rm=T))

state_agg <- in_out_patient_data %>%
  left_join(beneficiary_data_one_hot, by = 'BeneID') %>%
  group_by(Provider) %>%
  summarize(across(contains("State."), sum, na.rm=T))

county_agg <- in_out_patient_data %>%
  left_join(beneficiary_data_one_hot, by = 'BeneID') %>%
  group_by(Provider) %>%
  summarize(across(contains("County."), sum, na.rm=T))

in_out_patient_data_agg <- list(race_agg, gender_agg, state_agg, county_agg) %>%
  reduce(full_join, by='Provider') %>%
  left_join(in_out_patient_data_agg, by = "Provider")

beneficiary_data_numberic <- beneficiary_data %>%
  mutate_at(vars(contains('ChronicCond_')), as.numeric) %>%
  mutate(Alive = as.numeric(Alive))

chronicCond_agg <- in_out_patient_data %>%
  left_join(beneficiary_data_numberic, by = 'BeneID') %>%
  group_by(Provider) %>%
  summarize(across(contains("ChronicCond_"), sum, na.rm=T))

in_out_patient_data_agg <- chronicCond_agg %>%
  left_join(in_out_patient_data_agg, by = "Provider")


in_out_patient_data_agg <- in_out_patient_data %>%
  left_join(beneficiary_data_numberic, by = 'BeneID') %>%
  group_by(Provider) %>%
  summarize(IPAnnualReimbursementAmt_sum = sum(IPAnnualReimbursementAmt),
            IPAnnualDeductibleAmt_sum = sum(IPAnnualDeductibleAmt),
            OPAnnualReimbursementAmt_sum = sum(OPAnnualReimbursementAmt),
            OPAnnualDeductibleAmt_sum = sum(OPAnnualDeductibleAmt),
            Age_sum = sum(Age),
            Alive_sum = sum(Alive),
            ) %>%
  left_join(in_out_patient_data_agg, by = "Provider")

write.csv(in_out_patient_data_agg, "in_out_patient_data_agg.csv")

