setwd("C:/Users/shour/Desktop/project/AntiFraud")
getwd()

library('pacman')
p_load('tidyverse')
require('lubridate')

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
  mutate_at(vars(matches("Code")), ~replace_na(., -1))  %>%
  mutate_at(vars(matches("Code")), as.factor) %>%
  mutate_at(vars(matches("Physician")), ~replace_na(., "Non-Exist")) %>%
  # Should we assume NA in deductibleAmtPaid to be 0 ???
  mutate_at("DeductibleAmtPaid", ~replace_na(., 0))

inpatient_data$DaysAdmittedInHospital = as.numeric(difftime(inpatient_data$DischargeDt,
                                                 inpatient_data$AdmissionDt, 
                                                 units = "days"))
inpatient_data$TotalAmt = inpatient_data$InscClaimAmtReimbursed + inpatient_data$DeductibleAmtPaid
inpatient_data$AvgPerDay = round(inpatient_data$TotalAmt/inpatient_data$DaysAdmittedInHospital,2)

identical(inpatient_data[['ClaimStartDt']], inpatient_data[['AdmissionDt']])
identical(inpatient_data$ClaimEndDt, inpatient_data$DischargeDt)
# Both return false, I believe those columns are not identical

outpatient_data <- read_csv('data/Train_Outpatientdata.csv',
                            # Updated col_types to keep the possible letter in codes
                            col_types = cols(.default = 'c',
                                             ClaimStartDt = 'D',
                                             ClaimEndDt = 'D',
                                             InscClaimAmtReimbursed = 'n',
                                             DeductibleAmtPaid = 'n'
                            ))
outpatient_data <- outpatient_data %>%
  mutate_at(vars(matches("Code")), ~replace_na(., -1))  %>%
  mutate_at(vars(matches("Code")), as.factor) %>%
  mutate_at(vars(matches("Physician")), ~replace_na(., "Non-Exist"))

outpatient_data$TotalAmt = outpatient_data$InscClaimAmtReimbursed + outpatient_data$DeductibleAmtPaid

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

#draw graph - TBD

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
