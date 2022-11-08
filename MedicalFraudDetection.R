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
  mutate_at(vars(matches("Physician")), ~replace_na(., "Non-Exist"))

inpatient_data$DaysAdmittedInHospital = as.numeric(difftime(inpatient_data$DischargeDt,
                                                 inpatient_data$AdmissionDt, 
                                                 units = "days"))

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


