getwd()

library('pacman')
p_load('tidyverse')

provider_data <- read_csv('data/Train.csv')
provider_data <- provider_data %>%
  mutate(PotentialFraud = factor(ifelse(PotentialFraud == "Yes", 1, 0)))
table(provider_data$PotentialFraud)


beneficiary_data <- read_csv('data/Train_Beneficiarydata.csv',
                             col_types = cols(.default = 'n',
                                              BeneID = 'c',
                                              DOB = 'D',
                                              DOD = 'D',
                                              Race = 'f',
                                              RenalDiseaseIndicator = 'I',
                                              State = 'f',
                                              County = 'f'))
beneficiary_data <- beneficiary_data %>%
  mutate_at(vars(matches("ChronicCond")), ~ifelse(. == "1", 0, 1)) %>%
  mutate_at(vars(matches("ChronicCond")), as.factor)


inpatient_data <- read_csv('data/Train_Inpatientdata.csv',
                           col_types = cols(.default = 'n',
                                            BeneID = 'c',
                                            ClaimID = 'c',
                                            ClaimStartDt = 'D',
                                            ClaimsEndDt = 'D',
                                            Provider = 'c',
                                            AttendingPhysician = 'c',
                                            OperatingPhysician = 'c',
                                            OtherPhysician = 'c',
                                            AdmissionDt = 'D',
                                            DischargeDt = 'D'
                                            ))
inpatient_data <- inpatient_data %>%
  mutate_at(vars(matches("Code")), ~replace_na(., -1))  %>%
  mutate_at(vars(matches("Code")), as.factor) %>%
  mutate_at(vars(matches("Physician")), ~replace_na(., "Non-Exist"))

outpatient_data <- read_csv('data/Train_Outpatientdata.csv',
                           col_types = cols(.default = 'n',
                                            BeneID = 'c',
                                            ClaimID = 'c',
                                            ClaimStartDt = 'D',
                                            ClaimsEndDt = 'D',
                                            Provider = 'c',
                                            AttendingPhysician = 'c',
                                            OperatingPhysician = 'c',
                                            OtherPhysician = 'c',
                                            AdmissionDt = 'D',
                                            DischargeDt = 'D'
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


