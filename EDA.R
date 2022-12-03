#EDA
library(pacman)
p_load(DataExplorer)
p_load(tidyverse)

setwd('C:/work/AntiFraud')
trial_data <- read_csv('data/in_out_patient_data_agg.csv')

trial_data %>%
  create_report(
    output_file = "in_out_patient_data_agg.csv",
    report_title = "EDA Report - Cardiovascular Disease Dataset",
    y = "PotentialFraud"
  )



#install.packages("GGally")
p_load(GGally)

# change plot size (optional)
options(repr.plot.width = 20, repr.plot.height = 10)

trial_data %>% 
  #select("age", "cholesterol", "height", "weight") %>%
  ggpairs(mapping = aes(color = trial_data$PotentialFraud, alpha = 0.5))



in_out_patient_data %>%
  create_report(
    output_file = "in_out_patient_data.html",
    report_title = "EDA Report - Inpatient/Outpatient Data",
    y = "PotentialFraud"
  )



beneficiary_data %>%
  create_report(
    output_file = "beneficiary_data.html",
    report_title = "EDA Report - Inpatient/Outpatient Data",
    y = NULL
  )
