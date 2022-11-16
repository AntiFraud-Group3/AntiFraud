library(pacman)
p_load('tidyverse')
p_load('lubridate')

setwd('C:/work/AntiFraud')
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


#-----------------------------------------------
BendID_Nodes <- beneficiary_data %>%
  distinct(BeneID) %>%
  drop_na() %>%
  rename(label = BeneID) %>%
  mutate(group = 'Beneficiary') %>%
  mutate(shape = "dot") %>%
  #mutate(color = 8) %>%
  mutate(title = paste0("<p><b>", label, "</b>", "</p>"))%>%
  distinct()

Provider_Nodes <- provider_data %>%
  drop_na() %>%
  rename(label = Provider) %>%
  mutate(group = ifelse(PotentialFraud == "Yes", "Provider_Fraud", "Provider_NonFraud")) %>%
  mutate(shape = "star") %>%
  #mutate(color = ifelse(PotentialFraud == "Yes", 2, 3)) %>%
  mutate(title = paste0("<p><b>", label, "</b>", PotentialFraud,  "</p>"))%>%
  distinct()

Physician_Nodes <- inpatient_data %>%
  select(contains("Physician")) %>%
  drop_na() %>%
  pivot_longer(cols = contains("Physician"),
               names_to = "PhysicianType",
               values_to = "Physician") %>%
  distinct(Physician) %>%
  rename(label = Physician) %>%
  mutate(group = "Physician") %>%
  mutate(shape = "square") %>%
  #mutate(color = 4) %>%
  mutate(title = paste0("<p><b>", label, "</b>", "</p>")) %>%
  distinct()

nodes <- bind_rows(BendID_Nodes, Provider_Nodes, Physician_Nodes) %>% mutate(id = row_number())

bene_provider_from <- inpatient_data %>%
  select(BeneID, Provider) %>%
  left_join(nodes, by = c("BeneID" = "label")) %>%
  select(from = id)
bene_provider_to <- inpatient_data %>%
  select(BeneID, Provider) %>%
  left_join(nodes, by = c("Provider" = "label")) %>%
  select(to = id)
bene_provider = data.frame(from = bene_provider_from, to = bene_provider_to)

bene_provider <- bene_provider %>% distinct()

provider_physician_from <- inpatient_data %>%
  pivot_longer(cols = contains("Physician"),
               names_to = "Physician_Type",
               values_to = "Physician") %>%
  select(Provider, Physician) %>%
  left_join(nodes, by = c("Provider" = "label")) %>%
  select(from = id)
provider_physician_to <- inpatient_data %>%
  pivot_longer(cols = contains("Physician"),
               names_to = "Physician_Type",
               values_to = "Physician") %>%
  select(Provider, Physician) %>%
  left_join(nodes, by = c("Physician" = "label")) %>%
  select(to = id)
provider_physician = data.frame(from = provider_physician_from, to = provider_physician_to) %>% drop_na()
provider_physician <- provider_physician %>% distinct()

edges <- bind_rows(bene_provider, provider_physician) %>% drop_na()


nodes_special <- nodes %>%
  filter(label %in% c("PRV52065", "PRV52019")) %>%
  #filter(label %in% c("PRV53295", "PRV53242")) %>%
  select(id)

#provider_physician %>%
#  filter(from %in% unlist(nodes_special)
#         | to %in% unlist(nodes_special))

edges_special <- edges %>%
  filter(from %in% unlist(nodes_special)
         | to %in% unlist(nodes_special))

edges_sampled<-edges_special

nodes_id_sampled <- edges_sampled %>%
  pivot_longer(cols = c("from", "to"),
               names_to = "type",
               values_to = "id") %>%
  distinct(id)
nodes_sampled <- nodes_id_sampled %>% left_join(nodes, by = "id")


library(visNetwork)
visNetwork(nodes_sampled, edges_sampled) %>%
  #visIgraphLayout() %>%
  #visPhysics(stabilization  = FALSE) %>%
  visEdges(smooth = FALSE) %>%
  visGroups(groupname = "Provider_Fraud", color = "red") %>%  
  visGroups(groupname = "Provider_NonFraud", color = "green") %>%
  visGroups(groupname = "Physician", color = "yellow") %>%
  visLegend()


nodes_sampled_label <- nodes_sampled %>% select(label)
nodes_special <- nodes %>%
  filter(label %in% unlist(nodes_sampled_label)) %>%
  select(id)

rm(list = ls())
