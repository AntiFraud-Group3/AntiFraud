g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g + geom_bar()

table(mpg$class)

setwd('C:/work/AntiFraud')

#age, alive distribution
table(beneficiary_data$alive) #1421 dead, 137108 alive
#many ppl with age > 100
beneficiary_data %>%
  ggplot(aes(x = age)) +
  geom_histogram()


inpatient_data <- read_csv('data/Train_Inpatientdata.csv')


#distribution of diagnosisCode
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


table(inpatient_data_2_with_group1$GROUP_L1_NAME)
table(inpatient_data_2_with_group1_code$GROUP_L1_CODE)
unique(inpatient_data_2_with_group1$GROUP_L1_NAME)
unique(inpatient_data_2_with_group1_code$GROUP_L1_CODE)

inpatient_data_2_with_group1_code <- inpatient_data_2_with_group1 %>%
  mutate(GROUP_L1_CODE = as.factor(as.integer(as.factor(GROUP_L1_NAME))))


#mutate(DiagnosisGroupCode = substr(DiagnosisCode,0,3) )

inpatient_code <- inpatient_data %>%
  #select(contains('DiagnosisCode')) %>%
  pivot_longer(cols = contains("DiagnosisCode"), 
               names_to = "DiagnosisType", 
               values_to = "DiagnosisCode")

inpatient_code <- left_join(inpatient_code, diagnosis_codes, by = c('DiagnosisCode' = 'DIAGNOSIS CODE'))


inpatient_code %>% 
  select(DiagnosisCode) %>%
  sample_n(10)

%>%
  ggplot(aes(x = DiagnosisCode))
+ geom_bar()



ggplot(data=tips, aes(x=day)) + geom_bar(stat="bin")

p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity")



rm(list = ls())






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

#inpatient_data - beneid_freq
beneid_freq <- inpatient_data %>%
  group_by(BeneID) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

beneid_freq %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)

#inpatient_data - provider_freq
provider_freq <- inpatient_data %>%
  group_by(Provider) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

provider_freq %>%
  filter(n > 20) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)

#inpatient_data - physician_freq
physician_freq <- inpatient_pivot_physician %>%
  drop_na(PhysicianID) %>%
  group_by(PhysicianID) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

physician_freq %>%
  filter(n > 10) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1)



##
bene_to_provider <- bene_to_provider %>% distinct()
net = network(bene_to_provider, directed = FALSE)


edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)

bene_to_provider <- inpatient_data %>%
  group_by(BeneID, Provider) %>%
  summarise(weight = n()) %>%
  ungroup()

bene_nodes <- beneficiary_data %>%
  select(BeneID) %>%
  distinct()

provider_nodes <- provider_data %>%
  select(Provider) %>%
  distinct()

nodes <- bind_rows(bene_nodes, provider_nodes)
edges <- bene_to_provider
routes_network <- network(edges,
                          vertex.attr = nodes,
                          matrix.type = "edgelist",
                          ignore.eval = FALSE)
summary(routes_network)
plot(routes_network, vertex.cex = 3)



########
inpatient_data_sample <- inpatient_data %>% sample_n(100)

bene_sample <- inpatient_data_sample %>% 
  distinct(BeneID) %>% 
  rename(label = BeneID)
provider_sample <- inpatient_data_sample %>% 
  distinct(Provider) %>% 
  rename(label = Provider)

#nodes_sample <- bind_rows(bene_sample, provider_sample)
nodes_sample <- full_join(bene_sample, provider_sample, by = "label")
nodes_sample <- rowid_to_column(nodes_sample, "id")

per_route <- inpatient_data_sample %>%  
  group_by(BeneID, Provider) %>%
  summarise(weight = n()) %>% 
  ungroup()

edges_sample <- per_route %>% 
  left_join(nodes_sample, by = c("BeneID" = "label")) %>% 
  rename(from = id)

edges_sample <- edges_sample %>% 
  left_join(nodes_sample, by = c("Provider" = "label")) %>% 
  rename(to = id)
edges_sample <- select(edges_sample, from, to, weight)

network_sample <- network(edges_sample,
                          vertex.attr = nodes_sample,
                          matrix.type = "edgelist",
                          ignore.eval = FALSE)

plot(network_sample, vertex.cex = 3)
plot(network_sample, vertex.cex = 3, mode = "circle")
summary(network_sample)

p_load(visNetwork)
visNetwork(nodes_sample, edges_sample)
##


p_load('network')
p_load('sna')
p_load('GGally')

net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]
ggnet2(net)

# root URL
r = "https://raw.githubusercontent.com/briatte/ggnet/master/"

# read nodes
v = read.csv(paste0(r, "inst/extdata/nodes.tsv"), sep = "\t")
names(v)

# read edges
e = read.csv(paste0(r, "inst/extdata/network.tsv"), sep = "\t")
names(e)


# network object
net = network(e, directed = TRUE)

# party affiliation
x = data.frame(Twitter = network.vertex.names(net))
x = merge(x, v, by = "Twitter", sort = FALSE)$Groupe
net %v% "party" = as.character(x)

# color palette
y = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]
names(y) = levels(x)

# network plot
ggnet2(net, color = "party", palette = y, alpha = 0.75, size = 4, edge.alpha = 0.5)



AllPatientData %>%
  left_join(provider_data, by=c("Provider")) %>%
  ggplot(aes(x=TotalAmt, y=ClaimLength, color = PotentialFraud)) + 
  geom_count() + 
  labs(title="Total Amount VS No. of Days Admitted", 
       x="Total Amount", 
       y = "No. of Days Admitted")



AllPatientData$ClaimLength



APDtrain <- APDtrain_downsample  %>%
  select(!contains("ID") 
         & !contains("Code") 
         & !c(Provider, DOB, DOD, ClaimStartDt, ClaimEndDt)
         & !contains("Physician")) %>%
  sample_n(1000)


APDtrain <- APDtrain %>% mutate_if(is.numeric, scale)

p_load(caret) # for models

p_load(ROSE) #for over-/under-sampling function
p_load(precrec) #for Precision-Recall function

p_load(randomForest)
p_load(NeuralNetTools)
p_load(neuralnet)

APDtrain %>% 
  sapply(levels)

APDtrain %>%
  select(is.factor) %>%
  colnames %>%
  paste0(collapse=" + ")



model_matrix <- model.matrix( 
  ~ PotentialFraud + Gender + Race + RenalDiseaseIndicator + State + County + ChronicCond_Alzheimer + ChronicCond_Heartfailure + ChronicCond_Cancer + ChronicCond_ObstrPulmonary + ChronicCond_Depression + ChronicCond_Diabetes + ChronicCond_IschemicHeart + ChronicCond_Osteoporasis + ChronicCond_stroke + alive + PotentialFraud, 
  data = APDtrain
)
head(model_matrix)
set.seed(123)
model_nn <- 
  neuralnet(PotentialFraud ~ . , 
            data = model_matrix, 
            hidden = 5, 
            linear.output = FALSE)


str(model_matrix)
