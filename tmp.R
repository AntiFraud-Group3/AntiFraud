
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
  rename(label = BeneID) %>%
  mutate(group = 0)
provider <- inpatient_data %>% 
  distinct(Provider) %>% 
  rename(label = Provider)%>%
  mutate(group = 1)

nodes <- full_join(bene, provider, by = "label")
nodes <- bind_rows(bene, provider)
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