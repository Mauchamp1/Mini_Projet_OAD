library (dplyr)
library (terra)
library(ggplot2)
install.packages("readr")
library(readr)
install.packages("tidyverse")
library(tidyverse)


data_matreex_ini = readRDS("BA_sim.Rds")


summary(data_matreex_ini)
head(data_matreex_ini)

#Garde une seule simu
simuY <- data_matreex_ini %>%
  filter(ID.simulation == 30000)

ggplot(data = data_matreex_ini)+
  aes(ID.simulation)+
  geom_bar()




data_phoreau_ini <- read_tsv("Data Phoreau/Site_Species_BA.tsv")

data_phoreau_ini$speciesName <- as.factor(data_phoreau_ini$speciesName)
data_phoreau_ini$Scenario <- as.factor(data_phoreau_ini$Scenario)

summary(data_phoreau_ini$IDP)



# Supprimer de Matreex les SSP 126
data_matree_ssp585 <- data_matreex_ini %>%
  filter(ssp == "ssp585")

#On passe les espèce en format long
data_mat_sp_wide <- data_matree_ssp585 %>%
  pivot_wider(
    names_from = species,
    values_from = BA)
  
ggplot(data = data_mat_sp_wide)+
  aes(ID.simulation)+
  geom_bar()

test =data_mat_sp_wide %>% 
  group_by(time, ID.simulation)
  
  

  










