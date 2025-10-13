##Evolution de G

library(dplyr)
library (terra)
library(ggplot2)
library(readr)
library(tidyverse)

##Import data Matreex et tri
data_matreex_ini = readRDS("C:/Users/steph/Downloads/BA_sim.Rds")
data_matree_ssp585 <- data_matreex_ini %>%
  filter(ssp == "ssp585")

##Import data PhorEau et tri 
data_phoreau_ini <- read_tsv("C:/Users/steph/Downloads/Site_Species_BA.tsv")
data_phoreau_ini$speciesName <- as.factor(data_phoreau_ini$speciesName)
data_phoreau_ini$Scenario <- as.factor(data_phoreau_ini$Scenario)
phoreau_selec = data_phoreau_ini %>% 
  filter(Scenario == "BusinessAsUsual_ssp585")

##tri des données à l'année t1(1 pour Matreex et 2001 pour PhorEau) et Tfin(100 pour Matreex et 2100 pour Phoreau) 
df_filteredMatreex <- data_matree_ssp585 %>%
  filter(time %in% c(1, 100)) %>%
  arrange(plotcode, time)

df_filteredPhorEau <- phoreau_selec %>%
  filter(Year %in% c(2001, 2100)) %>%
  arrange(IDP, Year)

##Création de deux catégories de placettes (à monoessence sup à 90% et plurispé)
#Calcul du BA total et tri par ID placette
dominance_info_matreex <- df_filteredMatreex %>%
  group_by(plotcode) %>%
  mutate(BA_total = sum(BA, na.rm = TRUE)) %>%
  mutate(prop_BA = BA / BA_total) %>%
  summarise(max_prop = max(prop_BA, na.rm = TRUE)) %>%
  ungroup()

dominance_info_phoreau <- df_filteredPhorEau %>%
  group_by(IDP) %>%
  mutate(BA_total = sum(basalAreaSum, na.rm = TRUE)) %>%
  mutate(prop_BA = basalAreaSum / BA_total) %>%
  summarise(max_prop = max(prop_BA, na.rm = TRUE)) %>%
  ungroup()

#Créer deux vecteurs de plotcodes pour chaque modèle
plots_dominants_matreex <- dominance_info_matreex %>%
  filter(max_prop >= 0.9) %>%
  pull(plotcode)

plots_non_dominants_matreex <- dominance_info_matreex %>%
  filter(max_prop < 0.9) %>%
  pull(plotcode)

plots_dominants_phoreau <- dominance_info_phoreau %>%
  filter(max_prop >= 0.9) %>%
  pull(IDP)

plots_non_dominants_phoreau <- dominance_info_phoreau %>%
  filter(max_prop < 0.9) %>%
  pull(IDP)

#Créer deux bases de données contenant les deux types de peuplement pour chacun des modèles
Matreex_pplmt_monospé <- df_filteredMatreex %>%
  filter(plotcode %in% plots_dominants_matreex)
Matreex_pplmt_plurispé <- df_filteredMatreex %>%
  filter(plotcode %in% plots_non_dominants_matreex)

PhorEau_pplmt_monospé <- df_filteredPhorEau %>%
  filter(IDP %in% plots_dominants_phoreau)
PhorEau_pplmt_plurispé <- df_filteredPhorEau %>%
  filter(IDP %in% plots_non_dominants_phoreau)




