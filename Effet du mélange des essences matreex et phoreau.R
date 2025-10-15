library(dplyr)
library (terra)
library(ggplot2)
library(readr)
library(tidyverse)

##Import data Matreex et tri pour enlever les NA
data_matreex_ini = readRDS("C:/Users/steph/Downloads/BA_sim.Rds")
data_matreex_ssp585 <- data_matreex_ini %>%
  filter(ssp == "ssp585")

matreex_585_na <- data_matreex_ssp585[is.na(data_matreex_ssp585$BA),]

#On récupère les ID.simulation associés matreex_585_na
id_listmatreex <- unique(matreex_585_na$ID.simulation)

#On les enlève pour garder que les données BA présentes 
matreex_85_clean <-  data_matreex_ssp585 %>%
  filter(!ID.simulation %in% id_listmatreex)

##Import data PhorEau et tri 
data_phoreau_ini <- read_tsv("C:/Users/steph/Downloads/Site_Species_BA.tsv")
data_phoreau_ini$speciesName <- as.factor(data_phoreau_ini$speciesName)
data_phoreau_ini$Scenario <- as.factor(data_phoreau_ini$Scenario)
phoreau_selec = data_phoreau_ini %>% 
  filter(Scenario == "BusinessAsUsual_ssp585")

phoreau_selec_na <- phoreau_selec[is.na(phoreau_selec$basalAreaSum),]

#On récupère les ID.simulation associés phoreau_selec_na
id_listphoreau <- unique(phoreau_selec_na$IDP)

#On les enlève pour garder que les données BA présentes 
phoreau_selec_clean <-  phoreau_selec %>%
  filter(!IDP %in% id_listphoreau)

##tri des données à l'année t1(1 pour Matreex et 2001 pour PhorEau) et Tfin(100 pour Matreex et 2100 pour Phoreau) 
df_filteredMatreex <- data_matreex_ssp585 %>%
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
  filter(max_prop >= 0.8) %>%
  pull(plotcode)

plots_non_dominants_matreex <- dominance_info_matreex %>%
  filter(max_prop < 0.8) %>%
  pull(plotcode)

plots_dominants_phoreau <- dominance_info_phoreau %>%
  filter(max_prop >= 0.8) %>%
  pull(IDP)

plots_non_dominants_phoreau <- dominance_info_phoreau %>%
  filter(max_prop < 0.8) %>%
  pull(IDP)

#Créer deux bases de données contenant les deux types de peuplement pour chacun des modèles
Matreex_pplmt_monospé <- df_filteredMatreex %>%
  filter(plotcode %in% plots_dominants_matreex) %>%
  mutate(type_peuplement = "monospécifique")
Matreex_pplmt_plurispé <- df_filteredMatreex %>%
  filter(plotcode %in% plots_non_dominants_matreex) %>%
  mutate(type_peuplement = "plurispécifique")

Matreex_database<-bind_rows(Matreex_pplmt_monospé, Matreex_pplmt_plurispé)

PhorEau_pplmt_monospé <- df_filteredPhorEau %>%
  filter(IDP %in% plots_dominants_phoreau)%>%
  mutate(type_peuplement = "monospécifique")
PhorEau_pplmt_plurispé <- df_filteredPhorEau %>%
  filter(IDP %in% plots_non_dominants_phoreau)%>%
  mutate(type_peuplement = "plurispécifique")

PhorEau_database<-bind_rows(PhorEau_pplmt_monospé,PhorEau_pplmt_plurispé)

#récupération de l'essence principale dans le mélange pour chaque placette et pour tini tfin
PhorEau_monotri_essence <- PhorEau_pplmt_monospé %>% 
  group_by(IDP, Year) %>% 
  slice_max(basalAreaSum, n=1, )
  
PhorEau_pluritri_essence <- PhorEau_pplmt_plurispé %>% 
  group_by(IDP, Year) %>% 
  slice_max(basalAreaSum, n=1, ) 
  
Matreex_monotri_essence <- Matreex_pplmt_monospé %>% 
  group_by(plotcode, time) %>% 
  slice_max(BA, n=1, ) 
  
Matreex_pluritri_essence <- Matreex_pplmt_plurispé %>% 
  group_by(plotcode, time) %>% 
  slice_max(BA, n=1, ) 


##Indicateur Gini essence / Gini total placette toute essence confondue pour Matreex
#Calcul BA total par placette et temps
mat_tot_matreex <- Matreex_pplmt_plurispé %>%
  group_by(plotcode, time) %>%
  summarise(BA_total = sum(BA, na.rm = TRUE), .groups = "drop")

#Ajout du BA total et calcul de la proportion
mat_prop_matreex <- Matreex_pplmt_plurispé %>%
  left_join(mat_tot_matreex, by = c("plotcode", "time")) %>%
  mutate(prop = BA / BA_total)

#Séparer t_ini et t_fin, puis fusionner
mat_tini_matreex <- mat_prop_matreex %>%
  filter(time == 1) %>%
  select(plotcode, species, BA, BA_total, prop_ini = prop)

mat_tfin_matreex <- mat_prop_matreex %>%
  filter(time == 100) %>%
  select(plotcode, species, BA, BA_total, prop_fin = prop)

# Fusion et calcul de l’indicateur
mat_indic_matreex <- full_join(mat_tini_matreex, mat_tfin_matreex, by = c("plotcode", "species")) %>%
  mutate(
    prop_ini = replace_na(prop_ini, 0),
    prop_fin = replace_na(prop_fin, 0),
    indic_diff = prop_fin - prop_ini
  )

# Résultat final Matreex :
head(mat_indic_matreex)

