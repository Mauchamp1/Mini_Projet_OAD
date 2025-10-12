library (dplyr)
library (terra)
library(ggplot2)
#install.packages("readr")
library(readr)
#install.packages("tidyverse")
library(tidyverse)


data_matreex_ini = readRDS("BA_sim.Rds")


# summary(data_matreex_ini)
# head(data_matreex_ini)
# 
# #Garde une seule simu
# simuY <- data_matreex_ini %>%
#   filter(ID.simulation == 30000)
# 
# ggplot(data = data_matreex_ini)+
#   aes(ID.simulation)+
#   geom_bar()


# Supprimer de Matreex les SSP 126
data_matree_ssp585 <- data_matreex_ini %>%
  filter(ssp == "ssp585")

#On passe les espèce en format long et on renseigne le g
data_mat_sp_wide <- data_matree_ssp585 %>%
  pivot_wider(
    names_from = species,
    values_from = BA)
  
# ggplot(data = data_mat_sp_wide)+
#   aes(ID.simulation)+
#   geom_bar()

#Ajout d'une colonne vide

data_mat_sp_wide[ , 'gtot'] <- NA
#Calcul du gtot
data_mat_sp_wide$gtot <- rowSums(data_mat_sp_wide[, 7:32], na.rm = T)

summary (data_mat_sp_wide$gtot)  
erreur_matreex = data_mat_sp_wide %>% 
  filter(gtot > 80) %>% 
  filter (time == 1)
summary(erreur_matreex$gtot)

# for (z in 1:nrow(matree_propg)){
#   for (i in 1:26){
#     x = i+6
#     
#     matree_propg[z,x]= matree_propg[z,x]/matree_propg[z,33]}
#   }


# Calculer la proportion de chaque essence

matree_propg = data_mat_sp_wide

# Extraire la matrice des colonnes à diviser (g.sp)
mat_to_divide <- as.matrix(matree_propg[, 7:32])

# Extraire la matrice répétant la colonne à diviser (gtot)
ref_vector <- matree_propg[[33]]
ref_vector <- matrix(ref_vector, nrow = nrow(matree_propg), ncol = length(7:32))

# Répéter la colonne de division sur toutes les colonnes (recyclage ligne par ligne)
div_matrix <- mat_to_divide / ref_vector

# Réinjecter la matrice dans le data frame
matree_propg[, 7:32] <- as.data.frame(div_matrix)
rm(ref_vector, div_matrix,mat_to_divide)

#Calculerlesindicateur gfin/gini  
t1 = 1 #temps de base
tfin = 100 #temps à comparer

matree_t1 = matree_propg %>% 
  filter(time == t1)
matree_tfin = matree_propg %>% 
  filter(time == tfin)

#indic tfin/tini
indic_div = matree_tfin[,7:32]/matree_t1[,7:32]
indic_div = cbind(matree_propg[,1:6], indic_div)

#indic soustra
indic_soustra = matree_tfin[,7:32]-matree_t1[,7:32]
indic_soustra = cbind(matree_propg[,1:6],indic_soustra)

# indic_évol (écart entre les moyennes des dernières années ?)
# Vérif les indicateurs à faire (cohérence des rés)





#Import données phoreau
data_phoreau_ini <- read_tsv("Data Phoreau/Site_Species_BA.tsv")

data_phoreau_ini$speciesName <- as.factor(data_phoreau_ini$speciesName)
data_phoreau_ini$Scenario <- as.factor(data_phoreau_ini$Scenario)

summary(data_phoreau_ini$Scenario)

phoreau_selec = data_phoreau_ini %>% 
  filter(Scenario == "BusinessAsUsual_ssp585")

#On passe les espèce en format long et on renseigne le g
phoreau_wide <- phoreau_selec %>%
  pivot_wider(
    names_from = speciesName,
    values_from = basalAreaSum)


#Ajout d'une colonne vide

phoreau_wide[ , 'gtot'] <- NA
#Calcul du gtot
phoreau_wide$gtot <- rowSums(phoreau_wide[, 4:105], na.rm = T)

summary (phoreau_wide$gtot)  
#gtot n'est pas affiche sur le tableau car trop de variables

print(phoreau_wide$gtot[1])


# Calculer la proportion de chaque essence

phoreau_propg = phoreau_wide



# former la matrice répétant la colonne à diviser (gtot)
ref_vector <- phoreau_propg[[106]]
ref_vector <- matrix(ref_vector, nrow = nrow(phoreau_propg), ncol = length(4:105))

# Répéter la colonne de division sur toutes les colonnes (recyclage ligne par ligne)
div_matrix <- phoreau_propg[,4:105] / ref_vector

# Réinjecter la matrice dans le data frame
phoreau_propg[, 4:105] <- as.data.frame(div_matrix)
rm(ref_vector, div_matrix)
summary(phoreau_propg)


# #Calculerlesindicateur gfin/gini  
t1 = 2001 #temps de base
tfin = 2100 #temps à comparer
# 
phoreau_t1 = phoreau_propg %>%
  filter(time == t1)
phoreau_tfin = phoreau_propg %>%
  filter(time == tfin)
# 
# #indic tfin/tini
indic_div = phoreau_tfin[,7:32]/phoreau_t1[,7:32]
indic_div = cbind(phoreau_propg[,1:3], indic_div)
# 
# #indic soustra
indic_soustra = phoreau_tfin[,7:32]-phoreau_t1[,7:32]
indic_soustra = cbind(phoreau_propg[,1:3],indic_soustra)

# indic_évol (écart entre les moyennes des dernières années ?)
# Vérif les indicateurs à faire (cohérence des rés)
