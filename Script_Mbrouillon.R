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
tfin_9 = tfin- 9

matree_t1 = matree_propg %>% 
  filter(time == t1)
matree_tfin = matree_propg %>% 
  filter(time == tfin)

#indic tfin/tini
mat_indic_div = matree_tfin[,7:32]/matree_t1[,7:32]
mat_indic_div = cbind(matree_propg[,1:6], mat_indic_div)

#indic soustra
mat_indic_soustra = matree_tfin[,7:32]-matree_t1[,7:32]
mat_indic_soustra = cbind(matree_propg[,1:6],mat_indic_soustra)



# Indic tfin/tfin-9
mat__tfin_9 = matree_propg %>%
  filter(time == tfin_9)
mat_indic_div_9last = matree_tfin[,7:32]/mat__tfin_9[,7:32]
mat_indic_div_9last = cbind(matree_propg[,1:6], mat_indic_div_9last)

#indic prés/abs
#Réorganise le tableau avec les espèces en long et les années sélec en large (bien penser à enlever gtot qui varie selon l'année)
mat_pres_abs = matree_propg[,1:32] %>% 
  filter (time == tfin| time == t1) %>% 
  pivot_longer(cols = 7:32,
               names_to = "species",
               values_to = "BA" ) %>% 
  pivot_wider(
    names_from = time,
    values_from = BA)
#Attribue une évolution selon les conditions citées
mat_pres_abs = mat_pres_abs %>% 
  mutate( evolution = case_when(
    `1` > 0.1 & `100`< 0.01~"disp", #plus de 10% du gtot en 2001 moins de 1% en 2100
    `100`>0.1   & `100`/`1`> 1 ~ "exp", #pluis de 10% du gtot 2100 et une augmentation depuis la situation initiale
    `1`>0.1   & `100`/`1`< 1 & `100`> 0.01~ "declin",#plus de 10% en 2001, puis un déclin mais un g qui reste supérieur à 1% du gtot
    T ~ NA
  ))
# indic_évol (écart entre les moyennes des dernières années ?)
# Vérif les indicateurs à faire (cohérence des rés)


##########################################################################################################

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
tfin_9 = tfin-9

phoreau_t1 = phoreau_propg %>%
  filter(Year == t1)
phoreau_tfin = phoreau_propg %>%
  filter(Year == tfin)


# #indic tfin/tini
ph_indic_div = phoreau_tfin[,4:105]/phoreau_t1[,4:105]
ph_indic_div = cbind(phoreau_propg[,1:3], ph_indic_div)

# #indic soustra
ph_indic_soustra = phoreau_tfin[,4:105]-phoreau_t1[,4:105]
ph_indic_soustra = cbind(phoreau_propg[,1:3],ph_indic_soustra)

# Indic tfin/tfin-9
phoreau_tfin_9 = phoreau_propg %>%
  filter(Year == tfin_9)
ph_indic_div_9last = phoreau_tfin[,4:105]/phoreau_tfin_9[,4:105]# On divise pr ttes les sp entre années defin et celles neufs ans plus tôt
ph_indic_div_9last = cbind(phoreau_propg[,1:3], ph_indic_div_9last)

#indic prés/abs
#Réorganise le tableau avec les espèces en long et les années sélec en large (bien penser à enlever gtot qui varie selon l'année)
ph_indic_pres_abs = phoreau_propg[,1:105] %>% 
  filter (Year == tfin| Year == t1) %>% 
  pivot_longer(cols = 4:105,
               names_to = "speciesName",
               values_to = "basalAreaSum" ) %>% 
  pivot_wider(
    names_from = Year,
    values_from = basalAreaSum)
#Attribue une évolution selon les conditions citées
ph_indic_pres_abs = ph_indic_pres_abs %>% 
  mutate( evolution = case_when(
    `2001` > 0.1 & `2100`< 0.01~"disp", #plus de 10% du gtot en 2001 moins de 1% en 2100
    `2100`>0.1   & `2100`/`2001`> 1 ~ "exp", #pluis de 10% du gtot 2100 et une augmentation depuis la situation initiale
    `2001`>0.1   & `2100`/`2001`< 1 & `2100`> 0.01~ "declin",#plus de 10% en 2001, puis un déclin mais un g qui reste supérieur à 1% du gtot
    T ~ NA
  ))


# indic_évol (écart entre les moyennes des dernières années ?)
# Vérif les indicateurs à faire (cohérence des rés)
