
#-------------------------------------------------------------------------------
#-----------------Attention!!!!-------------------------------------------------

# Il est nécessaire d'avoir les fichiers suivants dans le dossier du script :

# Site_Species_BA.tsv
# Simulation_Site_Table.xlsx
# phoreau_avec_climessences.rds

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



#-------fonction Packages----------------------------
fonction_packages <- function(packages) {
  
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)}
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))}
  message("✅ Tous les packages sont maintenant installés et chargés.")
  }

#-------Fonction creation table indicateurs----------
fonction_phoreau_indicateurs <- function(annee_initial,annee_final){
  
  #recuperation des donnees de modelisation
  df_site <- read_excel("Simulation_Site_Table.xlsx")
  df <- fread("Data Phoreau/Site_Species_BA.tsv")
  
  #creation d'une table à partir des données de la table df avec le filtre du scenario et des deux annees qui nous interresse
  phoreau_analyse_stat <- df %>%
    filter(Scenario == "BusinessAsUsual_ssp585") %>%
    filter(Year %in% c(annee_initial, annee_final))
  
  #Ajout des conditions initiales issues de la table df_site dans la table créée préalablement
  phoreau_analyse_stat <- phoreau_analyse_stat %>%
    left_join(df_site %>% select(IDP, Lat, Lon, GRECO, SER, RUM_90cm,
                                 Fertility_score, Alt,
                                 Diam_Quad_Ini = QuadraticMeanDiameter,
                                 Dom_H_Ini = DominantHeight,
                                 Dom_Age_Ini = DominantAge,
                                 N_GB_Ini = N_GB,
                                 N_BM_Ini = N_BM,
                                 N_PB_Ini = N_PB,
                                 N_Regrowth_Ini = N_Regrowth), by = "IDP") %>%
    
    #Cette partie permet de tourner la table pour qu'il n'y ait plus de colonne "Year" et qu'il y ait une colonne de G pour chaque annee (2001;2100)
    pivot_wider(
      names_from = Year,
      values_from = basalAreaSum,
      names_prefix = "basalAreaSum_"
    ) %>%
    rename(
      G_ess_ini = paste0("basalAreaSum_", annee_initial),
      G_ess_final = paste0("basalAreaSum_", annee_final)
    ) %>% 
    
    #Ajout du calcul pour obtenir l'évolution du G de l'essence (valeur absolue)
    mutate(
      evol_G_ess = G_ess_final - G_ess_ini
    ) %>% 
    
    # Somme des G de chaque placette pour avec la surface terrière totale pour chaque placette en 2001 et en 2100 
    group_by(IDP) %>%
    mutate(
      G_total_ini = sum(G_ess_ini, na.rm = TRUE),
      G_total_final = sum(G_ess_final, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    
    # Calcul des pourcentages de croissance de l'essence et d'évolution de l'essence dans la placette
    mutate(
      pct_croissance_ess = ifelse(G_ess_ini > 0,((G_ess_final*100)/G_ess_ini)-100,NA_real_),
      pct_evol_ess_placette = ((G_ess_final*100)/G_total_final)-((G_ess_ini*100)/G_total_ini)
    ) %>%
    
    # Suppression de toutes les lignes qui ont une essence qui n'est pas présente sur la placette
    filter(!(G_ess_ini == 0 & G_ess_final == 0))
    
    # Création d'une nouvelle table fictive pour le calcul de l'essence dominante dans la placette
  calcul_melange <- phoreau_analyse_stat %>% 
    group_by(IDP) %>%
    mutate(prop_G_ini = G_ess_ini / G_total_ini) %>% # Calcul de la proportion de présence de chaque espèce
    summarise(max_prop = max(prop_G_ini, na.rm = TRUE)) %>% # Recherche de l'essence dominante dans la placette
    ungroup() %>% 
    
    # Condition qui définit si la placette est mono ou pluri
    mutate(type_ppl_ini = ifelse(max_prop >= 0.8, "monospecifique","plurispecifique"))
  
  # Ajout de l'information mono/pluri dans la table initiale avec toutes les données
  phoreau_analyse_stat <- phoreau_analyse_stat %>%
    left_join(calcul_melange %>% select(IDP, type_ppl_ini), by = "IDP")
  
  return(phoreau_analyse_stat)
}

fonction_climessence <- function(annee_initial,annee_final){
  
  # Appel des données climessences de simon
  clim_phor <- readRDS("phoreau_avec_climessences.rds")
  
  #Ajout des filtres pour ne garder que les scénarios d'intérêt
  clim_phor <- clim_phor %>%
    filter(Scenario == "BusinessAsUsual_ssp585") %>%
    filter(Year %in% c(annee_initial, annee_final))
  
  #Cette partie permet de tourner la table pour qu'il n'y ait plus de colonne "Year" et qu'il y ait une colonne de G pour chaque annee (2001;2100)
  clim_phor <- clim_phor %>%
    pivot_wider(
      names_from = Year,
      values_from = basalAreaSum,
      names_prefix = "basalAreaSum_"
    ) %>%
    rename(
      G_ess_ini = paste0("basalAreaSum_", annee_initial),
      G_ess_final = paste0("basalAreaSum_", annee_final)
    )
  
  # Suppression de toutes les lignes qui ont une essence qui n'est pas présente sur la placette
  clim_phor <- clim_phor %>%
    filter(!(G_ess_ini == 0 & G_ess_final == 0))
  
  return(clim_phor)
}

fonction_table_analyse <- function(annee_initial,annee_final){
  
  #déclenchement des deux fonctions precedentes dans l'ordre ci-dessous
  table_phoreau_indic <- fonction_phoreau_indicateurs(annee_initial,annee_final)
  table_climessence <- fonction_climessence(annee_initial,annee_final)
  
  #jointure des données clim dans la table phoreau indicateur
  table_phoreau_analyse <- table_phoreau_indic %>%
    left_join(table_climessence %>% select(IDP, speciesName, Nom_latin, Code_climessence, valeurs_climessence), by = c("IDP", "speciesName"))
  
  table_phoreau_analyse$Scenario <- as.factor(table_phoreau_analyse$Scenario)
  table_phoreau_analyse$speciesName <- as.factor(table_phoreau_analyse$speciesName)
  table_phoreau_analyse$GRECO <- as.factor(table_phoreau_analyse$GRECO)
  table_phoreau_analyse$SER <- as.factor(table_phoreau_analyse$SER)
  table_phoreau_analyse$Fertility_score <- as.factor(table_phoreau_analyse$Fertility_score)
  table_phoreau_analyse$type_ppl_ini <- as.factor(table_phoreau_analyse$type_ppl_ini)
  table_phoreau_analyse$Nom_latin <- as.factor(table_phoreau_analyse$Nom_latin)
  table_phoreau_analyse$Code_climessence <- as.factor(table_phoreau_analyse$Code_climessence)
  table_phoreau_analyse$valeurs_climessence <- as.factor(table_phoreau_analyse$valeurs_climessence)
  
  return(table_phoreau_analyse)
}

#------Appel des fonctions------

fonction_packages(c(
  "data.table",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "corrplot"))

analyse_phoreau_clim <- fonction_table_analyse(annee_initial = 2001,annee_final = 2100)
saveRDS(analyse_phoreau_clim, file = "table_phoreau_clim_analyse.rds")

summary(analyse_phoreau_clim$Scenario)
#----Analyse stat-----------------

#----Premiers test--------------
test <- lm(pct_croissance_ess ~ valeurs_climessence, data = analyse_phoreau_clim )
par(mfrow=c(2,2))
plot(test)
summary(test)
shapiro.test(sample(analyse_phoreau_clim$pct_croissance_ess, 4999,replace = F))

ggplot(analyse_phoreau_clim)+
  geom_boxplot(aes(valeurs_climessence,pct_croissance_ess))+
  ylim(-100,100)

#Test en enlevant les NA de CLimessences
phoclim_filter = analyse_phoreau_clim %>% 
  filter(!is.na(Code_climessence))
  
test_2 <- lm(pct_croissance_ess ~ valeurs_climessence, data = phoclim_filter )
par(mfrow=c(2,2))
plot(test_2)
summary(test_2)
shapiro.test(sample(phoclim_filter$pct_croissance_ess, 4999,replace = F)) 

#-----Compa Matreex et Phoreau--------
#Import des données de Matreex
mat_clim =readRDS("table_Matreex_clim_indic.Rds")  
colnames(mat_clim)[14] = "Code_climessence"

mat_clim_filter = mat_clim %>% 
  filter(!is.na(Code_climessence))  

lm_mat_pho = lm(mat_clim_filter$pct_croissance_ess ~ phoclim_filter$pct_croissance_ess)
summary(lm_mat_pho)

par(mfrow = c(2,2))
plot(lm_mat_pho)

#Obtention SER
#install.package(happign)
library(happign)
#install.packages(sf)
library(sf)
library (dplyr)
#install.packages(tmap)
library(tmap)
library(terra)
library(lidR)

all_layers <- get_layers_metadata("wms-r")


#Données matreex en FR
france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

france_metropole <- france %>%
  st_crop(xmin = -5.5, xmax = 9.8, ymin = 41, ymax = 51.5)

mat_clim_fr <- st_as_sf(mat_clim_filter, coords = c("Lon", "Lat"), crs = 4326)

mat_clim_fr_clip <- st_intersection(mat_clim_fr, france_metropole)#Placettes en france métropolitaine

class(mat_clim_fr)
class(france_metropole)

unique(mat_clim_fr_clip$speciesName)
unique(phoclim_filter$speciesName)

mat_sp_filter = mat_clim_fr_clip %>% 
  filter(speciesName %in% c("Quercus_petraea", "Quercus_ilex", "Quercus_suber", "Quercus_pubescens", "Quercus_faginea","Quercus_pyrenaica", "Quercus_robur"  ))
pho_sp_filter = phoclim_filter %>% 
  filter(speciesName %in% c("QPub", "QRob","QRub", "QPyr" , "QIle", "QFag", "QSub", "QPet" ))


sample_pho_filter_sp <- pho_sp_filter %>%
  dplyr::slice_sample(n = 2680) 

lm_mat_pho_quercus = lm(sample_pho_filter_sp$pct_croissance_ess ~ mat_sp_filter$pct_croissance_ess)
summary(lm_mat_pho_quercus)


