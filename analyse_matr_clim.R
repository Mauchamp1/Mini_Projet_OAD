#-----------------fonction Packages---------------------------------------
fonction_packages <- function(packages) {
  
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)}
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))}
  message("✅ Tous les packages sont maintenant installés et chargés.")
}

#-------Création de la nouvelle table avec les indicateurs----------
fonction_matreex_indicateurs <- function(annee_initial,annee_final){
  
  #recuperation des donnees de modelisation
  df = readRDS("BA_sim.Rds")
  
  colnames(df)[2] = "IDP"
  colnames(df)[4:8] = c("Lon", "Lat" , "speciesName",  "Year", "basalAreaSum")
  
  #creation d'une table à partir des données de la table df avec le filtre du scenario et des deux annees qui nous interresse
  mat_analyse_stat <- df %>%
    filter(ssp == "ssp585") %>%
    filter(Year %in% c(annee_initial, annee_final))
  
  #Ajout des conditions initiales issues de la table df_site dans la table créée préalablement
  mat_analyse_stat <- mat_analyse_stat %>%
    
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
  calcul_melange <- mat_analyse_stat %>% 
    group_by(IDP) %>%
    mutate(prop_G_ini = G_ess_ini / G_total_ini) %>% # Calcul de la proportion de présence de chaque espèce
    summarise(max_prop = max(prop_G_ini, na.rm = TRUE)) %>% # Recherche de l'essence dominante dans la placette
    ungroup() %>% 
    
    # Condition qui définit si la placette est mono ou pluri
    mutate(type_ppl_ini = ifelse(max_prop >= 0.8, "monospecifique","plurispecifique"))
  
  # Ajout de l'information mono/pluri dans la table initiale avec toutes les données
  mat_analyse_stat <- mat_analyse_stat %>%
    left_join(calcul_melange %>% select(IDP, type_ppl_ini), by = "IDP")
  
  return(mat_analyse_stat)
}


fonction_climessence <- function(annee_initial,annee_final){
  
  # Appel des données climessences de simon
  clim_mat <- readRDS("matreex_85_avec_climessences.rds")
  
  colnames(clim_mat)[2] = "IDP"
  colnames(clim_mat)[4:8] = c("Lon", "Lat" , "speciesName",  "Year", "basalAreaSum")
  
  #Ajout des filtres pour ne garder que les scénarios d'intérêt
  clim_mat <- clim_mat %>%
    filter(Year %in% c(annee_initial, annee_final))
  
  #Cette partie permet de tourner la table pour qu'il n'y ait plus de colonne "Year" et qu'il y ait une colonne de G pour chaque annee (2001;2100)
  clim_mat <- clim_mat %>%
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
  clim_mat <- clim_mat %>%
    filter(!(G_ess_ini == 0 & G_ess_final == 0))
  
  return(clim_mat)
}


fonction_table_analyse <- function(annee_initial,annee_final){
  
  #déclenchement des deux fonctions precedentes dans l'ordre ci-dessous
  table_matreex_indic <- fonction_matreex_indicateurs(annee_initial,annee_final)
  table_climessence <- fonction_climessence(annee_initial,annee_final)
  
  #jointure des données clim dans la table phoreau indicateur
  table_matreex_analyse <- table_matreex_indic %>%
    left_join(table_climessence %>% select(IDP, speciesName, valeurs_clim), by = c("IDP", "speciesName"))

  table_matreex_analyse$speciesName <- as.factor(table_matreex_analyse$speciesName)
  table_matreex_analyse$type_ppl_ini <- as.factor(table_matreex_analyse$type_ppl_ini)
  table_matreex_analyse$valeurs_clim<- as.factor(table_matreex_analyse$valeurs_clim)
  
  return(table_matreex_analyse)
}
#----Appel des fonctions----

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

analyse_matreex_clim <- fonction_table_analyse(annee_initial = 1,annee_final = 100)
saveRDS(analyse_matreex_clim, file = "table_matreex_clim_analyse.rds")


annee_initial <- 1
annee_final <- 100

#----Analyse stat-----------------

#modèle linéaire
test <- lm(pct_evol_ess_placette ~ valeurs_clim, data = analyse_matreex_clim)
par(mfrow=c(1,1))
plot(test)
summary(test)

ggplot(analyse_phoreau_clim)+
  geom_boxplot(aes(valeurs_clim,pct_croissance_ess))+
  ylim(-100,100)

test <- lm(pct_croissance_ess ~ valeurs_clim, data = analyse_matreex_clim)
par(mfrow=c(1,1))
plot(test)
summary(test)

ggplot(analyse_matreex_clim)+
  geom_boxplot(aes(valeurs_clim,pct_croissance_ess))+
  ylim(-100,100)

# Anova et Kruskal Wallis
anova_test <- aov(pct_croissance_ess ~ valeurs_clim, data = analyse_matreex_clim)
summary(anova_test)

#Anova significative validation par Tukey 
TukeyHSD(anova_test)

---------------------------------------------------------------------------------------------------
### conclusion on ne suit pas une loi normale, on doit donc passer par des tests non paramétriques
---------------------------------------------------------------------------------------------------
  
# Equivalent Anova 1 facteur en non paramétrique 
kruskal.test(pct_croissance_ess ~ valeurs_clim, data = analyse_matreex_clim)

# test de Dunn (équivalent de Tukey en non paramétrique) 
library(FSA)
dunnTest(pct_croissance_ess ~ valeurs_clim, data = analyse_matreex_clim, method = "bh")

#test de Wilcoxon sur 2 catégories 
wilcox.test(pct_croissance_ess ~ valeurs_clim, data = subset(analyse_matreex_clim, valeurs_clim %in% c("1","2")))
wilcox.test(pct_croissance_ess ~ valeurs_clim, data = subset(analyse_matreex_clim, valeurs_clim %in% c("1","3")))
wilcox.test(pct_croissance_ess ~ valeurs_clim, data = subset(analyse_matreex_clim, valeurs_clim %in% c("2","3")))

#test code de Georges K avec offset
analyse_matreex_clim2 <- subset(analyse_matreex_clim, is.finite(log(G_ess_final)) & is.finite(log(G_ess_ini)))
testoffset=lm(log(G_ess_final) ~ valeurs_clim + offset(log(G_ess_ini)), data=analyse_matreex_clim2)
par(mfrow = c(2, 2))
plot(testoffset)

#test betareg


