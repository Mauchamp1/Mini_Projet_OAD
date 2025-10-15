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
fonction_table_indicateur <- function(annee_initial,annee_final){
  
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
  
  return(mat_analyse_stat)
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

table_indicateurs_matreex <- fonction_table_indicateur(annee_initial = 1,annee_final = 100)


#----Analyse stat-----------------

