#----Fonction Packages----------------------------------------------------------
fonction_packages <- function(packages) {
  
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)}
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))}
  message("✅ Tous les packages sont maintenant installés et chargés.")
}

#----Fonction creation table indicateurs----------------------------------------

#fonction_phoreau
fonction_phoreau_indicateurs <- function(annee_initial,annee_final){
  
  #recuperation des donnees de modelisation
  df_site <- read_excel("Simulation_Site_Table.xlsx")
  df <- fread("Site_Species_BA.tsv")
  
  #creation d'une table à partir des données de la table df avec le filtre du scenario et des deux annees qui nous interresse
  phoreau_analyse_stat <- df %>%
    filter(Scenario == "BusinessAsUsual_ssp585") %>%
    filter(Year == annee_initial | (Year >= annee_final - 14 & Year <= annee_final)) %>%
    group_by(IDP,Scenario, speciesName) %>%
    summarise(
      G_ess_ini = basalAreaSum[Year == annee_initial],
      G_ess_final = mean(basalAreaSum[Year >= annee_final - 14 & Year <= annee_final], na.rm = TRUE),
                         .groups = "drop") %>%
    ungroup()
  
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

#fonction_matreex
fonction_matreex_indicateurs <- function(annee_initial,annee_final){
  
  #recuperation des donnees de modelisation
  df = readRDS("BA_sim.Rds")
  
  colnames(df)[2] = "IDP"
  colnames(df)[4:8] = c("Lon", "Lat" , "speciesName",  "Year", "basalAreaSum")
  
  #creation d'une table à partir des données de la table df avec le filtre du scenario et des deux annees qui nous interresse
  mat_analyse_stat <- df %>%
    filter(ssp == "ssp585") %>%
    filter(Year == annee_initial | (Year >= annee_final - 14 & Year <= annee_final)) %>% 
    group_by(IDP,ssp,speciesName,Lon,Lat) %>%
    summarise(
      G_ess_ini = basalAreaSum[Year == annee_initial],
      G_ess_final = mean(basalAreaSum[Year >= annee_final - 14 & Year <= annee_final], na.rm = TRUE),
                         .groups = "drop") %>%
    ungroup()
  
  #Ajout des conditions initiales issues de la table df_site dans la table créée préalablement
  mat_analyse_stat <- mat_analyse_stat %>%
    
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

fonction_climessence2 <- function(annee_initial,annee_final){
  
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

fonction_table_analyse2 <- function(annee_initial,annee_final){
  
  #déclenchement des deux fonctions precedentes dans l'ordre ci-dessous
  table_matreex_indic <- fonction_matreex_indicateurs(annee_initial,annee_final)
  table_climessence2 <- fonction_climessence2(annee_initial,annee_final)
  
  #jointure des données clim dans la table phoreau indicateur
  table_matreex_analyse <- table_matreex_indic %>%
    left_join(table_climessence2 %>% select(IDP, speciesName, valeurs_clim), by = c("IDP", "speciesName"))
  
  table_matreex_analyse$speciesName <- as.factor(table_matreex_analyse$speciesName)
  table_matreex_analyse$type_ppl_ini <- as.factor(table_matreex_analyse$type_ppl_ini)
  table_matreex_analyse$valeurs_clim<- as.factor(table_matreex_analyse$valeurs_clim)
  
  return(table_matreex_analyse)
}

#----Appel des fonctions--------------------------------------------------------

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

analyse_phoreau_clim <- fonction_table_analyse(
  annee_initial = 2001,
  annee_final = 2100)
analyse_matreex_clim <- fonction_table_analyse2(
  annee_initial = 1,
  annee_final = 100)

#enregistrer les tables au format RDS
saveRDS(analyse_phoreau_clim, file = "table_phoreau_clim_analyse.rds")
saveRDS(analyse_matreex_clim, file = "table_matreex_clim_analyse.rds")

#----Statistique----------------------------------------------------------------



##Calcul Spearman par groupe climatique


analyse_matreex_clim_etage <- analyse_matreex_clim %>% 
  mutate(
    etage = case_when(
      speciesName %in% c("Abies_alba","Pinus_sylvestris","Picea_abies") ~ "montagnard",
      speciesName %in% c("Carpinus_betulus","Quercus_robur",
                         "Quercus_petraea","Fagus_sylvatica") ~ "plaine",
      speciesName %in% c("Pinus_pinaster","Pinus_halepensis",
                         "Quercus_ilex","Quercus_pubescens") ~ "mediterraneen",
      TRUE ~ NA_character_
    )
  )%>% 
  filter(!is.na(etage)) %>% #supprime toutes les lignes qui ont des NA
  rename(valeurs_climessence = valeurs_clim)


analyse_phoreau_clim_etage <- analyse_phoreau_clim %>% 
  mutate(
    etage = case_when(
      speciesName %in% c("Abies_alba","Pinus_sylvestris","Picea_abies") ~ "montagnard",
      speciesName %in% c("Carpinus_betulus","Quercus_robur",
                         "Quercus_petraea","Fagus_sylvatica") ~ "plaine",
      speciesName %in% c("Pinus_pinaster","Pinus_halepensis",
                         "Quercus_ilex","Quercus_pubescens") ~ "mediterraneen",
      TRUE ~ NA_character_
    )
  )%>% 
  filter(!is.na(etage)) %>% #supprime toutes les lignes qui ont des NA



##Ré-Organisation du df par catégorie 

phoreau_clean <- analyse_phoreau_clim_etage %>%
  st_drop_geometry() %>%  # enlever la géométrie
  dplyr::select(type_ppl_ini, pct_croissance_ess, valeurs_climessence, speciesName, etage) %>%  # garder que l'essentiel
  mutate(valeurs_clim = as.numeric(valeurs_climessence)) %>%  # convertir en numérique
  filter(!is.na(pct_croissance_ess) & !is.na(valeurs_climessence))   # enlever NA

matreex_clean <- analyse_matreex_clim_etage %>%
  st_drop_geometry() %>%  # enlever la géométrie
  dplyr::select(type_ppl_ini, pct_croissance_ess, valeurs_climessence, speciesName, etage) %>%  # garder que l'essentiel
  mutate(valeurs_clim = as.numeric(valeurs_climessence)) %>%  # convertir en numérique
  filter(!is.na(pct_croissance_ess) & !is.na(valeurs_climessence))   # enlever NA

##spearman par catégorie 

corr_type_ppl_essence <- matreex_clean %>%
  group_by(etage, type_ppl_ini) %>%
  summarise(
    spearman_rho = if (length(unique(valeurs_clim)) > 1) {
      suppressWarnings(cor(pct_croissance_ess, valeurs_clim, method = "spearman"))
    } else NA_real_,
    p_value = if (length(unique(valeurs_clim)) > 1) {
      suppressWarnings(cor.test(pct_croissance_ess, valeurs_clim, method = "spearman", exact = FALSE)$p.value)
    } else NA_real_,
    mean_1 = mean(pct_croissance_ess[valeurs_clim == 1], na.rm = TRUE),
    mean_2 = mean(pct_croissance_ess[valeurs_clim == 2], na.rm = TRUE),
    mean_3 = mean(pct_croissance_ess[valeurs_clim == 3], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    relation = case_when(
      !is.na(p_value) & p_value < 0.05 & spearman_rho < 0 ~ "Forte (bonne note → haute croissance)",
      !is.na(p_value) & p_value < 0.05 & spearman_rho > 0 ~ "Forte (bonne note → basse croissance)",
      TRUE ~ "Faible / Non significative"
    ),
    interpretation = case_when(
      is.na(p_value) ~ "Non calculable (trop peu de notes distinctes)",
      p_value >= 0.05 ~ "Aucun lien significatif entre note et croissance",
      spearman_rho < 0 ~ "Relation attendue : les bonnes notes sont associées à une meilleure croissance",
      spearman_rho > 0 ~ "Relation inversée : les mauvaises notes montrent une croissance plus forte",
      TRUE ~ "Cas particulier"
    )
  )


# --- Résultat final par essence matreex ---*
corr_type_ppl_essence %>%
  arrange(p_value)
print(corr_type_ppl_essence, n=Inf)

ggplot(corr_type_ppl_essence[c(1,2,3,4,5,6),], aes(x = reorder(type_ppl_ini, spearman_rho), y = spearman_rho, fill = relation))+
  geom_col(color = "black", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_text(
    aes(
      label = ifelse(p_value < 0.05, paste0("ρ=", round(spearman_rho, 2)), "")
    ),
    vjust = ifelse(corr_type_ppl_essence[c(1,2,3,4,5,6),]$spearman_rho >= 0, -0.3, 1.2),
    size = 2.8,
    angle = 90,
    hjust = ifelse(corr_type_ppl_essence[c(1,2,3,4,5,6),]$spearman_rho >= 0, 0, 1)
  ) +
  scale_fill_manual(
    values = c(
      "Forte (bonne note → haute croissance)" = "#2E8B57",
      "Forte (bonne note → basse croissance)" = "#E74C3C",
      "Faible / Non significative" = "lightgrey"
    ),
    name = "Type de relation"
  ) +
  labs(
    title = "Corrélation entre la note climessence et la croissance selon le type de peuplement",
    subtitle = "Comparaison matreex et Climessence",
    x = "type de peuplement initial",
    y = "Coefficient de corrélation de Spearman (ρ)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )+
  facet_wrap(~ etage, scales = "free_x")


##spearman par essence phoreau

corr_type_ppl_essence_phor <- phoreau_clean %>%
  group_by(speciesName, type_ppl_ini) %>%
  summarise(
    spearman_rho = if (length(unique(valeurs_climessence)) > 1) {
      suppressWarnings(cor(pct_croissance_ess, valeurs_climessence, method = "spearman"))
    } else NA_real_,
    p_value = if (length(unique(valeurs_climessence)) > 1) {
      suppressWarnings(cor.test(pct_croissance_ess, valeurs_climessence, method = "spearman", exact = FALSE)$p.value)
    } else NA_real_,
    mean_1 = mean(pct_croissance_ess[valeurs_climessence == 1], na.rm = TRUE),
    mean_2 = mean(pct_croissance_ess[valeurs_climessence == 2], na.rm = TRUE),
    mean_3 = mean(pct_croissance_ess[valeurs_climessence == 3], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    relation = case_when(
      !is.na(p_value) & p_value < 0.05 & spearman_rho < 0 ~ "Forte (bonne note → haute croissance)",
      !is.na(p_value) & p_value < 0.05 & spearman_rho > 0 ~ "Forte (bonne note → basse croissance)",
      TRUE ~ "Faible / Non significative"
    ),
    interpretation = case_when(
      is.na(p_value) ~ "Non calculable (trop peu de notes distinctes)",
      p_value >= 0.05 ~ "Aucun lien significatif entre note et croissance",
      spearman_rho < 0 ~ "Relation attendue : les bonnes notes sont associées à une meilleure croissance",
      spearman_rho > 0 ~ "Relation inversée : les mauvaises notes montrent une croissance plus forte",
      TRUE ~ "Cas particulier"
    )
  )


# --- Résultat final par essence phoreau---
corr_type_ppl_essence_phor %>%
  arrange(p_value)
print(corr_type_ppl_essence_phor, n = Inf) %>% 
  drop_na()



ggplot(corr_type_ppl_essence_phor[c(9,10,19,20),], aes(x = reorder(type_ppl_ini, spearman_rho), y = spearman_rho, fill = relation))+
  geom_col(color = "black", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_text(
    aes(
      label = ifelse(p_value < 0.05, paste0("ρ=", round(spearman_rho, 2)), "")
    ),
    vjust = ifelse(corr_type_ppl_essence_phor[c(9,10,19,20),]$spearman_rho >= 0, -0.3, 1.2),
    size = 2.8,
    angle = 90,
    hjust = ifelse(corr_type_ppl_essence_phor[c(9,10,19,20),]$spearman_rho >= 0, 0, 1)
  ) +
  facet_wrap(~speciesName, scales="free_x")+
  scale_fill_manual(
    values = c(
      "Forte (bonne note → haute croissance)" = "#2E8B57",
      "Forte (bonne note → basse croissance)" = "#E74C3C",
      "Faible / Non significative" = "lightgrey"
    ),
    name = "Type de relation"
  ) +
  labs(
    title = "Corrélation entre la note climessence et la croissance selon le type de peuplement",
    subtitle = "Comparaison Phoreau et Climessence",
    x = "type de peuplement initial",
    y = "Coefficient de corrélation de Spearman (ρ)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )


# --- Fonction bootstrap pour Spearman phoreau ---
bootstrap_spearman_diff <- function(data1, data2, n_iter = 5000) {
  diffs <- numeric(n_iter)
  for (i in 1:n_iter) {
    sample1 <- data1[sample(nrow(data1), replace = TRUE), ]
    sample2 <- data2[sample(nrow(data2), replace = TRUE), ]
    
    rho1 <- suppressWarnings(cor(sample1$pct_croissance_ess, sample1$valeurs_climessence, method = "spearman"))
    rho2 <- suppressWarnings(cor(sample2$pct_croissance_ess, sample2$valeurs_climessence, method = "spearman"))
    
    diffs[i] <- rho1 - rho2
  }
  return(diffs)
}


# --- Boucle sur les essences ---
resultats_bootstrap <- list()

for (ess in essences_valides) {
  data_ess <- phoreau_clean %>%
    filter(speciesName == ess, !is.na(pct_croissance_ess), !is.na(valeurs_climessence))
  
  data_mono <- data_ess %>% filter(type_ppl_ini == "monospecifique")
  data_pluri <- data_ess %>% filter(type_ppl_ini == "plurispecifique")
  
  if (nrow(data_mono) >= 10 & nrow(data_pluri) >= 10) {  # éviter les petits échantillons
    boot <- bootstrap_spearman_diff(data_mono, data_pluri)
    
    ci <- quantile(boot, probs = c(0.025, 0.975), na.rm = TRUE)
    mean_diff <- mean(boot, na.rm = TRUE)
    significant <- ci[1] > 0 | ci[2] < 0
    
    resultats_bootstrap[[ess]] <- data.frame(
      speciesName = ess,
      mean_diff = mean_diff,
      ci_lower = ci[1],
      ci_upper = ci[2],
      significant = significant
    )
  }
}

# --- Résultats finaux ---
df_resultats <- do.call(rbind, resultats_bootstrap)
print(df_resultats)
