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

saveRDS(table_indicateurs_matreex, file = "table_Matreex_indic.Rds")



#----Analyse stat-----------------

#Import résultat Climessences
Clim_res = readRDS("matreex_85_avec_climessences.rds")
table_indicateurs_matreex = readRDS("table_Matreex_indic.Rds")
annee_initial = 1
annee_final = 100
Clim_resfilter = Clim_res %>% filter(  time == annee_final )

table_indicateurs_matreex_clim= left_join(table_indicateurs_matreex,Clim_resfilter[,c(2,6,9)], by = join_by( IDP == plotcode ,speciesName== species ) )

table_indicateurs_matreex_clim$valeurs_clim = as.factor(table_indicateurs_matreex_clim$valeurs_clim)

# ggplot(data = table_indicateurs_matreex_clim, aes(x= valeurs_clim, y = pct_croissance_ess)) +geom_boxplot() 
# 
saveRDS(table_indicateurs_matreex_clim, file = "table_Matreex_clim_indic.Rds")
matree_clime = table_indicateurs_matreex_clim

matree_clime = matree_clime %>% 
  filter(!is.na(valeurs_clim) & G_ess_ini > 1)
#Premieres analyses

#On regarde le pct de croissance en fonction de la valeur de Climessences
ggplot(data = matree_clime, aes(x= valeurs_clim, y = pct_croissance_ess)) +geom_boxplot() 

print(matree_clime %>% slice_max(pct_croissance_ess, n = 10))


lm_tt_evoless = lm(log(pct_croissance_ess)~ valeurs_clim , data = matree_clime)
#utilisation du log pour corriger la normalité des résidus
summary(lm_tt_evoless)
par(mfrow = c(2,2))
plot(lm_tt_evoless)

#La différence est significative mais très faibles est peu explicative. On observe que lorsque La proba de survie de Climessences est inf à 99%, en moyenne la croissance prédite par Matreex augmente. Ce qui est un résultat pertinent. 
#On observe en revanche que lorsque la proba de CLimessences est comprise entre 99% et 97,5% alors la croissance moyenne diminue (par rapport aux valeur ou Climessences == 1)

#Test en ne gradant que les catégorie 1 et 3 de Climessences
clim1_3 = matree_clime %>% 
  filter(valeurs_clim == 1 | valeurs_clim == 3)

lm_clim1_3 = lm(log(pct_croissance_ess)~ valeurs_clim , data = test)#On utilise le log de pct_croissance
summary(lm_test)
par(mfrow = c(2,2))
plot(lm_test)
# Pas de diff notable par rapport au test précédent

#Compa Matreex/ Phoreau sur analyse _phor_clim







# Test avec indicateur evol % essences dans la placette

lm_tt_prop_ess = lm( pct_evol_ess_placette ~valeurs_clim ,data = matree_clime)
summary(lm_tt_prop_ess)
par(mfrow = c(2,2))
plot(lm_tt_prop_ess)
anova(lm_tt_prop_ess)

shapiro.test(sample(matree_clime$pct_evol_ess_placette,4999, replace = F))

#Pb dans le Q-Q plot et mettre le log fonctionne pas


shapiro.test(sample(matree_clime$pct_evol_ess_placette,4999, replace = F))


# matrice de corrrespondance

pres_abs = matree_clime %>% 
  mutate( evolution = case_when(
    pct_croissance_ess == -100~"disp", #plus de 10% du gtot en 2001 moins de 1% en 2100
    pct_croissance_ess> 0 | pct_evol_ess_placette > 0 & G_total_final- G_total_ini >0~ "exp", #pluis de 10% du gtot 2100 et une augmentation depuis la situation initiale
    pct_croissance_ess<0 ~ "declin",#plus de 10% en 2001, puis un déclin mais un g qui reste supérieur à 1% du gtot
    T ~NA
  ))
pres_abs$evolution = as.factor(pres_abs$evolution)
summary(pres_abs$evolution)

table (pres_abs$valeurs_clim, pres_abs$evolution)
226/1353
798/5995

1015/5203
#On observe que Matreex prédit beaucoup plus de déclin que Climessence (environ 2000)

#Carte ok/pas OK
corresp_clim_Matree = pres_abs %>% 
  mutate( ok_pasok = case_when(
    evolution == "declin" & (valeurs_clim == 2 | valeurs_clim == 3)  ~"OK", #plus de 10% du gtot en 2001 moins de 1% en 2100
    evolution == "exp" &  valeurs_clim == 1 ~"OK", #pluis de 10% du gtot 2100 et une augmentation depuis la situation initiale
    T ~"pasOK"
  ))
corresp_clim_Matree$ok_pasok = as.factor(corresp_clim_Matree$ok_pasok)
summary(corresp_clim_Matree$ok_pasok)



france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

france_metropole <- france %>%
  st_crop(xmin = -5.5, xmax = 9.8, ymin = 41, ymax = 51.5)

mat_carte_okpasok <- st_as_sf(corresp_clim_Matree, coords = c("Lon", "Lat"), crs = 4326)


#graph presence/absence
ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = mat_carte_okpasok %>%  filter(valeurs_clim != 1),
    aes(color = ok_pasok),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "OK" = "green",
      "pasOK" = "red"),
    name = "Légende :"
  ) +
  labs(
    title = paste("Correspondance entre Climessences et Matreex"),
    subtitle = "Scénario : BusinessAsUsual_ssp585"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.background = element_rect(fill = "grey90", color = NA),
    legend.box.background = element_rect(color = "black"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
