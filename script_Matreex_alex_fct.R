#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#

#-----------------fonction Packages---------------------------------------
package_a_installer <- function(packages) {
  # Vérifie quels packages ne sont pas encore installés
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  # Installe uniquement ceux qui manquent
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)}
  # Charge tous les packages
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))}
  message("✅ Tous les packages sont maintenant installés et chargés.")}

#----

package_a_installer(c(
  "data.table",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf"))


#---------Recupération des données------------------


mat_data = readRDS("BA_sim.Rds")

colnames(mat_data)[2] = "IDP"
colnames(mat_data)[4:8] = c("Lon", "Lat" , "speciesName",  "Year", "basalAreaSum")

#---------on s'en fou------------

summary(mat_data)
dd <- mat_data %>% group_by(speciesName) %>% summarise(SumBA = sum(basalAreaSum))
ggplot(dd, aes(x = forcats::fct_reorder(speciesName, SumBA), y =SumBA)) +geom_col()+ coord_flip()
# cluster_ID	Lat	Lon


#------boxplot---------------
unique(mat_data$speciesName)

mat_data %>%
  filter(speciesName == "QRub") %>%
  ggplot(aes(x = factor(Year), y = basalAreaSum)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = paste("Distribution de la surface terrière par année de QRub"),
    x = "Année",
    y = "Surface terrière (m²/ha)"
  ) +
  theme_bw()


#----------Carte d'évolution G------------------------------------


unique(mat_data$Year)
#------ Paramètre
t_interest = c(100, 91) #( tfin, tdébut)
choix_ssp = "ssp585"
#------
mat_filtre_diff <- mat_data %>%
  filter(ssp == choix_ssp) %>%
  filter(Year %in% t_interest) %>%
  group_by(IDP, speciesName) %>%
  filter(!(all(basalAreaSum == 0))) %>%
  ungroup()
unique(mat_filtre_diff$Year)

mat_diff <- mat_filtre_diff %>%
  group_by(IDP, ssp, speciesName, Lat, Lon) %>%
  summarise(
    basal_tfin = sum(basalAreaSum[Year == t_interest[1]], na.rm = TRUE),
    basal_tdebut = sum(basalAreaSum[Year == t_interest[2]], na.rm = T),
    .groups = "drop")


mat_total <- mat_filtre_diff %>%
  filter(Year %in% t_interest )%>%
  group_by(IDP, ssp, Year, Lat, Lon) %>%
  summarise(total_basal = sum(basalAreaSum, na.rm = TRUE), .groups = "drop")

mat_diff <- mat_diff %>%
  left_join(
    mat_total %>% filter(Year == t_interest[1]) %>% select(IDP, ssp, Lat, Lon, total_basal_tfin = total_basal),
    by = c("IDP", "ssp", "Lat", "Lon")
  ) %>%
  left_join(
    mat_total %>% filter(Year == t_interest[2]) %>% select(IDP, ssp, Lat, Lon, total_basal_tdebut = total_basal),
    by = c("IDP", "ssp", "Lat", "Lon")
  ) %>%
  mutate(
    diff_valeur_abs = ifelse(basal_tdebut > 0, basal_tfin - basal_tdebut, NA_real_),
    pct_disparition = ifelse(basal_tdebut > 0,
                             ((basal_tdebut - basal_tfin) / basal_tdebut) * 100,
                             NA_real_),
    pct_disparition = ifelse(pct_disparition < 0, NA_real_, pct_disparition),
    pct_apparition = ifelse(basal_tdebut == 0,
                            (basal_tfin /total_basal_tfin) * 100,
                            NA_real_),
    pct_croi = ifelse(is.na(pct_disparition) & is.na(pct_apparition),
                      (((basal_tfin /total_basal_tfin) * 100)-((basal_tdebut / total_basal_tdebut) * 100)),
                      NA_real_))


mat_classi <- mat_diff %>%
  mutate(classe_app = cut(pct_apparition,
                          breaks = c(0, 25, 50, 75, 100),
                          include.lowest = TRUE,
                          labels = c("0–25%", "25–50%", "50–75%", "75–100%")))

mat_classi <- mat_classi %>%
  mutate(classe_disp = cut(pct_disparition,
                           breaks = c(0, 25, 50, 75, 100),
                           include.lowest = TRUE,
                           labels = c("0–25%", "25–50%", "50–75%", "75–100%")))

france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

france_metropole <- france %>%
  st_crop(xmin = -5.5, xmax = 9.8, ymin = 41, ymax = 51.5)

mat_carte_diff <- st_as_sf(mat_classi, coords = c("Lon", "Lat"), crs = 4326)

essence_cible <- "AAlb"

#graph----

ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = mat_carte_diff %>% filter(speciesName == essence_cible, !is.na(diff_valeur_abs)),
    aes(color = diff_valeur_abs),
    size = 2
  ) +
  scale_color_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Evolution de la\nsurface terrière (m²/Ha) :"
  ) +
  labs(
    title = paste("Evolution de la surface terrière sur la période 2001-2100 pour", essence_cible),
    subtitle = "Scénario : ssp585"
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


ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = mat_carte_diff %>% filter(speciesName == essence_cible, !is.na(pct_disparition)),
    aes(color = classe_disp),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "0–25%" = "white",
      "25–50%" = "#ffcccc",
      "50–75%" = "#ff6666",
      "75–100%" = "red"),
    name = "Diminution de la\nsurface terrière (%) :"
  ) +
  labs(
    title = paste("Proportion de diminution de la surface terrière sur la période 2001-2100 pour", essence_cible),
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

ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = mat_carte_diff %>% filter(speciesName == essence_cible, !is.na(pct_apparition)),
    aes(color = classe_app),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "0–25%" = "white",
      "25–50%" = "#ccffcc",
      "50–75%" = "#66cc66",
      "75–100%" = "green"),
    name = "Proportion d'apparition (%) :"
  ) +
  labs(
    title = paste("Proportion d'apparition de", essence_cible,"sur la période 2001-2100"),
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


#-------Carte présence/absence---------------------
seuil_pres = 50

mat_abs_pres <- mat_classi %>%
  mutate(abs_pres = ifelse(basal_tfin > 1,"presence","absence"))

mat_abs_pres <- mat_abs_pres %>% 
  mutate(
    risque_pres = case_when(
      abs_pres == "presence" & pct_disparition > seuil_pres ~ "faible",
      abs_pres == "presence" & pct_disparition <= seuil_pres ~ "moyen",
      abs_pres == "presence" & pct_apparition > 0 | abs_pres == "presence" & pct_croi > 0 ~ "fort")
  )

france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

france_metropole <- france %>%
  st_crop(xmin = -5.5, xmax = 9.8, ymin = 41, ymax = 51.5)

mat_carte_abs_pres <- st_as_sf(mat_abs_pres, coords = c("Lon", "Lat"), crs = 4326)

essence_cible <- "AAlb"

#graph presence/absence
ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = mat_carte_abs_pres %>% filter(speciesName == essence_cible),
    aes(color = abs_pres),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "presence" = "green",
      "absence" = "red"),
    name = "Légende :"
  ) +
  labs(
    title = paste("Carte de la présence/absence de", essence_cible,"sur la période 2001-2100"),
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

#graph gradient presence
ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = mat_carte_abs_pres %>% filter(speciesName == essence_cible, !is.na(risque_pres)),
    aes(color = risque_pres),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "faible" = "red",
      "moyen" = "orange",
      "fort" = "green"),
    name = "Probabilité de présence :"
  ) +
  labs(
    title = paste("Carte de la probabilité de présence de", essence_cible,"sur la période 2001-2100"),
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
