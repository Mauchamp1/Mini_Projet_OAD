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

df_site <- read_excel("Simulation_Site_Table.xlsx")
df <- fread("Site_Species_BA.tsv")


#------boxplot---------------
unique(df$speciesName)

df %>%
  filter(speciesName == "QRub") %>%
  ggplot(aes(x = factor(Year), y = basalAreaSum)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = paste("Distribution de la surface terrière par année de QRub"),
    x = "Année",
    y = "Surface terrière (m²/ha)"
  ) +
  theme_bw()


#-------courbe evolution G------

df %>%
  filter(IDP %in% c("1000001"),
         speciesName %in% c("QRub", "AAlb","PAbi","FSyl"),
         Scenario %in% c("BusinessAsUsual_ssp585")) %>%
  ggplot(aes(x = Year, y = basalAreaSum, color = speciesName)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Évolution de la surface terrière par année",
    x = "Année",
    y = "Surface terrière moyenne (m²/ha)",
    color = "Essence"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

unique(df$Scenario)

df %>%
  filter(IDP %in% c("1000001"),
         Scenario %in% c("BusinessAsUsual_ssp585")) %>%
  ggplot(aes(x = Year, y = basalAreaSum, color = speciesName)) +
  geom_line(size = 1.2) +
  labs(
    title = "Évolution de la surface terrière par année",
    x = "Années",
    y = "Surface terrière (m²/ha)",
    color = "Essence"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )


#-----carte G d'une essence--------------------------

df_filtre <- df %>%
  filter(
    speciesName == "AAlb",
    Year == 2100,
    Scenario == "BusinessAsUsual_ssp585")

df_filtre <- df_filtre %>%
  left_join(df_site %>% select(IDP, Lat, Lon), by = "IDP")

france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

df_sf <- st_as_sf(df_filtre, coords = c("Lon", "Lat"), crs = 4326)

ggplot() +
  geom_sf(data = france, fill = "grey95", color = "grey70") +
  geom_sf(data = df_sf, aes(color = basalAreaSum), size = 2) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = paste("Surface terrière (", essence_choisie, ") en", annee_choisie),
    subtitle = paste("Scénario :", scenario_choisi),
    color = "Surface terrière (m²/ha)"
  ) +
  theme_minimal() +
  coord_sf(xlim = c(-5.5, 10), ylim = c(41, 52), expand = FALSE) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right")


#----------Carte d'évolution G------------------------------------

head(df)
unique(df$Year)

df_filtre_diff <- df %>%
  filter(Scenario == "BusinessAsUsual_ssp585") %>%
  filter(Year %in% c(2001, 2100)) %>%
  group_by(IDP, speciesName) %>%
  filter(!(all(basalAreaSum == 0))) %>%
  ungroup()

df_filtre_diff <- df_filtre_diff %>%
  left_join(df_site %>% select(IDP, Lat, Lon), by = "IDP")

df_diff <- df_filtre_diff %>%
  group_by(IDP, Scenario, speciesName, Lat, Lon) %>%
  summarise(
    basal_2001 = sum(basalAreaSum[Year == 2001], na.rm = TRUE),
    basal_2100 = sum(basalAreaSum[Year == 2100], na.rm = TRUE),
    .groups = "drop"
  )

df_total <- df_filtre_diff %>%
  filter(Year %in% c(2001, 2100)) %>%
  group_by(IDP, Scenario, Year, Lat, Lon) %>%
  summarise(total_basal = sum(basalAreaSum, na.rm = TRUE), .groups = "drop")

df_diff <- df_diff %>%
  left_join(
    df_total %>% filter(Year == 2001) %>% select(IDP, Scenario, Lat, Lon, total_basal_2001 = total_basal),
    by = c("IDP", "Scenario", "Lat", "Lon")
  ) %>%
  left_join(
    df_total %>% filter(Year == 2100) %>% select(IDP, Scenario, Lat, Lon, total_basal_2100 = total_basal),
    by = c("IDP", "Scenario", "Lat", "Lon")
  ) %>%
  mutate(
    diff_valeur_abs = ifelse(basal_2001 > 0, basal_2100 - basal_2001, NA_real_),
    pct_disparition = ifelse(basal_2001 > 0,
                             ((basal_2001 - basal_2100) / basal_2001) * 100,
                             NA_real_),
    pct_disparition = ifelse(pct_disparition < 0, NA_real_, pct_disparition),
    pct_apparition = ifelse(basal_2001 == 0,
                            (basal_2100 / total_basal_2100) * 100,
                            NA_real_),
    pct_croi = ifelse(is.na(pct_disparition) & is.na(pct_apparition),
                      (((basal_2100 / total_basal_2100) * 100)-((basal_2001 / total_basal_2001) * 100)),
                      NA_real_))

df_diff <- df_diff %>%
  mutate(classe_app = cut(pct_apparition,
      breaks = c(0, 25, 50, 75, 100),
      include.lowest = TRUE,
      labels = c("0–25%", "25–50%", "50–75%", "75–100%")))

df_diff <- df_diff %>%
  mutate(classe_disp = cut(pct_disparition,
                          breaks = c(0, 25, 50, 75, 100),
                          include.lowest = TRUE,
                          labels = c("0–25%", "25–50%", "50–75%", "75–100%")))


france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

france_metropole <- france %>%
  st_crop(xmin = -5.5, xmax = 9.8, ymin = 41, ymax = 51.5)

df_carte_diff <- st_as_sf(df_diff, coords = c("Lon", "Lat"), crs = 4326)

essence_cible <- "AAlb"

#graph----

ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = df_carte_diff %>% filter(speciesName == essence_cible, !is.na(diff_valeur_abs)),
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
    data = df_carte_diff %>% filter(speciesName == essence_cible, !is.na(pct_disparition)),
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
    data = df_carte_diff %>% filter(speciesName == essence_cible, !is.na(pct_apparition)),
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

df_abs_pres <- df_diff %>%
  mutate(abs_pres = ifelse(basal_2100 > 1,"presence","absence"))

df_abs_pres <- df_abs_pres %>% 
  mutate(
    risque_pres = case_when(
      abs_pres == "presence" & pct_disparition > 50 ~ "faible",
      abs_pres == "presence" & pct_disparition <= 50 ~ "moyen",
      abs_pres == "presence" & pct_apparition > 0 | abs_pres == "presence" & pct_croi > 0 ~ "fort")
      )

france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

france_metropole <- france %>%
  st_crop(xmin = -5.5, xmax = 9.8, ymin = 41, ymax = 51.5)

df_carte_abs_pres <- st_as_sf(df_abs_pres, coords = c("Lon", "Lat"), crs = 4326)

essence_cible <- "AAlb"

#graph presence/absence
ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey20") +
  geom_sf(
    data = df_carte_abs_pres %>% filter(speciesName == essence_cible),
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
    data = df_carte_abs_pres %>% filter(speciesName == essence_cible, !is.na(risque_pres)),
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



