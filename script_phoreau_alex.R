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


#---------on s'en fou------------
head(df)
table(df$Year)
summary(df)
dd <- df %>% group_by(speciesName) %>% summarise(SumBA = sum(basalAreaSum))
ggplot(dd, aes(x = forcats::fct_reorder(speciesName, SumBA), y =SumBA)) +geom_col()+ coord_flip()
# cluster_ID	Lat	Lon


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

#-----ajout moyenne G-------------
df <- df %>%
  group_by(speciesName, Year) %>%
  mutate(mean_basalAreaSum = mean(basalAreaSum, na.rm = TRUE)) %>%
  ungroup() 

#-------courbe evolution G------
df %>%
  filter(speciesName == "QRub") %>%
  ggplot(aes(x = Year, y = basalAreaSum)) +
  geom_line(color = "blue", linewidth = 1) +  # courbe
  labs(
    title = "Évolution de la surface terrière moyenne par année - QRub",
    x = "Année",
    y = "Surface basale moyenne (m²/ha)"
  ) +
  theme_minimal()


df %>%
  filter(IDP %in% c("1000001"),speciesName %in% c("QRub", "AAlb","PAbi","FSyl"),Scenario %in% c("BusinessAsUsual_ssp585")) %>%
  ggplot(aes(x = Year, y = basalAreaSum, color = speciesName)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Évolution de la surface terrière moyenne par année",
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
    title = "Évolution de la surface terrière par année en libre évolution",
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

df_joined <- df %>%
  left_join(df_site %>% select(IDP, Lat, Lon), by = "IDP")

essence_choisie <- "AAlb"
annee_choisie <- 2100
scenario_choisi <- "BusinessAsUsual_ssp585"

df_filtre <- df_joined %>%
  filter(
    speciesName == essence_choisie,
    Year == annee_choisie,
    Scenario == scenario_choisi)

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
    legend.position = "right"
  )

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

france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "France")

france_metropole <- france %>%
  st_crop(xmin = -5.5, xmax = 9.8, ymin = 41, ymax = 51.5)

df_carte_diff <- st_as_sf(df_diff, coords = c("Lon", "Lat"), crs = 4326)

essence_cible <- "AAlb"

#graph----

ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey70") +
  geom_sf(
    data = df_carte_diff %>% filter(speciesName == essence_cible, !is.na(diff_valeur_abs)),
    aes(color = diff_valeur_abs),
    size = 2
  ) +
  scale_color_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0,
    name = "Evolution de\nla surface terrière :"
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
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey70") +
  geom_sf(
    data = df_carte_diff %>% filter(speciesName == essence_cible, !is.na(pct_disparition)),
    aes(color = pct_disparition),
    size = 2
  ) +
  scale_color_gradient2(
    low = "white",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Pourcentage de\ndisparition (%) :"
  ) +
  labs(
    title = paste("Diminution de la surface terrière sur la période 2001-2100 pour", essence_cible),
    subtitle = "Scénario : BusinessAsUsual_ssp585"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
    )

ggplot() +
  geom_sf(data = france_metropole, fill = "grey95", color = "grey70") +
  geom_sf(
    data = df_carte_diff %>% filter(speciesName == essence_cible, !is.na(pct_apparition)),
    aes(color = pct_apparition),
    size = 2
  ) +
  scale_color_gradient2(
    low = "white",
    mid = "white",
    high = "green",
    midpoint = 0,
    name = "Pourcentage de\ndisparition (%) :"
  ) +
  labs(
    title = paste("Diminution de la surface terrière sur la période 2001-2100 pour", essence_cible),
    subtitle = "Scénario : BusinessAsUsual_ssp585"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )



