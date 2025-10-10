#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#

install.packages("data.table")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")

#### read tanguy Phoreau simul
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

df_site <- read_excel("Simulation_Site_Table.xlsx")
df_tot <- left_join(df, df_site, by = "IDP")

df <- fread("Site_Species_BA.tsv")

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
    title = paste("Distribution de la surface basale par année de QRub"),
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
  ggplot(aes(x = Year, y = mean_basalAreaSum)) +
  geom_line(color = "blue", linewidth = 1) +  # courbe
  labs(
    title = "Évolution de la surface terrière moyenne par année - QRub",
    x = "Année",
    y = "Surface basale moyenne (m²/ha)"
  ) +
  theme_minimal()


df %>%
  filter(IDP %in% c("1000001"),speciesName %in% c("QRub", "AAlb","PAbi","FSyl")) %>%
  ggplot(aes(x = Year, y = basalAreaSum, color = speciesName)) +
  geom_point(linewidth = 0.6) +
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


