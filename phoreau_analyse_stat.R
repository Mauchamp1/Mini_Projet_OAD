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

#-------Création de la nouvelle table--------------

annee_initial <- 2001
annee_final <- 2100

phoreau_analyse_stat <- df %>%
  filter(Scenario == "BusinessAsUsual_ssp585") %>%
  filter(Year %in% c(annee_initial, annee_final))

phoreau_analyse_stat <- phoreau_analyse_stat %>%
  left_join(df_site %>% select(IDP, Lat, Lon, GRECO, SER, RUM_90cm,
                               Fertility_score, Alt,
                               Diam_Quad_Ini = QuadraticMeanDiameter,
                               Dom_H_Ini = DominantHeight,
                               Dom_Age_Ini = DominantAge,
                               N_GB_Ini = N_GB,
                               N_BM_Ini = N_BM,
                               N_PB_Ini = N_PB,
                               N_Regrowth_Ini = N_Regrowth),
            by = "IDP") %>%
  pivot_wider(
    names_from = Year,
    values_from = basalAreaSum,
    names_prefix = "basalAreaSum_"
  ) %>%
  rename(
    G_ess_ini = paste0("basalAreaSum_", annee_initial),
    G_ess_final = paste0("basalAreaSum_", annee_final)
  ) %>% 
  mutate(
    evol_G_ess = G_ess_final - G_ess_ini
  ) %>% 
  group_by(IDP) %>%
  mutate(
    G_total_ini = sum(G_ess_ini, na.rm = TRUE),
    G_total_final = sum(G_ess_final, na.rm = TRUE)
  ) %>%
  ungroup()
  




