library (dplyr)
library (terra)
library(ggplot2)
install.packages("readr")
library(readr)

data_matreex_ini = readRDS("BA_sim.Rds")


summary(data_matreex_ini)
head(data_matreex_ini)

simuY <- data_matreex_ini %>%
  filter(ID.simulation == 30000)

ggplot(data = data_matreex_ini)+
  aes(ID.simulation)+
  geom_bar()




data_phoreau_ini <- read_tsv("Data Phoreau/Site_Species_BA.tsv")

data_phoreau_ini$speciesName <- as.factor(data_phoreau_ini$speciesName)
data_phoreau_ini$Scenario <- as.factor(data_phoreau_ini$Scenario)

summary(data_phoreau_ini$IDP)


















