# scripts/data_preparation.R

library(tidyverse)
library(lubridate)

# Charger les données
nobel_data <- read_csv("data/nobel_prize_data.csv")

# Inspection des données
glimpse(nobel_data)

# Convertir les dates de naissance en format Date
nobel_data <- nobel_data %>%
    mutate(birth_date = ymd(birth_date))

# Créer une colonne pour le pourcentage de partage du prix
nobel_data <- nobel_data %>%
    separate(prize_share, into = c("num", "den"), sep = "/", convert = TRUE) %>%
    mutate(share_pct = num / den) %>%
    select(-num, -den)

# Ajouter une colonne pour l'âge des lauréats lors de la réception du prix
nobel_data <- nobel_data %>%
    mutate(birth_year = year(birth_date),
           winning_age = year - birth_year)

# Sauvegarder les données préparées
write_csv(nobel_data, "data/nobel_prize_data_prepared.csv")