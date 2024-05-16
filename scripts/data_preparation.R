# Charger les packages nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

# Charger le dataset
nobel_data <- read_csv("/mnt/data/nobel_prize_data.csv")

# Inspection initiale des données
head(nobel_data)
str(nobel_data)
summary(nobel_data)

# Analyser la distribution des prix par année et par catégorie
ggplot(nobel_data, aes(x = year, fill = category)) +
    geom_histogram(binwidth = 1, position = "stack") +
    theme_minimal() +
    labs(title = "Distribution des Prix Nobel par Année et par Catégorie", x = "Année", y = "Nombre de Prix", fill = "Catégorie")

# Analyser la répartition des lauréats par sexe
ggplot(nobel_data, aes(x = sex, fill = sex)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Répartition des Lauréats par Sexe", x = "Sexe", y = "Nombre de Lauréats")

# Analyser l'âge moyen des lauréats par catégorie
nobel_data %>%
    mutate(age = year(birth_date)) %>%
    group_by(category) %>%
    summarise(age_moyen = mean(age, na.rm = TRUE)) %>%
    ggplot(aes(x = category, y = age_moyen, fill = category)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Âge Moyen des Lauréats par Catégorie", x = "Catégorie", y = "Âge Moyen")
