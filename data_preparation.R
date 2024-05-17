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

# Calculer la répartition des prix par sexe
sex_distribution <- nobel_data %>%
    filter(!is.na(sex)) %>%
    count(sex) %>%
    mutate(percentage = n / sum(n) * 100)

# Extraire le pourcentage de prix attribués aux femmes
female_percentage <- sex_distribution %>%
    filter(sex == "Female") %>%
    pull(percentage)

# Filtrer les données pour obtenir les trois premières femmes lauréates du prix Nobel
first_three_females <- nobel_data %>%
    filter(sex == "Female") %>%
    arrange(year) %>%
    head(3) %>%
    select(full_name, year, category, motivation, birth_country, organization_name)

# Filtrer les données pour identifier les gagnants répétitifs
repeated_winners <- nobel_data %>%
    group_by(full_name) %>%
    filter(n() > 1) %>%
    arrange(full_name, year) %>%
    select(full_name, year, category, motivation, birth_country, organization_name)

# Calculer le nombre de prix par catégorie et trier en ordre décroissant
category_counts <- nobel_data %>%
    group_by(category) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

# Identifier la catégorie avec le plus et le moins de prix décernés
most_prizes_category <- category_counts %>%
    filter(count == max(count)) %>%
    pull(category)

least_prizes_category <- category_counts %>%
    filter(count == min(count)) %>%
    pull(category)

# Filtrer les données pour trouver le premier prix Nobel d'économie
first_economics_prize <- nobel_data %>%
    filter(category == "Economics") %>%
    arrange(year) %>%
    head(3) %>%
    select(year, full_name, motivation, birth_country, organization_name)

# Calculer le nombre de gagnants masculins et féminins par catégorie
gender_category_counts <- nobel_data %>%
    filter(!is.na(sex)) %>%
    group_by(category, sex) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

# Calculer le nombre de prix décernés chaque année
annual_prizes <- nobel_data %>%
    group_by(year) %>%
    summarise(count = n())

# Calculer une moyenne mobile sur 5 ans du nombre de prix
annual_prizes <- annual_prizes %>%
    mutate(moving_avg = zoo::rollmean(count, k = 5, fill = NA, align = 'right'))

# Calculer la part de prix moyenne des gagnants sur une base annuelle
annual_share <- nobel_data %>%
    group_by(year) %>%
    summarise(average_share = mean(share_pct, na.rm = TRUE))

# Calculer une moyenne mobile sur 5 ans de la part en pourcentage
annual_share <- annual_share %>%
    mutate(moving_avg_share = zoo::rollmean(average_share, k = 5, fill = NA, align = 'right'))

# Calculer le nombre total de prix remportés par chaque pays et sélectionner les 20 premiers
top20_countries <- nobel_data %>%
    filter(!is.na(birth_country_current)) %>%
    group_by(birth_country_current) %>%
    summarise(prizes = n()) %>%
    arrange(desc(prizes)) %>%
    head(20)

# Calculer le nombre total de prix remportés par chaque pays et associer les codes ISO à 3 lettres
country_prizes <- nobel_data %>%
    filter(!is.na(birth_country_current)) %>%
    group_by(birth_country_current, ISO) %>%
    summarise(prizes = n()) %>%
    arrange(desc(prizes))

# Calculer le nombre de prix par pays et par catégorie
country_category_prizes <- nobel_data %>%
    filter(!is.na(birth_country_current) & !is.na(category)) %>%
    group_by(birth_country_current, category) %>%
    summarise(prizes = n()) %>%
    ungroup() %>%
    arrange(prizes)

# Calculer le nombre cumulé de prix remportés par chaque pays chaque année
cumulative_prizes <- nobel_data %>%
    filter(!is.na(birth_country_current) & !is.na(year)) %>%
    group_by(birth_country_current, year) %>%
    summarise(prizes = n()) %>%
    arrange(year) %>%
    group_by(birth_country_current) %>%
    mutate(cumulative_prizes = cumsum(prizes)) %>%
    ungroup()

# Calculer le nombre de lauréats du prix Nobel affiliés à chaque organisation
organization_counts <- nobel_data %>%
    filter(!is.na(organization_name)) %>%
    group_by(organization_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

# Extraire les 20 organisations ayant le plus grand nombre de lauréats
top20_organizations <- organization_counts %>%
    head(20)

# Nombre de lauréats affiliés à l'Université de Chicago
chicago_affiliates <- organization_counts %>%
    filter(organization_name == "University of Chicago") %>%
    pull(count)

# Nombre de lauréats affiliés à l'Université de Harvard
harvard_affiliates <- organization_counts %>%
    filter(organization_name == "Harvard University") %>%
    pull(count)

# Calculer le nombre de lauréats du prix Nobel affiliés à chaque ville d'organisation
city_counts <- nobel_data %>%
    filter(!is.na(organization_city)) %>%
    group_by(organization_city) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

# Extraire les 20 villes ayant le plus grand nombre de lauréats
top20_cities <- city_counts %>%
    head(20)

# Première ville avec le plus grand nombre de découvertes
top_city <- city_counts %>%
    slice(1) %>%
    pull(organization_city)

# Ville d'Europe avec le plus grand nombre de découvertes (Exclure Cambridge)
european_cities <- nobel_data %>%
    filter(!is.na(organization_city) & 
               birth_country_current %in% c("France", "Germany", "United Kingdom", "Italy", "Spain", "Netherlands", "Belgium", "Sweden", "Switzerland", "Poland") & 
               organization_city != "Cambridge") %>%
    group_by(organization_city) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

top_european_city <- european_cities %>%
    slice(1) %>%
    pull(organization_city)

# Calculer le nombre de lauréats du prix Nobel nés dans chaque ville
birth_city_counts <- nobel_data %>%
    filter(!is.na(birth_city)) %>%
    group_by(birth_city) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

# Extraire les 20 villes ayant le plus grand nombre de lauréats
top20_birth_cities <- birth_city_counts %>%
    head(20)

# Calculer le nombre de lauréats nés à New York
new_york_laureates <- nobel_data %>%
    filter(birth_city == "New York, NY") %>%
    nrow()

# Calculer le nombre total de lauréats aux États-Unis
us_laureates <- nobel_data %>%
    filter(birth_country_current == "United States of America") %>%
    nrow()

# Calculer le pourcentage
new_york_percentage <- (new_york_laureates / us_laureates) * 100

# Nombre de lauréats nés à Londres, Paris et Vienne
london_laureates <- nobel_data %>%
    filter(birth_city == "London") %>%
    nrow()

paris_laureates <- nobel_data %>%
    filter(birth_city == "Paris") %>%
    nrow()

vienna_laureates <- nobel_data %>%
    filter(birth_city == "Vienna") %>%
    nrow()

# Déterminer combien des cinq premières villes se trouvent aux États-Unis
top5_us_cities <- top20_birth_cities %>%
    head(5) %>%
    inner_join(nobel_data %>% filter(birth_country_current == "United States of America"), by = "birth_city") %>%
    n_distinct("birth_city")

# Calculer les statistiques descriptives pour l'âge au moment de l'attribution du prix
age_statistics <- nobel_data %>%
    summarise(
        mean_age = mean(winning_age, na.rm = TRUE),
        median_age = median(winning_age, na.rm = TRUE),
        min_age = min(winning_age, na.rm = TRUE),
        max_age = max(winning_age, na.rm = TRUE),
        sd_age = sd(winning_age, na.rm = TRUE)
    )

# Extraire le plus jeune et le plus âgé gagnant
youngest_winner <- nobel_data %>%
    filter(winning_age == min(winning_age, na.rm = TRUE))

oldest_winner <- nobel_data %>%
    filter(winning_age == max(winning_age, na.rm = TRUE))

# Combiner les deux dans un seul tableau
combined_winners <- bind_rows(youngest_winner, oldest_winner)