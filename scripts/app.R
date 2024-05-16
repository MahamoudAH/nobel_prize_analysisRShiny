# scripts/app.R

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(zoo)
library(RColorBrewer)

# Définir le répertoire de travail
setwd("C:/Users/Nassir/Documents/Nobel+Prize+Analysis/nobel_prize_analysisRShiny")

# Charger les données préparées
nobel_data <- read_csv("data/nobel_prize_data_prepared.csv")

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

# Créer une expression réactive pour filtrer les données des trois premières femmes
first_three_females_reactive <- reactive({
    nobel_data %>%
        filter(sex == "Female") %>%
        arrange(year) %>%
        head(3) %>%
        select(full_name, year, category, motivation, birth_country, organization_name)
})

# Filtrer les données pour identifier les gagnants répétitifs
repeated_winners <- nobel_data %>%
    group_by(full_name) %>%
    filter(n() > 1) %>%
    arrange(full_name, year) %>%
    select(full_name, year, category, motivation, birth_country, organization_name)

# Créer une expression réactive pour filtrer les données des gagnants répétitifs
repeated_winners_reactive <- reactive({
    nobel_data %>%
        group_by(full_name) %>%
        filter(n() > 1) %>%
        arrange(full_name, year) %>%
        select(full_name, year, category, motivation, birth_country, organization_name)
})

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

# Définition de l'UI
ui <- fluidPage(
    titlePanel("Analyse des Prix Nobel"),
    sidebarLayout(
        sidebarPanel(
            h3("Filtrer les données"),
            selectInput("category", "Catégorie:", choices = c("All", unique(nobel_data$category)), selected = "All"),
            sliderInput("year", "Année:", min = min(nobel_data$year), max = max(nobel_data$year), value = c(min(nobel_data$year), max(nobel_data$year))),
            selectInput("gender", "Genre:", choices = c("All", unique(nobel_data$sex)), selected = "All"),
            selectInput("country", "Pays:", choices = c("All", unique(nobel_data$birth_country_current)), selected = "All"),
            checkboxInput("show_data", "Show Data Table", value = FALSE)
        ),
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Graphiques",
                    plotlyOutput("nobel_plot"),
                    conditionalPanel(
                        condition = "input.show_data == true",
                        DTOutput("nobel_table"),
                        downloadButton("download_data", "Download Data")
                    )
                ),
                tabPanel("Résumé", verbatimTextOutput("summary")),
                tabPanel("Répartition par Sexe", plotlyOutput("sex_donut")),
                tabPanel("Premières Femmes Lauréates", DTOutput("first_three_females_table")),
                tabPanel("Gagnants Répétitifs", DTOutput("repeated_winners_table")),
                tabPanel("Prix par Catégorie", plotlyOutput("category_bar_chart")),
                tabPanel("Premier Prix d'Économie", DTOutput("first_economics_prize_table")),
                tabPanel("Gagnants par Sexe et Catégorie", plotlyOutput("gender_category_bar_chart")),
                tabPanel("Nombre de Prix au Fil du Temps", plotlyOutput("annual_prizes_plot")),
                tabPanel("Part des Prix Partagés", plotlyOutput("shared_prizes_plot")),
                tabPanel("Pays avec le Plus de Prix", plotlyOutput("top20_countries_plot")),
                tabPanel("Carte des Prix Nobel", plotlyOutput("world_map_plot")),
                tabPanel("Prix par Pays et Catégorie", plotlyOutput("country_category_bar_chart")),
                tabPanel("Nombre de Prix par Pays au Fil du Temps", plotlyOutput("cumulative_prizes_line_chart")),
                tabPanel("Meilleurs Organismes de Recherche", plotlyOutput("top20_organizations_plot"), 
                         verbatimTextOutput("chicago_harvard_affiliates")),
                tabPanel("Villes de Découvertes", plotlyOutput("top20_cities_plot"), 
                         verbatimTextOutput("top_discovery_city"),
                         verbatimTextOutput("top_european_discovery_city")),
                tabPanel("Villes de Naissance des Lauréats", plotlyOutput("top20_birth_cities_plot"),
                         verbatimTextOutput("new_york_percentage"),
                         verbatimTextOutput("london_paris_vienna_laureates"),
                         verbatimTextOutput("us_cities_in_top5")),
                tabPanel("Âge des Lauréats",
                         fluidRow(
                             column(12, DTOutput("age_winners_table")),
                             column(12, verbatimTextOutput("age_statistics")),
                             column(12, plotlyOutput("age_distribution_histogram")),
                             column(12, plotlyOutput("age_boxplot"))
                         )
                )
            )
        )
    )
)

# Définition du serveur
server <- function(input, output) {
    
    # Expression réactive pour filtrer les données
    filtered_data <- reactive({
        data <- nobel_data
        if (input$category != "All") {
            data <- data %>% filter(category == input$category)
        }
        if (input$gender != "All") {
            data <- data %>% filter(sex == input$gender)
        }
        if (input$country != "All") {
            data <- data %>% filter(birth_country == input$country)
        }
        data <- data %>% filter(year >= input$year[1] & year <= input$year[2])
        data
    })
    
    # Graphique interactif
    output$nobel_plot <- renderPlotly({
        plot_ly(data = filtered_data(), x = ~year, y = ~winning_age, type = 'scatter', mode = 'markers',
                text = ~paste("Nom:", full_name, "<br>Âge:", winning_age, "<br>Année:", year, "<br>Catégorie:", category))
    })
    
    # Résumé des données
    output$summary <- renderPrint({
        summary(filtered_data())
    })
    
    # Tableau des données
    output$nobel_table <- renderDT({
        datatable(filtered_data())
    })
    
    # Bouton de téléchargement
    output$download_data <- downloadHandler(
        filename = function() {
            paste("nobel_data_filtered", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write_csv(filtered_data(), file)
        }
    )
    
    # Graphique en anneau (donut chart) pour la répartition des prix par sexe
    output$sex_donut <- renderPlotly({
        plot_ly(
            data = sex_distribution,
            labels = ~sex,
            values = ~n,
            type = 'pie',
            hole = 0.4,
            textinfo = 'label+percent',
            insidetextorientation = 'radial'
        ) %>%
            layout(title = paste("Répartition des Prix Nobel par Sexe (Femmes:", round(female_percentage, 2), "%)"))
    })
    
    # Tableau des trois premières femmes lauréates du prix Nobel
    output$first_three_females_table <- renderDT({
        datatable(first_three_females_reactive())
    })
    
    # Tableau des gagnants répétitifs du prix Nobel
    output$repeated_winners_table <- renderDT({
        datatable(repeated_winners_reactive())
    })
    
    # Diagramme en barres pour le nombre de prix par catégorie
    output$category_bar_chart <- renderPlotly({
        plot_ly(
            data = category_counts,
            x = ~reorder(category, -count),
            y = ~count,
            type = 'bar',
            marker = list(color = ~count, colorscale = 'Aggrnyl')
        ) %>%
            layout(
                title = "Nombre de Prix Attribués par Catégorie",
                xaxis = list(title = "Catégorie de Prix Nobel"),
                yaxis = list(title = "Nombre de Prix")
            )
    })
    
    # Tableau du premier prix Nobel d'économie
    output$first_economics_prize_table <- renderDT({
        datatable(first_economics_prize)
    })
    
    # Diagramme en bâtons pour le nombre de gagnants masculins et féminins par catégorie
    output$gender_category_bar_chart <- renderPlotly({
        plot_ly(
            data = gender_category_counts,
            x = ~reorder(category, -count),
            y = ~count,
            color = ~sex,
            type = 'bar',
            barmode = 'stack',
            text = ~paste(sex, ":", count)
        ) %>%
            layout(
                title = "Répartition des Gagnants par Sexe et Catégorie",
                xaxis = list(title = "Catégorie de Prix Nobel"),
                yaxis = list(title = "Nombre de Prix"),
                hovermode = 'closest'
            )
    })
    
    # Diagramme de dispersion avec moyenne mobile pour le nombre de prix au fil du temps
    output$annual_prizes_plot <- renderPlotly({
        plot_ly(data = annual_prizes, x = ~year, y = ~count, type = 'scatter', mode = 'markers',
                name = 'Nombre de Prix', text = ~paste("Année:", year, "<br>Nombre de Prix:", count)) %>%
            add_trace(y = ~moving_avg, type = 'scatter', mode = 'lines', name = 'Moyenne Mobile sur 5 Ans',
                      text = ~paste("Année:", year, "<br>Moyenne Mobile:", round(moving_avg, 2))) %>%
            layout(
                title = "Nombre de Prix Attribués au Fil du Temps",
                xaxis = list(title = "Année", tickvals = seq(1900, 2020, by = 5)),
                yaxis = list(title = "Nombre de Prix"),
                hovermode = 'closest'
            )
    })
    
    # Diagramme de dispersion avec moyenne mobile et part des prix partagés au fil du temps
    output$shared_prizes_plot <- renderPlotly({
        plot_ly(data = annual_prizes, x = ~year, y = ~count, type = 'scatter', mode = 'markers',
                name = 'Nombre de Prix', text = ~paste("Année:", year, "<br>Nombre de Prix:", count)) %>%
            add_trace(y = ~moving_avg, type = 'scatter', mode = 'lines', name = 'Moyenne Mobile sur 5 Ans',
                      text = ~paste("Année:", year, "<br>Moyenne Mobile:", round(moving_avg, 2)), yaxis = "y") %>%
            add_trace(data = annual_share, y = ~moving_avg_share, type = 'scatter', mode = 'lines', name = 'Moyenne Mobile Part des Prix',
                      text = ~paste("Année:", year, "<br>Moyenne Mobile Part des Prix:", round(moving_avg_share, 2)), yaxis = "y2") %>%
            layout(
                title = "Nombre de Prix et Part des Prix Partagés au Fil du Temps",
                xaxis = list(title = "Année", tickvals = seq(1900, 2020, by = 5)),
                yaxis = list(title = "Nombre de Prix"),
                yaxis2 = list(title = "Moyenne Mobile Part des Prix", overlaying = "y", side = "right", autorange = "reversed"),
                hovermode = 'closest'
            )
    })
    
    # Diagramme à barres horizontales pour les pays avec le plus de prix
    output$top20_countries_plot <- renderPlotly({
        plot_ly(data = top20_countries, x = ~prizes, y = ~reorder(birth_country_current, prizes),
                type = 'bar', orientation = 'h',
                marker = list(
                    color = ~prizes, 
                    colorscale = 'Viridis',
                    colorbar = list(title = 'Nombre de Prix')
                )) %>%
            layout(
                title = "Pays avec le Plus de Prix Nobel",
                xaxis = list(title = "Nombre de Prix"),
                yaxis = list(title = "Pays"),
                hovermode = 'closest'
            )
    })
    
    # Carte choroplèthe pour montrer le nombre de prix gagnés par pays
    output$world_map_plot <- renderPlotly({
        plot_ly(data = country_prizes, type = 'choropleth', locations = ~ISO, z = ~prizes,
                colorscale = 'YlOrRd', reversescale = FALSE, marker = list(line = list(width = 0.5)),
                colorbar = list(title = 'Nombre de Prix')) %>%
            layout(
                title = "Carte des Prix Nobel par Pays",
                geo = list(
                    showframe = FALSE,
                    showcoastlines = TRUE,
                    projection = list(type = 'equirectangular')
                )
            )
    })
    
    # Diagramme en bâtons empilé pour le nombre de prix par pays et par catégorie
    output$country_category_bar_chart <- renderPlotly({
        plot_ly(
            data = country_category_prizes,
            x = ~prizes,
            y = ~reorder(birth_country_current, prizes),
            color = ~category,
            type = 'bar',
            orientation = 'h',
            barmode = 'stack'
        ) %>%
            layout(
                title = "Nombre de Prix par Pays et par Catégorie",
                xaxis = list(title = "Nombre de Prix"),
                yaxis = list(title = "Pays"),
                barmode = 'stack',
                hovermode = 'closest',
                margin = list(l = 150) # Ajuster les marges pour une meilleure lisibilité
            )
    })
    
    # Graphique en ligne pour le nombre cumulé de prix par pays au fil du temps
    output$cumulative_prizes_line_chart <- renderPlotly({
        # Générer une palette de couleurs distinctes
        colors <- colorRampPalette(brewer.pal(n = 12, name = "Set3"))(length(unique(cumulative_prizes$birth_country_current)))
        
        plot_ly(data = cumulative_prizes, x = ~year, y = ~cumulative_prizes, color = ~birth_country_current, colors = colors, type = 'scatter', mode = 'lines') %>%
            layout(
                title = "Nombre Cumulé de Prix par Pays au Fil du Temps",
                xaxis = list(title = "Année"),
                yaxis = list(title = "Nombre Cumulé de Prix"),
                legend = list(title = list(text = "Pays"), orientation = "h", x = 0.5, xanchor = "center"),
                hovermode = 'closest'
            )
    })
    
    # Diagramme à barres pour les meilleures organisations de recherche
    output$top20_organizations_plot <- renderPlotly({
        plot_ly(data = top20_organizations, x = ~count, y = ~reorder(organization_name, count),
                type = 'bar', orientation = 'h',
                marker = list(color = 'steelblue')) %>%
            layout(
                title = "Top 20 des Organisations Affiliées aux Lauréats du Prix Nobel",
                xaxis = list(title = "Nombre de Lauréats"),
                yaxis = list(title = "Organisation"),
                hovermode = 'closest'
            )
    })
    
    # Afficher le nombre de lauréats affiliés à l'Université de Chicago et à l'Université de Harvard
    output$chicago_harvard_affiliates <- renderPrint({
        cat("Nombre de lauréats affiliés à l'Université de Chicago:", chicago_affiliates, "\n")
        cat("Nombre de lauréats affiliés à l'Université de Harvard:", harvard_affiliates)
    })
    
    # Diagramme à barres pour les meilleures villes de découvertes
    output$top20_cities_plot <- renderPlotly({
        plot_ly(data = top20_cities, x = ~count, y = ~reorder(organization_city, count),
                type = 'bar', orientation = 'h',
                marker = list(color = 'coral')) %>%
            layout(
                title = "Top 20 des Villes d'Organisation des Institutions de Recherche Associées aux Lauréats du Prix Nobel",
                xaxis = list(title = "Nombre de Lauréats"),
                yaxis = list(title = "Ville"),
                hovermode = 'closest'
            )
    })
    
    # Afficher la première ville avec le plus grand nombre de découvertes
    output$top_discovery_city <- renderPrint({
        cat("Première ville avec le plus grand nombre de découvertes:", top_city)
    })
    
    # Afficher la ville d'Europe avec le plus grand nombre de découvertes
    output$top_european_discovery_city <- renderPrint({
        cat("Ville d'Europe avec le plus grand nombre de découvertes:", top_european_city)
    })
    
    # Diagramme à barres pour les villes de naissance des lauréats
    output$top20_birth_cities_plot <- renderPlotly({
        plot_ly(data = top20_birth_cities, x = ~count, y = ~reorder(birth_city, count),
                type = 'bar', orientation = 'h',
                marker = list(color = ~count, colorscale = 'Plasma')) %>%
            layout(
                title = "Top 20 des Villes de Naissance des Lauréats du Prix Nobel",
                xaxis = list(title = "Nombre de Lauréats"),
                yaxis = list(title = "Ville"),
                hovermode = 'closest'
            )
    })
    
    # Afficher le pourcentage des prix décernés aux États-Unis par des lauréats nés à New York
    output$new_york_percentage <- renderPrint({
        cat("Pourcentage des prix décernés aux États-Unis par des lauréats nés à New York:", round(new_york_percentage, 2), "%")
    })
    
    # Afficher le nombre de lauréats nés à Londres, Paris et Vienne
    output$london_paris_vienna_laureates <- renderPrint({
        cat("Nombre de lauréats nés à Londres:", london_laureates, "\n")
        cat("Nombre de lauréats nés à Paris:", paris_laureates, "\n")
        cat("Nombre de lauréats nés à Vienne:", vienna_laureates)
    })
    
    # Afficher le nombre de villes américaines parmi les cinq premières
    output$us_cities_in_top5 <- renderPrint({
        cat("Nombre de villes américaines parmi les cinq premières:", top5_us_cities)
    })
    
    # Tableau combiné des plus jeunes et plus âgés gagnants
    output$age_winners_table <- renderDT({
        datatable(combined_winners)
    })
    
    # Statistiques descriptives pour l'âge au moment de l'attribution du prix
    output$age_statistics <- renderPrint({
        age_statistics
    })
    
    # Histogramme de la distribution des âges
    output$age_distribution_histogram <- renderPlotly({
        plot_ly(
            data = nobel_data,
            x = ~winning_age,
            type = 'histogram',
            nbinsx = 30,
            marker = list(color = 'blue')
        ) %>%
            layout(
                title = "Distribution de l'Âge au Moment de l'Attribution du Prix",
                xaxis = list(title = "Âge"),
                yaxis = list(title = "Nombre de Lauréats")
            )
    })
    
    # Boîte à moustache pour la variation de l'âge par catégorie
    output$age_boxplot <- renderPlotly({
        plot_ly(
            data = nobel_data,
            x = ~category,
            y = ~winning_age,
            type = 'box',
            color = ~category,
            colors = 'Set3'
        ) %>%
            layout(
                title = "Variation de l'Âge par Catégorie de Prix",
                xaxis = list(title = "Catégorie"),
                yaxis = list(title = "Âge")
            )
    })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
