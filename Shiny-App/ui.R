#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(leaflet)
library(pdftools)
library(tesseract)
library(stringr)
library(shinydashboard)
library(shinycssloaders)

job_data <- read.csv2("../data/data2.csv")
setDT(job_data)


shinyUI(fluidPage(


  tags$head(
    tags$style(HTML("
      #filters-container {
        border: 2px solid #FFFFFF; /* Bordure blanche pour le carré des filtres */
        padding: 10px; /* Espacement interne */
        border-radius: 10px; /* Bords arrondis */
        background-color: #F8F9F9; /* Couleur de fond légère pour les filtres */
      }
      .shiny-input-container {
        margin-bottom: 10px; /* Espacement entre les filtres */
      }
      label { color: #4E5D6C; } /* Couleur des labels des filtres pour contraste */
    "))
  ),
  
  dashboardPage(
    skin = "green",
    
    dashboardHeader(title = "DataJobQuest !"),
    dashboardSidebar(
      
      sidebarMenu(id = "tabs",
        
        # Autres éléments de l'UI
        tags$img(src = 'logo_djq.png', style = 'display: block; margin-left: auto; margin-right: auto;', width = '186'),
        menuItem("Welcome Page", tabName = "welcome", icon = icon("home")),
        menuItem("Le Grand Tableau des Offres", tabName = "tableau", icon = icon("table")),
        menuItem("La Carte ", tabName = "map", icon = icon("map")),
        menuItem("Ton Grimoire (CV)", tabName = "cv", icon = icon("upload")),
        menuItem("Analyses", tabName = "analyses", icon = icon("chart-bar"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
                
                
          fluidRow(
            column(12,
                   h3("🗺 Guide de l'aventure !"),
                   p("Bienvenue, brave explorateur du royaume de DataJobQuest ! Tu te tiens à la frontière d'un monde rempli d'opportunités cachées,
                     prêt à dévoiler les mystères de la quête professionnelle. Ce guide est ta boussole, te menant à travers forêts denses
                     de possibilités et montagnes escarpées de défis, vers les trésors d'emploi que tu cherches à conquérir."),
                   p("Chaque page de ce guide est une carte, tracée par les sages de DataJobQuest, pour t'aider à naviguer dans les eaux
                     tumultueuses du marché de l'emploi. Utilise-le pour apprendre à maîtriser les outils à ta disposition, découvrir des 
                     astuces secrètes et éviter les pièges cachés dans les ombres. Avec ce guide en main, aucune offre d'emploi ne restera 
                     hors de portée, et chaque compétence que tu possèdes brillera comme un phare dans la nuit."),
                   p("Que ta quête commence ici, à l'aube de ton aventure, où chaque clic t'emmène plus près de ton but ultime.
                     Laisse le 🗺 Guide de l'aventure t'ouvrir les portes d'un avenir prometteur, pavé d'opportunités d'emploi en or. 
                     En avant, vers la gloire !")
                   
          )),
          tabName = "welcome",
          actionButton("start_guide", "Débuter l'aventure", class = "btn-success"),
        ),
        tabItem(tabName = "tableau",
                div(id = "filters-container",
                    fluidRow(class = "filter-row",
                  column(2, selectInput("secteurInput", "Secteur:", 
                                        choices = c("Tous", unique(job_data$SecteurActivité)))),
                  column(2, selectInput("posteInput", "Intitulé de Poste:", 
                                        choices = c("Tous", unique(job_data$IntituléPoste)))),
                  column(2, selectInput("lieuInput", "Lieu d'Exercice:", 
                                        choices = c("Tous", unique(job_data$LieuExercice)))),
                  column(2, selectInput("salaireInput", "Fourchette Salariale:", 
                                        choices = c("Tous", unique(job_data$FourchetteSalaire)))),
                  column(2, selectInput("typeEmploiInput", "Type d'Emploi:", 
                                        choices = c("Tous", unique(job_data$TypeEmploi)))),
                  column(2, textInput("competenceInput", "Compétences:", placeholder = "Tapez des compétences ici"))
                )),
                mainPanel(
                  DT::dataTableOutput("tableAnnonces")
                )
        ),
        tabItem(tabName = "map",
                leafletOutput("mymap", width = "100%", height = "1000px") %>% withSpinner(color = "green")
        ),
        tabItem(tabName = "cv",
                div(id = "filters-container",fluidRow(
                  column(6, fileInput("fileInput", "Charger son CV (format PDF uniquement)", accept = c(".pdf")))
                ),
                fluidRow(
                  column(6, actionButton("btnAnalyse", "Analyser les Compétences"))
                )),
                DT::dataTableOutput("tableCorrespondances")
        ),
        tabItem(tabName = "analyses",
                fluidRow(
                  box(selectInput("filterRecherche", "Filtrer par Recherche Effectuée:",
                                  choices = c("Toutes" = "", unique(data$RechercheEffectuée)))),
                  box(selectInput("filterZone", "Filtrer par Zone:",
                                  choices = c("Villes" = unique(data$LieuExercice), "Départements" = unique(data$nom_departement)),
                                  multiple = TRUE, selected = ""),
                      title = "Filtres", width = 12, status = "primary", solidHeader = TRUE),
                  box(selectInput("filterTypeEmploi", "Filtrer par Type d'Emploi:",
                                  choices = c("Tous" = "", unique(data$TypeEmploi))),
                      title = "Filtres", width = 12),
                  box(plotOutput("plotCompetences"), title = "Top 5 Compétences Demandées", width = 6),
                  box(plotOutput("plotZonesGeo"), title = "Top 10 des Zones par Nombre d'Offres", width = 6)
                )
        )
      )
    )
  )))