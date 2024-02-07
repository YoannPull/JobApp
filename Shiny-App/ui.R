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
  
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
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
    skin = "purple",
    
    dashboardHeader(title = "STAGE SVP!"),
    dashboardSidebar(
      
      sidebarMenu(
        
        # Autres éléments de l'UI
        tags$img(src = 'logo.png', style = 'display: block; margin-left: auto; margin-right: auto;', width = '186'),
        menuItem("Welcome Page", tabName = "welcome", icon = icon("home")),
        menuItem("Tableau des offres", tabName = "tableau", icon = icon("table")),
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Charger CV", tabName = "cv", icon = icon("upload"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "welcome",
                
                fluidRow(
                  column(12,
                         h3("Bienvenue dans le Guide d'Utilisation !"),
                         p("Ce guide vous aidera à comprendre comment utiliser l'application afin de pouvoir explorer les différentes offres d'emplois."),
                         h4("Tableau des offres"),
                         p("Cette page permet de filtrer et consulter les offres d'emploi disponibles.")
                  )
                )
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
                leafletOutput("mymap", width = "100%", height = "1000px") %>% withSpinner(color = "purple")
        ),
        tabItem(tabName = "cv",
                div(id = "filters-container",fluidRow(
                  column(6, fileInput("fileInput", "Charger son CV (format PDF uniquement)", accept = c(".pdf")))
                ),
                fluidRow(
                  column(6, actionButton("btnAnalyse", "Analyser les Compétences"))
                )),
                DT::dataTableOutput("tableCorrespondances")
        )
      )
    )
  )))