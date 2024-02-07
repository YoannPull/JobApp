library(shiny)
library(data.table)
library(tmaptools)
library(dplyr)
library(httr)

job_data <- read.csv2("../data/data2.csv")

setDT(job_data)


function(input, output, session) {
  
  ################ Code de la page Welcome Page ################
  
  observeEvent(input$start_guide, {
    showModal(modalDialog(
      title = "🌟 Bienvenue dans l'aventure DataJobQuest ! 🌟",
      tags$p("Prêt(e) pour une quête épique au cœur du monde des données ? DataJobQuest est là pour t'accompagner dans ta recherche d'emploi. Avec nous, plonge dans un univers où les offres d'emploi les plus fraîches t'attendent, où ton CV devient une clé magique pour des correspondances parfaites, et où les opportunités se révèlent sur une carte pleine de promesses."),
      footer = tagList(
        modalButton("Fermer"),
        actionButton("next_intro", "À l'aventure !", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$next_intro, {
    removeModal()
    showModal(modalDialog(
      title = "📜 Étape 1 : Le Grand Tableau des Offres",
      tags$p("Le Grand Tableau des Offres est ton premier arrêt : un parchemin magique où tu peux filtrer les annonces par secteur, poste, lieu, et bien plus. C'est ici que tu affines ta quête pour dénicher les trésors cachés qui matchent avec tes compétences et aspirations."),
      footer = tagList(
        actionButton("back_to_welcome", "Précedent", class = "btn-default"),
        actionButton("next_to_map", "En avant", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$back_to_welcome, {
    removeModal()
  })
  
  observeEvent(input$next_to_map, {
    removeModal()
    showModal(modalDialog(
      title = "🗺 Étape 2 : La Carte",
      tags$p("La Carte se dévoile à toi, offrant une vue d'ensemble des terres d'opportunités. Chaque marqueur est une aventure potentielle dans la région de ton choix. Utilise-la pour naviguer à travers le monde des emplois et découvrir où ta prochaine quête t'attend."),
      footer = tagList(
        actionButton("back_to_joblistings", "Retour en arrière", class = "btn-default"),
        actionButton("next_to_cv", "Poursuivre", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$back_to_joblistings, {
    removeModal()
  })
  
  observeEvent(input$next_to_cv, {
    removeModal()
    showModal(modalDialog(
      title = "🔍 Étape 3: L'Analyse de ton Grimoire (CV) !",
      tags$p("Dans l'onglet 'Charger CV', ton CV n'est plus qu'un simple document : c'est un grimoire de compétences. Télécharge-le au format PDF et laisse la magie opérer. Notre algorithme scrutera chaque sortilège (compétence) pour te révéler les missions les plus alignées avec tes pouvoirs."),
      footer = tagList(
        actionButton("back_to_map", "Précédent", class = "btn-default"),
        actionButton("end_guide", "Terminer la quête", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$back_to_map, {
    removeModal()
  })
  
  observeEvent(input$end_guide, {
    removeModal()
  })


  
  ################ Code de la page "Tableau des offres" ################
  
  filteredData <- reactive({
    
    dataFiltered <- job_data
    filters <- list(
      
      # Création des listes déroulantes pour filtrer
      SecteurActivité = input$secteurInput,
      IntituléPoste = input$posteInput,
      LieuExercice = input$lieuInput,
      FourchetteSalaire = input$salaireInput,
      TypeEmploi = input$typeEmploiInput
    )
    for (filterName in names(filters)) {
      if (filters[[filterName]] != "Tous") {
        dataFiltered <- dataFiltered[get(filterName) == filters[[filterName]], ]
      }
    }
    
    # Filtre par saisie de compétences
    if (input$competenceInput != "") {
      competencesSaisies <- unlist(strsplit(tolower(input$competenceInput), split = "\\s*,\\s*"))
      print(paste("Compétences saisies:", competencesSaisies))
      
      dataFiltered <- dataFiltered[sapply(dataFiltered$CompétencesDemandées, function(x) {
        print(paste("Compétences pour l'offre:", x))
        
        # Vérifier si au moins un mot-clé est présent dans les compétences demandées
        matchFound <- any(sapply(competencesSaisies, function(motCle) {
          matchResult <- grepl(motCle, tolower(x))
          print(paste("Vérification de", motCle, "dans", x, ":", matchResult))
          matchResult
        }))
        
        print(paste("Correspondance trouvée:", matchFound))
        matchFound
      }), ]
    }
    dataFiltered
  })
  
  
  
  # Affiche la bdd avec seulement quelques colonnes importante et filtré
  output$tableAnnonces <- DT::renderDataTable({
    filteredData()[, .(RechercheEffectuée, IntituléPoste, FourchetteSalaire,
                       Entreprise, LieuExercice, TypeEmploi, DuréeEmploi,
                       SiteSourceAnnonce, LienAnnonce)]
  }, options = list(
    pageLength = 5,  
    autoWidth = TRUE, 
    dom = 'ftpi',  
    language = list(
      search = '<i class="fa fa-search" aria-hidden="true"></i>',
      searchPlaceholder = 'Cherchez un job de la data, une entreprise, une ville...'
    )
  ),
  selection = "single",
  callback = JS(
    "table.on('init.dt', function() {
        $('.dataTables_filter input').css('width', '500px'); // Ajustez la largeur comme vous le souhaitez
      });"
  )
  )
  
  
  
  
  # Permet de cliquer sur une ligne pour afficher plus de détail
  observeEvent(input$tableAnnonces_rows_selected, {
    selectedRow <- input$tableAnnonces_rows_selected
    
    if(length(selectedRow) > 0) {
      annonceDetails <- filteredData()[selectedRow, ]
      showModal(modalDialog(
        title = "Détails de l'Annonce",
        h3(annonceDetails$IntituléPoste),
        
        tags$style(HTML("
    .bold-underline {
      font-weight: bold;
      text-decoration: underline;
    }
  ")),
        p(tags$p(class = "bold-underline", "Description du Poste : "), annonceDetails$DescriptionPoste),
        p(tags$p(class = "bold-underline", "Fourchette de Salaire :"), annonceDetails$FourchetteSalaire),
        p(tags$p(class = "bold-underline", "Type d'emploi :"), annonceDetails$TypeEmploi),
        p(tags$p(class = "bold-underline", "Durée de l'emploi : "), annonceDetails$DuréeEmploi),
        p(tags$p(class = "bold-underline", "Site de l'annonce :"), annonceDetails$SiteSourceAnnonce),
        p(tags$p(class = "bold-underline", "Compétences Demandées :"), annonceDetails$CompétencesDemandées),
        tags$br(), 
        p(a(href = as.character(annonceDetails$LienAnnonce), target = "_blank",
            icon("external-link-alt"),
            " Voir l'annonce",
            style = "text-decoration: none;")),
        
        footer = tagList(modalButton("Fermer")),
        # JS pour fermer la page si on clique en dehors de la fenêtre
        easyClose = TRUE,
        tags$script(HTML("
        $(document).on('click', '.modal-backdrop', function(){
          $('.modal').modal('hide');
        });
      "))
      ))
    }
  })
  
  ################ Code de la page "Map" ################
  
  counts_per_ville <- table(job_data$LieuExercice) # compte le nombre d'offre par ville
  
  output$mymap <- renderLeaflet({
        leaflet(job_data) %>%
          setView(lng = 2.2137, lat = 46.6031, zoom = 5) %>%  # Centre sur la france
          
          addProviderTiles(providers$Stadia.AlidadeSmooth,  
                           options = providerTileOptions(noWrap = TRUE)) %>%
          
          addCircleMarkers(data = job_data,
                           ~lon, ~lat,  # Coordonnées des offres
                           radius = 8,  
                           fillOpacity = 0.8,  
                           color = "darkgreen",  
                           fillColor = "darkgreen",  
                           popup = ~paste0("<strong>", LieuExercice, "</strong>: ", counts_per_ville[LieuExercice], " offre(s)", "<br>",
                                           "<a href=\"#\" onclick=\"Shiny.setInputValue('selectedCity', '", LieuExercice,
                                           "', {priority: 'event'});\">Voir les offres</a>"),
                           group = "markers")  
      })
    
  
  observeEvent(input$selectedCity, {
    updateTabItems(session,inputId = "tabs", selected = "tableau")
    updateTextInput(session, "lieuInput", value = input$selectedCity)
  })
  
  
  ################ Code de la page "Chargez CV" ################
  
  
  verifier_competences <- function(competences_offre, comp_cv) {
    competences_offre <- tolower(competences_offre)
    competences_offre_liste <- unlist(str_split(competences_offre, ",\\s*"))
    nb_correspondances <- sum(competences_offre_liste %in% comp_cv)
    proportion_correspondances <- round(100 * nb_correspondances / length(competences_offre_liste),2)
    return(proportion_correspondances) # Retourne la proportion de compétences qui matchent
  }
  
  prescence_competence <- function(competences_offre, comp_cv){
    competences_offre <- tolower(competences_offre)
    competences_offre_liste <- unlist(str_split(competences_offre, ",\\s*"))
    comp_cv <- tolower(comp_cv) 
    
    competences_presentes <- competences_offre_liste[competences_offre_liste %in% comp_cv]
    competences_absentes <- competences_offre_liste[!(competences_offre_liste %in% comp_cv)]
    cat("comp_cv",comp_cv,"\n")
    cat("Pres",competences_presentes,"\n")
    cat("ABS",competences_absentes)
    return(list(competences_presentes = competences_presentes,
                competences_absentes  = competences_absentes))
  }# Retourne les listes des competenes présentes et absentes d'une offre
  
  competences_cv <- reactiveVal(NULL)
  offres_correspondantes <- reactiveVal()
  
  observeEvent(input$btnAnalyse, {
    req(input$fileInput)
    
    # Lecture du fichier PDF
    chemin_pdf <- input$fileInput$datapath
    texte_cv <- tolower(pdftools::pdf_text(chemin_pdf))
    
    # Extraction des compétences du CV
    competences_cv(unique(str_extract_all(texte_cv, "\\b([A-Za-z]+)\\b")[[1]]))
    
    # Calcul de la proportion de correspondance pour chaque offre
    job_data$ProportionCompetencesCorrespondantes <- sapply(job_data$CompétencesDemandées,
                                                            function(x) verifier_competences(x, competences_cv()))
    
    job_data_filtré <- job_data[job_data$ProportionCompetencesCorrespondantes > 0, ]
    
    # Trier les offres par la proportion de compétences correspondantes en ordre décroissant
    offres_triees <- job_data_filtré[order(-job_data_filtré$ProportionCompetencesCorrespondantes), ]
    
    offres_correspondantes(offres_triees)
    
    # Affichage des résultats avec la proportion de compétences qui matchent
    output$tableCorrespondances <- DT::renderDataTable({
      offres_correspondantes()[, .(IntituléPoste,
                                   Entreprise,
                                   LieuExercice,
                                   CompétencesDemandées,
                                   "Proportion Competences Correspondantes" = paste0(ProportionCompetencesCorrespondantes,"%"))]
    }, options = list(lengthChange = FALSE, pageLength = 10, autoWidth = TRUE, dom = 'tpi'),
    selection = "single")
  })
  

  
  # Permet de cliquer sur une ligne pour afficher plus de détail
  observeEvent(input$tableCorrespondances_rows_selected, {
    selectedRow <- input$tableCorrespondances_rows_selected
    print(selectedRow)
    
    if(length(selectedRow) == 1) {
      offreDetails <- offres_correspondantes()[selectedRow, ]
      competences_resultat <- prescence_competence(offreDetails$CompétencesDemandées, competences_cv())
      
      competences_presentes_html <- paste("<span style='color:green;'>", competences_resultat$competences_presentes, "</span>", collapse = ", ")
      competences_absentes_html <- paste("<span style='color:red;'>", competences_resultat$competences_absentes, "</span>", collapse = ", ")
      
      showModal(modalDialog(
        title = "Détails de l'Offre Correspondante",
        h3(as.character(offreDetails$IntituléPoste)),
        
        tags$style(HTML("
      .bold-underline {
        font-weight: bold;
        text-decoration: underline;
      }
      ")),
        p(tags$p(class = "bold-underline", "Description du Poste : "), as.character(offreDetails$DescriptionPoste)),
        p(tags$p(class = "bold-underline", "Fourchette de Salaire :"), as.character(offreDetails$FourchetteSalaire)),
        p(tags$p(class = "bold-underline", "Type d'emploi :"), as.character(offreDetails$TypeEmploi)),
        p(tags$p(class = "bold-underline", "Durée de l'emploi : "), as.character(offreDetails$DuréeEmploi)),
        p(tags$p(class = "bold-underline", "Site de l'annonce :"), as.character(offreDetails$SiteSourceAnnonce)),
        p(tags$p(class = "bold-underline", "Compétences Demandées :")),
        p(HTML(paste("Présentes: ", toupper(competences_presentes_html), "<br>Absentes: ", toupper(competences_absentes_html)))),
        tags$br(), 
        p(a(href = as.character(offreDetails$LienAnnonce), target = "_blank",
            icon("external-link-alt"),
            " Voir l'annonce",
            style = "text-decoration: none;")),
        footer = tagList(modalButton("Fermer")),
        
        # JS pour fermer la page si on clique en dehors de la fenêtre
        easyClose = TRUE,
        tags$script(HTML("
        $(document).on('click', '.modal-backdrop', function(){
          $('.modal').modal('hide');
        });
      "))
      ))
    }
  }
  )
  ################ Code de la page "ANALYSE" ################
  
  
  output$plotCompetences <- renderPlot({
    filtered_data <- data[RechercheEffectuée %like% input$filterRecherche | input$filterRecherche == "", ]
    data_compétences <- filtered_data[, .(Compétences = unlist(strsplit(Compétences, ",\\s*"))), by = .(ID)]
    data_compétences <- data_compétences[, .(Frequence = .N), by = .(Compétences)]
    setorder(data_compétences, -Frequence)
    top_compétences <- head(data_compétences, 5)
    
    ggplot(top_compétences, aes(x = "", y = Frequence, fill = Compétences)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Top 5 Compétences Demandées", fill = "Compétence") +
      theme_void()
  })
  
  output$plotZonesGeo <- renderPlot({
    filtered_data <- data[(RechercheEffectuée %like% input$filterRecherche | input$filterRecherche == ""), ]
    data_zones_geo <- filtered_data[!is.na(nom_departement), .(Offres = .N), by = .(nom_departement)]
    setorder(data_zones_geo, -Offres)
    top_zones_geo <- head(data_zones_geo, 10)
    
    ggplot(top_zones_geo, aes(x = reorder(nom_departement, Offres), y = Offres)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(title = "Top 10 des Départements par Nombre d'Offres", x = "nom_departement", y = "Nombre d'offres") +
      theme_minimal()
  })
  
  
  
}