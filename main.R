library(data.table)
library(httr)
library(jsonlite) # Assurez-vous d'avoir jsonlite pour la fonction fromJSON

data <- fread("data/full_data2.csv", header = TRUE)
data$Département <- as.character(data$Département)

#https://www.data.gouv.fr/fr/datasets/departements-de-france/
data_dep <- fread("data/departements-france.csv")

data <- merge(data, data_dep, by.x = "Département" ,by.y = "code_departement", 
              all.x = TRUE,allow.cartesian=TRUE)


get_lat_long <- function(city_name) {
  base_url <- "https://nominatim.openstreetmap.org/search"
  params <- list(
    format = "json",
    q = city_name
  )
  
  response <- GET(url = base_url, query = params)
  data <- content(response, "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(data)
  
  if (length(data) > 0) {
    location <- data[1, ]
    latitude <- as.numeric(location$lat)
    longitude <- as.numeric(location$lon)
    return(c(latitude, longitude))
  } else {
    cat("Erreur lors de la récupération des coordonnées.\n")
    return(NULL)
  }
}

# Create lat, lon, and NomLieu columns
data$lat <- NA
data$lon <- NA


# Définition des mappings pour les mises à jour de 'LieuExercice' et 'Département'
data[LieuExercice == "Case", LieuExercice := "NC"]
data[LieuExercice == "", LieuExercice := "NC"]

data[LieuExercice == "Paris - La Défense", Département := "92"]
data[LieuExercice == "Suresnes", Département := "92"]
data[LieuExercice == "Île-de-France", Département := "75"]
data[LieuExercice == "Île-de-France - Auvergne-Rhône-Alpes", Département := "75"]
data[LieuExercice == "Paris, Pessac", Département := "75"]
data[LieuExercice == "Bordeaux, Montpellier", Département := "33"]
data[LieuExercice == "Paris, Bordeaux", Département := "75"]
data[LieuExercice == "Ajaccio - 2A", Département := "2A"]
data[LieuExercice == "Bastia - 2B", Département := "2B"]
data[LieuExercice == "Normandie", Département := "76"]
data[LieuExercice == "Grand Est", Département := "67"]
data[LieuExercice == "Rhône-Alpes", Département := "69"]
data[LieuExercice == "Bretagne", Département := "35"]
data[LieuExercice == "Languedoc-Roussillon - Sud-Ouest -", Département := "31"]
data[LieuExercice == "Sainte-Luce-sur-Loire, Paris", Département := "75"]
data[LieuExercice == "Champagne-Ardenne - Nord -", Département := "51"]
data[LieuExercice == "NC", Département := NA]
data[LieuExercice == "France", Département := NA ]



data$LieuExerciceCorr <- data$LieuExercice
data[LieuExercice == "Paris - La Défense", LieuExerciceCorr := "Puteaux"]
data[LieuExercice == "Ajaccio - 2A", LieuExerciceCorr := "Ajaccio"]
data[LieuExercice == "Bastia - 2B", LieuExerciceCorr := "Bastia"]
data[LieuExercice == "Paris, Pessac", LieuExerciceCorr := "Paris"]
data[LieuExercice == "Bordeaux, Montpellier", LieuExerciceCorr := "Bordeaux"]
data[LieuExercice == "Paris, Bordeaux", LieuExerciceCorr := "Paris"]
data[LieuExercice == "Île-de-France", LieuExerciceCorr := "Paris"]
data[LieuExercice == "Île-de-France - Auvergne-Rhône-Alpes", LieuExerciceCorr := "Paris"]
data[LieuExercice == "Centrale nucleaire du Tricastin", LieuExerciceCorr := "Tricastin"]

data[LieuExercice == "NC", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Normandie", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Grand Est", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Rhône-Alpes", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Bretagne", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Languedoc-Roussillon - Sud-Ouest -", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Sainte-Luce-sur-Loire, Paris", LieuExerciceCorr := nom_departement]
data[LieuExercice == "France", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Champagne-Ardenne - Nord -", LieuExerciceCorr := nom_departement]

# vérification à la main
data[LieuExercice == "Fin", LieuExerciceCorr := "Paris"] 
data[LieuExercice == "Ales", LieuExerciceCorr := "Alès"]
data[LieuExercice == "Macon", LieuExerciceCorr := "Macôn"]
data[LieuExercice == "Torce", LieuExerciceCorr := "Torcé"]

data[is.na(LieuExerciceCorr), LieuExerciceCorr := ""]

data[is.na(Département), .(NA_Count = .N), by = .(LieuExercice)][,LieuExercice]


#pb ligne 2123,
for (i in seq_along(data$LieuExerciceCorr)) {
  ville <- data$LieuExerciceCorr[i]
  cat("\014")  
  cat(i/nrow(data)*100,"% \n")
  if (ville != ""){
    coordinates <- get_lat_long(ville)
    data$lat[i] <- coordinates[1]
    data$lon[i] <- coordinates[2]
    if (is.na(data$lat[i])){
      cat(i, ville, data$lat[i], "\n")
    }
  }
}


# # if the code stop due to my internet connection
# j <- i
# for (i in j:nrow(data)) {
#   ville <- data$LieuExerciceCorr[i]
#   cat("\014")
#   cat(i/nrow(data)*100,"% \n")
#   if (ville != ""){
#     coordinates <- get_lat_long(ville)
#     data$lat[i] <- coordinates[1]
#     data$lon[i] <- coordinates[2]
#     if (is.na(data$lat[i])){
#       cat(i, ville, data$lat[i], "\n")
#     }
#   }
# }


# Renomage des variables :

# job_data <- fread("data/job_data.csv", header = T)
data$old_LieuExercice <- data$LieuExercice
data$LieuExercice <- data$LieuExerciceCorr
data[,LieuExerciceCorr :=NULL]
colnames(data)[4] <- "DuréeEmploi"
colnames(data)[15] <- "CompétencesDemandées"
colnames(data)[14] <- "SecteurEntreprise"




# Ajoute de variable pour les graphiques :

extraire_comp <- function(competences_offre){
  competences_offre <- tolower(competences_offre)
  competences_offre_liste <- unlist(str_split(competences_offre, ",\\s*"))
  return(competences_offre_liste)
}

library(ggplot2)

# Préparer les données pour le pie chart des compétences
# Séparer les compétences et compter leur fréquence
data_compétences <- data[, .(Compétences = unlist(strsplit(Compétences, ",\\s*"))), by = .(ID)]
data_compétences <- data_compétences[, .(Frequence = .N), by = .(Compétences)]
setorder(data_compétences, -Frequence)
top_compétences <- head(data_compétences, 5)

# Préparer les données pour le bar plot des zones géographiques
data_zones_geo <- data[!is.na(nom_departement), .(Offres = .N), by = .(nom_departement)]
setorder(data_zones_geo, -Offres)
top_zones_geo <- head(data_zones_geo, 10)

ggplot(top_compétences, aes(x = "", y = Frequence, fill = Compétences)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Top 5 Compétences Demandées", fill = "Compétence") +
  theme_void()


ggplot(top_zones_geo, aes(x = reorder(nom_departement, Offres), y = Offres)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 des Départements par Nombre d'Offres", x = "nom_departement", y = "Nombre d'offres") +
  theme_minimal()


write.csv2(data,"data/data3.csv")
