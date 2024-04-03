# chargement des données : 

setwd("/Users/apple/Desktop/Data_R")
med_metadata_eDNA <- read.csv("Med_metadonnees_ADNe.csv")

# chargement des packages : 
library(lubridate) # pour manipuler les dates/heures
library(sf) # pour manipuler des données spatiales vectorielles library(amt) # pour l'analyse des données de tracking
library(mapview) # pour la visualisation interactive des données spatiales library(raster) #pour manipuler les données spatiales raster mapviewOptions(basemaps="OpenStreetMap")


## subset_data

# retirer les lagunes, les ports et les rivières
unique(med_metadata_eDNA$component) #pour identifier les appelations des catégories qu'on souhaite retirer
med_metadata_eDNA <- subset(med_metadata_eDNA, 
                                   !(component %in% c("harbour", "lagoon", "freshwater_river")))

# ne concerver que les échantillonnages de Méditerrannée française 
unique(med_metadata_eDNA$country) 

med_metadata_eDNA <- subset(med_metadata_eDNA, 
                            !(country %in% c("Spain", "Italy", "")))


## pour le test, je ne concerve que les échantillons en confinement 
med_metadata_eDNA_lockdown <- subset(med_metadata_eDNA, lockdown == 1)

## Visualisation des données : 
# Carto des transects 

# Créer un objet sf pour les points de départ
start_points <- st_as_sf(med_metadata_eDNA_lockdown, coords = c("longitude_start_DD", "latitude_start_DD"))
# Définir le système de coordonnées WGS84 (EPSG:4326) pour les points de départ
st_crs(start_points) <- 4326


# Créer un objet sf pour les points d'arrivée
med_metadata_eDNA_lockdown <- med_metadata_eDNA_lockdown[complete.cases(med_metadata_eDNA_lockdown$longitude_end_DD, med_metadata_eDNA_lockdown$latitude_end_DD), ]
end_points <- st_as_sf(med_metadata_eDNA_lockdown, coords = c("longitude_end_DD", "latitude_end_DD"))

# Définir le système de coordonnées WGS84 (EPSG:4326) pour les points d'arrivée
st_crs(end_points) <- 4326

# Afficher les points de départ 
mapview(start_points, color = "red", cex = 0.7, alpha = 1) +
  # Afficher les points d'arrivée 
mapview(end_points, color = "blue", cex = 0.7, alpha = 1)


