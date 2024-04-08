# chargement des données : 

setwd("/Users/apple/Desktop/Data_R/Data/raw_data")
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
mtdt_l <- subset(med_metadata_eDNA, lockdown == 1)

## Visualisation des données : 
# Carto des transects 

# Créer un objet sf pour les points de départ
start_points <- st_as_sf(mtdt_l, coords = c("longitude_start_DD", "latitude_start_DD"))
# Définir le système de coordonnées WGS84 (EPSG:4326) pour les points de départ
st_crs(start_points) <- 4326


# Créer un objet sf pour les points d'arrivée
mtdt_l <- mtdt_l[complete.cases(mtdt_l$longitude_end_DD, mtdt_l$latitude_end_DD), ]
end_points <- st_as_sf(mtdt_l, coords = c("longitude_end_DD", "latitude_end_DD"))

# Définir le système de coordonnées WGS84 (EPSG:4326) pour les points d'arrivée
st_crs(end_points) <- 4326

# Afficher les points de départ 
mapview(start_points, color = "red", cex = 0.7, alpha = 1) +
  # Afficher les points d'arrivée 
  mapview(end_points, color = "blue", cex = 0.7, alpha = 1)



# Create a new column with the WKT geometry
mtdt_l$wkt_geometry <- ifelse(is.na(mtdt_l$longitude_start_DD) | is.na(mtdt_l$latitude_start_DD) |
                                is.na(mtdt_l$longitude_end_DD) | is.na(mtdt_l$latitude_end_DD),
                              paste("POINT(", mtdt_l$longitude_start_DD, " ", mtdt_l$latitude_start_DD, ")",
                                    sep = ""),
                              paste("LINESTRING(",
                                    mtdt_l$longitude_start_DD, " ", mtdt_l$latitude_start_DD, ", ",
                                    mtdt_l$longitude_end_DD, " ", mtdt_l$latitude_end_DD, ")",
                                    sep = ""))


# Ajouter les transects sur la carte 
mtdt_l <- st_as_sf(mtdt_l, wkt = "wkt_geometry")

# Définir le système de coordonnées WGS84 (EPSG:4326) pour les lignes
st_crs(mtdt_l) <- 4326

# Ajouter les transects sur la carte : 
mapview(start_points, color = "red", cex = 0.7, alpha = 1) +
  mapview(end_points, color = "blue", cex = 0.7, alpha = 1) +
  mapview(mtdt_l, color = "black", alpha = 0.7)

