#################################################
#################################################
#### Extraction des points d'échantillonnage #### 
#################################################
#################################################

setwd("/Users/apple/Desktop/Data_R/Data/raw_data")
mtdt_all <- read.csv("Med_metadonnees_ADNe.csv")

# chargement des packages : 
library(sf) # pour manipuler des données spatiales vectorielles library(amt) # pour l'analyse des données de tracking
library(mapview) # pour la visualisation interactive des données spatiales library(raster) #pour manipuler les données spatiales raster mapviewOptions(basemaps="OpenStreetMap")


### Visualisation des points d'échantillonnage 

## Carto des transects
# Créer un objet sf pour les points de départ
start_points <- st_as_sf(mtdt_all, coords = c("longitude_start_DD", "latitude_start_DD"))
# Définir le système de coordonnées WGS84 (EPSG:4326) pour les points de départ
st_crs(start_points) <- 4326


# Créer un objet sf pour les points d'arrivée
#mtdt_all <- mtdt_all[complete.cases(mtdt_all$longitude_end_DD, mtdt_all$latitude_end_DD), ]
end_points <- st_as_sf(mtdt_all, coords = c("longitude_end_DD", "latitude_end_DD"), na.fail = FALSE)
# Définir le système de coordonnées WGS84 (EPSG:4326) pour les points d'arrivée
st_crs(end_points) <- 4326

# 
# #Create a new column with the WKT geometry (Marieke)
# mtdt_all$wkt_geometry <- ifelse(is.na(mtdt_all$longitude_start_DD) | is.na(mtdt_all$latitude_start_DD) |
#                                   is.na(mtdt_all$longitude_end_DD) | is.na(mtdt_all$latitude_end_DD),
#                                 paste("POINT(", mtdt_all$longitude_start_DD, " ", mtdt_all$latitude_start_DD, ")",
#                                       sep = ""),
#                                 paste("LINESTRING(",
#                                       mtdt_all$longitude_start_DD, " ", mtdt_all$latitude_start_DD, ", ",
#                                       mtdt_all$longitude_end_DD, " ", mtdt_all$latitude_end_DD, ")",
#                                       sep = ""))
# 
# 
# 
# # Ajouter les transects sur la carte
# mtdt_all <- st_as_sf(mtdt_all, wkt = "wkt_geometry")
# # Définir le système de coordonnées WGS84 (EPSG:4326) pour les lignes
# st_crs(mtdt_all) <- 4326
# 
# 
# 
# ### Représentation des points et transects 
# Global_samplings <- mapview(start_points, color = "red", cex = 0.7, alpha = 1) +
#   mapview(end_points, color = "blue", cex = 0.7, alpha = 1) +
#   mapview(mtdt_all, color = "black", cex = 0.7, alpha = 0.7) 
# 
# Global_samplings


#########

## RÉCUPÉRER LES POINTS DE MILIEU DE TRANSECT ET/OU START TRANSECT
# Créer un objet sf vide avec une colonne de géométrie vide
midpoint_sf <- st_sf(geometry = st_sfc())

# Boucle à travers chaque ligne de mtdt_all pour calculer et ajouter les points médians
for (i in 1:nrow(mtdt_all)) {
  if (is.na(mtdt_all$longitude_end_DD[i]) || is.na(mtdt_all$latitude_end_DD[i])) {
    # Si les coordonnées de fin sont manquantes, utiliser les coordonnées de départ
    midpoint <- st_point(c(mtdt_all$longitude_start_DD[i], mtdt_all$latitude_start_DD[i]))
  } else {
    # Calculer les coordonnées du point médian
    mid_longitude <- (mtdt_all$longitude_start_DD[i] + mtdt_all$longitude_end_DD[i]) / 2
    mid_latitude <- (mtdt_all$latitude_start_DD[i] + mtdt_all$latitude_end_DD[i]) / 2
    # Créer un objet POINT pour le point médian
    midpoint <- st_point(c(mid_longitude, mid_latitude))
  }
  # Ajouter le point médian à l'objet sf
  midpoint_sf <- rbind(midpoint_sf, st_sf(geometry = st_sfc(midpoint)))
}

# Afficher les points médians
midpoint_sf

# Afficher les points de départ, d'arrivée, les transects et les points médians dans mapview
Global_samplings <- mapview(start_points, color = "red", cex = 0.7, alpha = 1) +
  mapview(end_points, color = "blue", cex = 0.7, alpha = 1) +
  # mapview(mtdt_all, color = "black", cex = 0.7, alpha = 0.7) +
  mapview(midpoint_sf, color = "green", cex = 0.7, alpha = 1)

# Afficher la carte
Global_samplings

### Créer un buffer de 1 km autour des points médians
# midpoint_buffer <- st_buffer(midpoint_sf, dist = 0.016)


# Définition des CRS 

# st_crs(start_points) <- 2154
# st_crs(end_points) <- 2154
st_crs(midpoint_sf) <- 4326
# st_crs(midpoint_buffer) <- 2154

start_points <- st_transform(start_points, 2154)
end_points <- st_transform(end_points, 2154)
midpoint_sf <- st_transform(midpoint_sf, 2154)

### Créer un buffer de 1 km autour des points médians
midpoint_buffer <- st_buffer(midpoint_sf, dist = 1000)
midpoint_buffer <- st_transform(midpoint_buffer, 2154)
str(midpoint_buffer)

# Afficher les buffers sur la carte
Global_samplings <- mapview(start_points, color = "red", cex = 0.7, alpha = 1) +
  mapview(end_points, color = "blue", cex = 0.7, alpha = 1) +
  # mapview(mtdt_all, color = "black", cex = 0.7, alpha = 0.7) +
  mapview(midpoint_sf, color = "green", cex = 0.7, alpha = 1) +
  mapview(midpoint_buffer, color = "yellow", cex = 0.7, alpha = 0.7)

# Afficher la carte
Global_samplings




# # Enregistrer midpoint_buffer
# st_write(midpoint_buffer, "midpoint_buffer.shp")
# # Enregistrer midpoint_sf
# st_write(midpoint_sf, "midpoint_sf.shp")



################################################
################################################
#### Extraction des données de covariables #####
################################################
################################################

# Charger le package raster
library(raster)
library(mapview)

# Changement de CRS lambert 2154 = CRS d'extraction pour les buffers 
# On veut des métriques, donc on prend du lambert 54 et pas en degrés avec WG84
start_points <- st_transform(start_points, crs = 2154)
end_points <- st_transform(end_points, crs = 2154)
midpoint_sf <- st_transform(midpoint_sf, crs = 2154)
midpoint_buffer <- st_transform(midpoint_buffer, crs = 2154)


####################################################
#################### Gravity #######################
####################################################
setwd("/Users/apple/Desktop/Covariables/Gravity") 

# Charger le raster
raster_gravity <- raster("rastGravity_ZEEFRMCMed.tif")
raster_gravity <- projectRaster(raster_gravity, crs = "+init=epsg:2154")

par(mfrow = c(1, 1))
plot(raster_gravity, main = "Gravity")


############## Pour start point ####################
####################################################
# On utilise les valeurs de startpoint, puisque certains points de milieu de transect sont situés sur terre
# On vérifie que les objets sont bien des objets spaciaux avec les bonnes coordonnées CRS, sinon on les met en forme 
#class(start_points)
#crs(start_points)

# # Extraire les valeurs raster pour les milieux de transect.
#mtdt_all$gravity_start <- extract(raster_gravity, start_points)
####################################################

############## Pour la zone buffer #################
####################################################

gravity <- extract(raster_gravity, midpoint_buffer, layer = 1)

# Fonction pour calculer la moyenne en prenant en compte les valeurs NA
calcul_moyenne_ligne <- function(valeurs) {
  valeurs_numeric <- as.numeric(valeurs)
  # Retourner NA si toutes les valeurs sont NA ou NULL
  if(all(is.na(valeurs_numeric)) | all(is.null(valeurs_numeric))) {
    return(NA)
  } else {
    # Calculer la moyenne en ignorant les valeurs NA
    return(mean(valeurs_numeric, na.rm = TRUE))
  }
}


## Extraction pour la première bande : posidonia seagrass
# Calculer la moyenne des valeurs par ligne
moyennes_par_ligne <- sapply(gravity, calcul_moyenne_ligne)
# Afficher les moyennes
head(moyennes_par_ligne) 
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs
mtdt_all$gravity <- moyennes_par_ligne
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)



####################################################
################## Distance shore ################## 
####################################################
setwd("/Users/apple/Desktop/Covariables/Dist_shore") 

# Charger le raster
raster_dshore <- raster("Dist_shore_cut.tif")
raster_dshore <- projectRaster(raster_dshore, crs = "+init=epsg:2154")

par(mfrow = c(1, 1))
plot(raster_dshore, main = "Distance to the shore")

############## Pour start point ####################
####################################################
# # On utilise les valeurs de startpoint, puisque certains points de milieu de transect sont situés sur terre
# # On vérifie que les objets sont bien des objets spaciaux avec les bonnes coordonnées CRS, sinon on les met en forme 
# class(start_points)
# crs(start_points)
# 
# # Extraire les valeurs raster pour les milieux de transect.
# mtdt_all$dshore_start <- extract(raster_dshore, start_points)
####################################################


############## Pour la zone buffer #################
####################################################

dshore <- extract(raster_dshore, midpoint_buffer, layer = 1)

## Extraction pour la première bande : posidonia seagrass
# Calculer la moyenne des valeurs par ligne
moyennes_par_ligne <- sapply(dshore, calcul_moyenne_ligne)
# Afficher les moyennes
head(moyennes_par_ligne) 
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs
mtdt_all$dshore <- moyennes_par_ligne
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)


####################################################
################# Principal Habitat ################
####################################################

setwd("/Users/apple/Desktop/Covariables/Habitat") 

# Charger les 7 premières bandes du raster Habitat
habitat_band1 <- raster("bioc_landscape_indices_bathy_anchorings_2023_medfr_1000m_2154.tif")
habitat_band1 <- projectRaster(habitat_band1, crs = "+init=epsg:2154")

habitat_band2 <- raster("bioc_landscape_indices_bathy_anchorings_2023_medfr_1000m_2154.tif", band = 2)
habitat_band2 <- projectRaster(habitat_band2, crs = "+init=epsg:2154")

habitat_band3 <- raster("bioc_landscape_indices_bathy_anchorings_2023_medfr_1000m_2154.tif", band = 3)
habitat_band3 <- projectRaster(habitat_band3, crs = "+init=epsg:2154")

habitat_band4 <- raster("bioc_landscape_indices_bathy_anchorings_2023_medfr_1000m_2154.tif", band = 4)
habitat_band4 <- projectRaster(habitat_band4, crs = "+init=epsg:2154")

habitat_band5 <- raster("bioc_landscape_indices_bathy_anchorings_2023_medfr_1000m_2154.tif", band = 5)
habitat_band5 <- projectRaster(habitat_band5, crs = "+init=epsg:2154")

habitat_band6 <- raster("bioc_landscape_indices_bathy_anchorings_2023_medfr_1000m_2154.tif", band = 6)
habitat_band6 <- projectRaster(habitat_band6, crs = "+init=epsg:2154")

habitat_band7 <- raster("bioc_landscape_indices_bathy_anchorings_2023_medfr_1000m_2154.tif", band = 7)
habitat_band7 <- projectRaster(habitat_band7, crs = "+init=epsg:2154")

#Visualisation des bandes 
par(mfrow = c(2, 2))
plot(habitat_band1, main = "Habitat posidonia seagrass")
plot(habitat_band2, main = "Habitat coralligeneous")
plot(habitat_band3, main = "Habitat rocks")
plot(habitat_band4, main = "Habitat sand")
plot(habitat_band5, main = "Habitat dead matte")
plot(habitat_band6, main = "Habitat other seagrass")
plot(habitat_band7, main = "Habitat infralittoral algae")


# Ici, on va extraire la valeur de l'habitat principal 
# dans un rayon de 1000m autour du point central d'échantillonnage midpoint 

Global_samplings_habitats <- mapview(start_points, color = "red", cex = 0.7, alpha = 1) +
  # mapview(end_points, color = "blue", cex = 0.7, alpha = 1) +
  # mapview(mtdt_all, color = "black", cex = 0.7, alpha = 0.7) +
  mapview(midpoint_sf, color = "green", cex = 0.7, alpha = 1) +
  mapview(midpoint_buffer, color = "yellow", cex = 0.7, alpha = 0.7) +
  mapview(habitat_band1) #+ mapview (habitat_band2) + mapview (habitat_band3) + mapview(habitat_band4) + mapview(habitat_band5) + mapview(habitat_band6) + mapview(habitat_band7)

Global_samplings_habitats


## Extraire les valeurs raster pour les buffers de milieux de transect pour chaque bande

values_bande_1 <- extract(habitat_band1, midpoint_buffer, layer = 1)
values_bande_2 <- extract(habitat_band2, midpoint_buffer, layer = 1)
values_bande_3 <- extract(habitat_band3, midpoint_buffer, layer = 1)
values_bande_4 <- extract(habitat_band4, midpoint_buffer, layer = 1)
values_bande_5 <- extract(habitat_band5, midpoint_buffer, layer = 1)
values_bande_6 <- extract(habitat_band6, midpoint_buffer, layer = 1)
values_bande_7 <- extract(habitat_band7, midpoint_buffer, layer = 1)


# Fonction pour calculer la moyenne en prenant en compte les valeurs NA
 calcul_moyenne_ligne <- function(valeurs) {
   valeurs_numeric <- as.numeric(valeurs)
   # Retourner NA si toutes les valeurs sont NA ou NULL
   if(all(is.na(valeurs_numeric)) | all(is.null(valeurs_numeric))) {
     return(NA)
   } else {
     # Calculer la moyenne en ignorant les valeurs NA
     return(mean(valeurs_numeric, na.rm = TRUE))
   }
 }
 
 ## Extraction pour la première bande : posidonia seagrass
 # Calculer la moyenne des valeurs par ligne
 moyennes_par_ligne <- sapply(values_bande_1, calcul_moyenne_ligne)
 # Afficher les moyennes
 head(moyennes_par_ligne) 
 # Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs
 mtdt_all$posidonia_seagrass <- moyennes_par_ligne
 # Afficher les premières lignes de mtdt_all pour vérifier
 head(mtdt_all)
 
 

## Extraction pour la seconde bande : coralligeneous 
# Calculer la moyenne des valeurs par ligne pour la bande 2
moyennes_bande_2 <- sapply(values_bande_2, calcul_moyenne_ligne)
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs pour la bande 2
mtdt_all$coralligeneous <- moyennes_bande_2
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)
 

## Extraction pour la troisième bande : rocks 
# Calculer la moyenne des valeurs par ligne pour la bande 3
moyennes_bande_3 <- sapply(values_bande_3, calcul_moyenne_ligne)
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs pour la bande 3
mtdt_all$rocks <- moyennes_bande_3
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)


## Extraction pour la quatrième bande : sand 
# Calculer la moyenne des valeurs par ligne pour la bande 4
moyennes_bande_4 <- sapply(values_bande_4, calcul_moyenne_ligne)
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs pour la bande 4
mtdt_all$sand <- moyennes_bande_4
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)


## Extraction pour la troisième bande : dead matte 
# Calculer la moyenne des valeurs par ligne pour la bande 5
moyennes_bande_5 <- sapply(values_bande_5, calcul_moyenne_ligne)
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs pour la bande 5
mtdt_all$dead_matte <- moyennes_bande_5
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)


## Extraction pour la troisième bande : other seagrass 
# Calculer la moyenne des valeurs par ligne pour la bande 6
moyennes_bande_6 <- sapply(values_bande_6, calcul_moyenne_ligne)
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs pour la bande 6
mtdt_all$other_seagrass <- moyennes_bande_6
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)


## Extraction pour la troisième bande : other seagrass 
# Calculer la moyenne des valeurs par ligne pour la bande 7
moyennes_bande_7 <- sapply(values_bande_7, calcul_moyenne_ligne)
# Ajouter une nouvelle colonne à mtdt_all avec les moyennes des valeurs pour la bande 7
mtdt_all$infralitoral_algae <- moyennes_bande_7
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)


## Nouvelle colonne habitat principal
# Sélectionner les colonnes de posidonia_seagrass à infralitoral_algae
habitat_cols <- mtdt_all[, c("posidonia_seagrass", "coralligeneous", "rocks", "sand", "dead_matte", "other_seagrass", "infralitoral_algae")]
# Convertir les valeurs en numériques
habitat_cols <- lapply(habitat_cols, function(x) as.numeric(as.character(x)))
# Convertir habitat_cols en une matrice
habitat_matrix <- do.call(cbind, habitat_cols)
# Obtenir l'indice de la colonne avec la valeur maximale pour chaque ligne
max_col_index <- apply(habitat_matrix, 1, which.max)
# Obtenir les noms de colonnes
col_names <- colnames(habitat_matrix)
# Extraire le nom de colonne correspondant à l'indice
max_col_name <- sapply(max_col_index, function(i) col_names[i])
# Ajouter la nouvelle colonne au dataframe mtdt_all
mtdt_all$habitat_principal <- max_col_name
# Afficher les premières lignes de mtdt_all pour vérifier
head(mtdt_all)


## Nouvelle colonne nombre d'habitats 

# Sélection des colonnes d'habitat
habitat_cols <- mtdt_all[, grep("^posidonia_seagrass|^coralligeneous|^rocks|^sand|^dead_matte|^other_seagrass|^infralitoral_algae$", names(mtdt_all))]
str(habitat_cols)

# Afficher le type des colonnes restantes
sapply(habitat_cols, class)

# Convertir les colonnes en numérique
#habitat_cols <- as.data.frame(lapply(habitat_cols, as.numeric))

# Remplacer les valeurs NA par 0 dans habitat_cols
habitat_cols[is.na(habitat_cols)] <- 0

# Calculer le nombre d'habitats
mtdt_all$nb_habitat <- ifelse(mtdt_all$nb_habitat == 0, NA, mtdt_all$nb_habitat)

head(mtdt_all)


##### RESULTATS FINAUX #####
# Enregistrer en XLSX
library(openxlsx)
write.xlsx(mtdt_all, "mtdt_all.xlsx")
