setwd("/Users/apple/Desktop/Data_R")

load("/Users/apple/Desktop/Data_R/confinement.RData")

#setwd("/Users/apple/Desktop/Data_R/Tableurs_data")


### Sélection subset confinement 

# Extraire les identifiants SPYGEN de la colonne spygen_code de mtdt_l
identifiants <- mtdt_l$spygen_code
# Filtrer les lignes de SPY_data en fonction des identifiants SPYGEN
SPY_data_filtre <- data %>%
  filter(Row.names %in% identifiants)

# Identifier les identifiants SPYGEN de mtdt_l absents dans SPY_data
identifiants_absents <- anti_join(mtdt_l, data, by = c("spygen_code" = "Row.names"))$spygen_code
# Afficher les identifiants SPYGEN absents
print(identifiants_absents)
# "SPY201221" "SPY201227" "SPY200798" "SPY200805" 

library(openxlsx)

write.xlsx(SPY_data_filtre, "SPY_data_filtre.xlsx")





