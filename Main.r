library(ggplot)
library(plyr)
library(dplyr)

storeLocations = read.csv("./starbucks.csv", sep=",")
#test
frLocations = storeLocations[storeLocations$Country == "FR",]


#à améliorer pour afficher "number of locations" à la place de "V1"
#et utiliser les vrai noms de pays a la place des codes
countriesByNumberOfStores = ddply(storeLocations, .(Country), function(x)nrow(x))


#inutilisable car trop de villes écrites de plusieurs façons différentes (maj/min/caractères chinois)
#et sur certaines villes seul le quartier est indiqué
#citiesByNumberOfStores = ddply(storeLocations, .(City), function(x)nrow(x))






