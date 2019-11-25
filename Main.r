library(ggplot2)
library(plyr)
library(dplyr)

storeLocations = read.csv("~/Documents/5A/R/Rproject/starbucks.csv", sep=",")
countryCodes = read.csv("~/Documents/5A/R/Rproject/country_codes.csv", sep=",")

#test
#frLocations = storeLocations[storeLocations$Country == "FR",]


#Extrait un classement des pays ainsi qu'un top 5
countriesByNumberOfStores = ddply(storeLocations, .(Country), function(x)nrow(x))
countriesByNumberOfStores = countriesByNumberOfStores[with(countriesByNumberOfStores, order(-V1)),]              #ordonne par nombre de magasin ascendant
Top5Countries = head(countriesByNumberOfStores, 5)                                                               # on recupere le top 5
countryCode_vector = match (Top5Countries$Country, countryCodes$Code)                                            #on prend leur nom de pays
Top5Countries <- data.frame(Country = countryCodes[countryCode_vector,1], NumberOfStores = Top5Countries[,2])


#inutilisable car trop de villes écrites de plusieurs façons différentes (maj/min/caractères chinois)
#et sur certaines villes seul le quartier est indiqué
citiesByNumberOfStores = ddply(storeLocations, .(City), function(x)nrow(x))

citiesByNumberOfStores = citiesByNumberOfStores[with(citiesByNumberOfStores, order(-V1)),]
topFiveCities = head(citiesByNumberOfStores, 5)






