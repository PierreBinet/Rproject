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

ggplot(Top5Countries) + 
  #theme_bw() +
  guides(fill=FALSE) +
  geom_col(aes(x=Country, y=NumberOfStores,fill=Country)) + 
  ggtitle("Top Five Countries with the most Starbucks Stores") +
  ylab("Number of Stores")

#Extrait un classement des villes ainsi qu'un top 5
citiesByNumberOfStores = ddply(storeLocations, .(City), function(x)nrow(x))
citiesByNumberOfStores = citiesByNumberOfStores[with(citiesByNumberOfStores, order(-V1)),]
citiesByNumberOfStores <- data.frame (City = citiesByNumberOfStores[,1], NumberOfStores=citiesByNumberOfStores[,2])
Top5Cities = head(citiesByNumberOfStores, 5)

ggplot(Top5Cities) + 
  geom_col(aes(x=City, y=NumberOfStores,fill=City)) + 
  guides(fill=FALSE) +
  ggtitle("Top Five Cities with the most Starbucks Stores") +
  ylab("Number of Stores")

#Extrait les types de commerces