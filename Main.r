library(ggplot2)
library(plyr)
library(dplyr)

df = read.csv("~/Documents/5A/R/Rproject/starbucks.csv", sep=",")
countryCodes = read.csv("~/Documents/5A/R/Rproject/country_codes.csv", sep=",")

#prétraitement: _Les noms des villes chinoises sont écrits en chinois 
#                 (par soucis de compréhension par tous nous les écrivont en alphabet latin)
#_Les quartiers de la ville de Tokyo remplacent le nom de la vile dans la colonne City, la faisant disparaitre des tops suivants
df[df$City=="Shinjuku-ku" | "Shibuya-ku"]<-"Tokyo"

storeLocations <- df

#Extrait un classement des pays ainsi qu'un top 5
countriesByNumberOfStores = ddply(storeLocations, .(Country), function(x)nrow(x))
countriesByNumberOfStores = countriesByNumberOfStores[with(countriesByNumberOfStores, order(-V1)),]              #ordonne par nombre de magasin ascendant
Top5Countries_code = head(countriesByNumberOfStores, 5)                                                               # on recupere le top 5
countryCode_vector = match (Top5Countries_code$Country, countryCodes$Code)                                            #on prend leur nom de pays
Top5Countries <- data.frame(Country = countryCodes[countryCode_vector,1], NumberOfStores = Top5Countries_code[,2])

ggplot(Top5Countries) +
  guides(fill=FALSE) +
  geom_col(aes(x=Country, y=NumberOfStores,fill=Country)) + 
  ggtitle("Top Five Countries with the most Starbucks Stores") +
  ylab("Number of Stores")

###Ligne permettant d'extraire uniquement les données des 5 pays mais avec toutes les colonnes###
Top5CountriesFullData =storeLocations[which(storeLocations$Country %in% Top5Countries_code$Country),] 

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

#Extrait les types de Ownerships
numberTypesOfOwnership = ddply(storeLocations,.(Ownership.Type),function(x)nrow(x))
numberTypesOfOwnership <- data.frame(Ownership_Type = numberTypesOfOwnership[,1], NumberOfStores= numberTypesOfOwnership[,2])
print(numberTypesOfOwnership$Ownership_Type)

ggplot(numberTypesOfOwnership, aes(x="",y=NumberOfStores, fill=Ownership_Type))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("#CFEAFF", "#003965", "#077BD3","#7BC4FF"))+
  ggtitle("Ownership Type Repartition Worldwide") +
  ylab("Number of Stores")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
  

#Extrait les types de Ownership par pays du top 5
OwnTypeTop5Countries  = count(Top5CountriesFullData, Country, Ownership.Type)
countryCode_vector = match(OwnTypeTop5Countries$Country, countryCodes$Code)                                           
OwnTypeTop5Countries = data.frame(Country=countryCodes[countryCode_vector,1],
                                  Ownership_Type=OwnTypeTop5Countries[,2],
                                  Amount=OwnTypeTop5Countries[,3])
####Pour une raison X ou Y la ligne au-dessus refuse de s'executer automatiquement donc faut toujours le faire à la main

ggplot(OwnTypeTop5Countries, aes(x ="", y=Amount, fill=Ownership_Type)) + 
  geom_bar(stat = "identity", width = 1, position = position_fill()) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values=c("#CFEAFF", "#077BD3", "#7BC4FF"))+
  facet_wrap(~Country) +
  ggtitle("Ownership Type in Top 5 Countries with the most Starbucks") +
  ylab("Number of Stores")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
