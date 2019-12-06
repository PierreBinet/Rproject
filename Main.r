library(ggplot2)
library(plyr)
library(dplyr)
library(rworldmap)
library(geosphere)
library(data.table)
library(maps)
library(reshape) 

storeLocations = read.csv("./starbucks.csv", sep=",")
countryCodes = read.csv("./country_codes.csv", sep=",")
countryData = read.csv("./country_data.csv", sep=",")


#adding country codes to the countryData dataset
countryData$Country <- trimws(countryData$Country)
countryData = merge(countryData, countryCodes, by.x="Country", by.y="Name", all.x=TRUE)


#################################################################################################################################################
#Extrait un classement des pays ainsi qu'un top 5
countriesByNumberOfStores = ddply(storeLocations, .(Country), function(x)nrow(x))
countriesByNumberOfStores = countriesByNumberOfStores[with(countriesByNumberOfStores, order(-V1)),]       #ordonne par nombre de magasin ascendant
Top5Countries_code = head(countriesByNumberOfStores, 5)                                                   # on recupere le top 5
countryCode_vector = match (Top5Countries_code$Country, countryData$Code)                                 #on prend leur nom de pays
Top5Countries <- data.frame(Country = countryData[countryCode_vector,1], NumberOfStores = Top5Countries_code[,2])

ggplot(Top5Countries) +
  guides(fill=FALSE) +
  geom_col(aes(x=Country, y=NumberOfStores,fill=Country)) + 
  ggtitle("Top Five Countries with the most Starbucks Stores") +
  ylab("Number of Stores")

###Ligne permettant d'extraire uniquement les données des 5 pays mais avec toutes les colonnes###
Top5CountriesFullData =storeLocations[which(storeLocations$Country %in% Top5Countries_code$Country),]


#################################################################################################################################################
#Extrait un classement des villes ainsi qu'un top 5
citiesByNumberOfStores = ddply(storeLocations, .(City,Country,State.Province), function(x)nrow(x))
citiesByNumberOfStores = citiesByNumberOfStores[with(citiesByNumberOfStores, order(-V1)),]
citiesByNumberOfStores <- data.frame (City = citiesByNumberOfStores[,1],Country=citiesByNumberOfStores[,2],Province=citiesByNumberOfStores[,3] ,NumberOfStores=citiesByNumberOfStores[,4])
Top5Cities = head(citiesByNumberOfStores, 5)

ggplot(Top5Cities) + 
  geom_col(aes(x=City, y=NumberOfStores,fill=City)) + 
  guides(fill=FALSE) +
  ggtitle("Top Five Cities with the most Starbucks Stores") +
  ylab("Number of Stores")

###Ligne permettant d'extraire uniquement les données des 5 villes mais avec toutes les colonnes###
Top5CitiesFullData =storeLocations[which((storeLocations$City %in% Top5Cities$City) & (storeLocations$Country %in% Top5Cities$Country) & (storeLocations$State.Province %in% Top5Cities$Province)) ,]


#################################################################################################################################################
#computes the number of store per habitant for each country
countryCode_vector = match(countriesByNumberOfStores$Country, countryData$Code)                                           
numberOfStorePerHab = data.frame(Country=countryData[countryCode_vector,1],
                                  NumberOfStore=countriesByNumberOfStores[,2],
                                  Population=countryData[countryCode_vector,3])
numberOfStorePerHab$result =1000000*(numberOfStorePerHab$NumberOfStore/numberOfStorePerHab$Population)

mostStorePerHab = head(numberOfStorePerHab[order(-numberOfStorePerHab$result),],10)
ggplot(mostStorePerHab) +
  guides(fill=FALSE) +
  geom_col(aes(reorder(Country, result,sum), y=result,fill=Country)) + 
  ggtitle("Top Ten Countries with the most Starbucks Stores per million inhabitants") +
  ylab("Number of Stores per million inhabitants") +
  xlab("")


#################################################################################################################################################
#computes the number of store per habitant for each country of the top five
countryCode_vector = match (Top5Countries_code$Country, countryData$Code)                                             
numberOfStorePerHabTop5 = data.frame(Country=countryData[countryCode_vector,1],
                                 NumberOfStore=Top5Countries[,2],
                                 Population=countryData[countryCode_vector,3])
numberOfStorePerHabTop5$result =1000000*(numberOfStorePerHabTop5$NumberOfStore/numberOfStorePerHabTop5$Population)

ggplot(numberOfStorePerHabTop5) +
  guides(fill=FALSE) +
  geom_col(aes(reorder(Country, result,sum), y=result,fill=Country)) + 
  ggtitle("Number of stores per million inhabitants for the top 5 countries with the most starbucks") +
  ylab("Number of Stores per million inhabitants") +
  xlab("")


#################################################################################################################################################
#computes the number of store                                           
numberOfStoreAndGDP = data.frame(Country=countryData[countryCode_vector,1],
                                 NumberOfStore=countriesByNumberOfStores[,2],
                                 GDP=((countryData[countryCode_vector,9]/1000)*countryData[countryCode_vector,3])*1000)

ggplot(numberOfStoreAndGDP) +
  guides(fill=FALSE) +
  geom_point(aes(y=NumberOfStore, x=GDP)) + 
  ggtitle("Relationship between the total GDP and the number of stores per country") +
  ylab("Number of stores") +
  xlab("Total GDP")


#################################################################################################################################################
#Extrait les types de Ownerships
numberTypesOfOwnership = ddply(storeLocations,.(Ownership.Type),function(x)nrow(x))
numberTypesOfOwnership <- data.frame(Ownership_Type = numberTypesOfOwnership[,1], NumberOfStores= numberTypesOfOwnership[,2])

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
countryCode_vector = match(OwnTypeTop5Countries$Country, countryData$Code)                                           
OwnTypeTop5Countries = data.frame(Country=countryData[countryCode_vector,1],
                                  Ownership_Type=OwnTypeTop5Countries[,2],
                                  Amount=OwnTypeTop5Countries[,3])
####La ligne au-dessus refuse de s'executer automatiquement donc il faut toujours le faire à la main

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

#################################################################################################################################################
#Plot les points des différents Starbucks dans le monde
points <- data.frame(Longitude = storeLocations$Longitude,Latitude = storeLocations$Latitude)
points[, 1] <- as.numeric(as.character( storeLocations[, 12] ))
points[, 2] <- as.numeric(as.character( storeLocations[, 13] ))
map1 <- getMap(resolution = "high")
plot(map1)
points(points$Longitude,points$Latitude, col="green",cex=.4)

#Plot les points des différents Starbucks dans les top 5 pays
points2 <- data.frame(Longitude = Top5CountriesFullData$Longitude,Latitude = Top5CountriesFullData$Latitude)
points2[, 1] <- as.numeric(as.character( Top5CountriesFullData[, 12] ))
points2[, 2] <- as.numeric(as.character( Top5CountriesFullData[, 13] ))
map2 <- getMap(resolution = "high")
plot(map2)
points(points2$Longitude,points2$Latitude, col="green",cex=.4)


#################################################################################################################################################
#Calcule la distance moyenne entre les Starbucks
aux <- data.frame(Country=Top5CitiesFullData$Country, City=Top5CitiesFullData$City, Longitude = Top5CitiesFullData$Longitude,Latitude = Top5CitiesFullData$Latitude)
aux[, 3] <- as.numeric(as.character( aux[, 3] ))
aux[, 4] <- as.numeric(as.character( aux[, 4] ))

# create all possible pairs of origin-destination in a long format
all_pairs <- expand.grid.df(aux,aux)
names(all_pairs)[5:8] <- c("country_dest","name_dest","long_dest","lat_dest")
dt <-all_pairs[ all_pairs$City==all_pairs$name_dest,]
setDT(all_pairs)[ , dist_km := distGeo(matrix(c(Longitude, Latitude), ncol = 2),matrix(c(long_dest, lat_dest), ncol = 2))/1000]

all_pairs = na.omit(all_pairs)
all_pairs <- all_pairs[all_pairs$dist_km!=0]                                  #omit when distance is 0 (same store)
all_pairs <- all_pairs[all_pairs$dist_km<100]                                 #omit distances of 100 km (must be a mistake) 
mean_dist = ddply(all_pairs, .(City),summarize,mean = mean(dist_km))

ggplot(mean_dist) + 
  geom_col(aes(x=City, y=mean,fill=City)) + 
  guides(fill=FALSE) +
  ggtitle("Top Five Cities average distance between stores") +
  ylab("Distance (km)")
