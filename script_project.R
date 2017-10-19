setwd("~/Bureau/m2/s3/stat/project")
######################################
# Imporation des données : paramètres mesurés selon mois et selon année
######################################

mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
annee<-read.csv("bdd_annee_somlit.csv", header = T,  dec = ".")

#############
# Transformation des paramètres année et mois en facteur
#############

# annee$Annee<-as.factor(annee$Annee)
# mois$Annee<-as.factor(mois$Annee); mois$Mois<-as.factor(mois$Mois)

# annee$Annee<-as.numeric(annee$Annee)
# mois$Annee<-as.numeric(mois$Annee); mois$Mois<-as.numeric(mois$Mois)


#############
# Analyse multivariée
#############
str(annee)
summary(annee)

# exemple d'ACP pour Antioche pour paramètre pH et température

# choix des paramètres(select) et de l'année/site
data<-subset(annee, NOM_SITE=="Antioche", select = c("pH","Temperature", "Salinite", "Oxygene"))

#nb_na<-sum(is.na(annee))
#c("Il y a", nb_na,"NA")
# calcul moyenne

library(FactoMineR)
ACP<-PCA(scale(data), scale.unit = TRUE )
round(ACP$eig,2)
barplot(ACP$eig[,1], main="Ebouli des valeurs propres", xlab = "Composantes", ylab="Valeurs propres")
100/ncol(data) #contrib min
round(ACP$var$contrib[,1:2],2 )
round (ACP$var$cos2[,1] + ACP$var$cos2[,2],2) # Somme des cos2 des variables pour deux axes gardés
coordP<-round(ACP$var$coord[,1:2],2);coordP #Coordonnées sur le cercle de corrélation
coordS<-round(ACP$ind$coord[,1:2],2);coordS #Coordonnées des stations

par(mfrow=c(1,2))
plot(ACP,choix="var",axes= c(1,2))
plot(ACP,choix="ind",axes= c(1,2))
library(ade4)
s.class(ACP$ind$coord[,c(1,2)] , fac=annee$NOM_SITE, col=c(1:20) )
s.class(ACP$ind$coord[,c(1,2)] , fac=annee$Annee, col=c(1:20) )


library(lattice)
xyplot(Temperature~Annee, groups = NOM_SITE, data=annee, main="Température~Année", xlab="Années", ylab="Temps", col=c(1:4),pch=c(16:20))

plot(Temperature~Annee, annee, type = "l", main = "Température selon Année")
with(subset(annee, NOM_SITE=="Eyrac"), plot(Annee, Temperature, type = "l", points(Temperature~Annee, col = "green")))
with(subset(annee, NOM_SITE=="Antioche"), plot(Annee, Temperature, type= "l", points(Temperature~Annee, col = "blue")))

?plot