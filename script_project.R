#######################
# Authors:
# Tristan FRANCES
# Antoine KOURMANALIEVA
# Mercia NGOMA KOMB
#######################

setwd("~/Bureau/m2/s3/stat/project/script/")
######################################
# Imporation des données : paramètres mesurés selon mois et selon année
######################################

mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
annee<-read.csv("bdd_annee_somlit.csv", header = T,  dec = ".")

#############
# Traitement des données pour l'Analyse multivariée ACP
#############
str(annee)
summary(annee)

# exemple d'ACP pour Antioche et Eyrac pour paramètre pH, température, Sallinité et Oxygene 

# choix des paramètres(select) et de l'année/site
data0<-subset(annee, NOM_SITE=="Antioche" | NOM_SITE=="Eyrac", select = c("pH","Temperature", "Salinite", "Oxygene"))
data0<-na.omit(data0)

data<-subset(annee, NOM_SITE=="Antioche" | NOM_SITE=="Eyrac", select = c("Annee","NOM_SITE","pH","Temperature", "Salinite", "Oxygene")) # utile pour la représentation graphique
data<-na.omit(data)

nb_na<-sum(is.na(data0)) # Calculer nombre de NA présent dans le sous ensemble
c("Il y a", nb_na,"NA")

#############
# Transformation des paramètres année et mois en facteur (pour une représentation utilisant s.class)
#############

# data$Annee<-as.factor(data$Annee)
# mois$Annee<-as.factor(mois$Annee); mois$Mois<-as.factor(mois$Mois)

#############
# Réalisation de l'ACP
#############

library(FactoMineR)
?PCA
ACP<-PCA(scale(data0), scale.unit = TRUE, graph= FALSE)
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

#library(ade4)
#s.class(ACP$ind$coord[,c(1,2)] , fac=data$NOM_SITE, col=c(1:20) )
#s.class(ACP$ind$coord[,c(1,2)] , fac=data$Annee, col=c(1:20) )


###############
# Représentations graphiques avec plot ou xyplot
###############

par(mfrow=c(1,1))
plot(Temperature~Annee, data=data, type = "n", main = "Température selon Année")

with(subset(data, NOM_SITE=="Eyrac"), c(lines(Annee, Temperature, col = 1), points(Annee, Temperature, col = 1, pch = 16)))
with(subset(data, NOM_SITE=="Antioche"), c(lines(Annee, Temperature, col = 2), points(Annee, Temperature, col = 2, pch = 17)))
legend("topleft", legend=c("Eyrac", "Antioche"), col=c(1:2), pch = c(16,17), lty = 1, title = "Sites")

library(lattice)
# /!\ Problème de légende
xyplot(Temperature~Annee, groups = NOM_SITE, data=data, type = "b", main="Température~Année", 
  xlab="Années", ylab="Températures", col=c(1:2), pch=c(16:17), 
  key=list(space="right", lines=list(col=c(1:2)), text=list(c("Eyrac","Antioches")))
)
