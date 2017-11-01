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
data0<-subset(annee, NOM_SITE=="Antioche" | NOM_SITE=="Eyrac", select = c("pH","Temperature", "Salinite", "Oxygene", "DN15", "DC13", "CN"))

data<-subset(annee, NOM_SITE=="Antioche" | NOM_SITE=="Eyrac", select = c("Annee","NOM_SITE","pH","Temperature", "Salinite", "Oxygene", "DN15", "DC13", "CN")) # utile pour la représentation graphique

nb_na<-sum(is.na(data0)) # Calculer nombre de NA présent dans le sous ensemble
c("Il y a", nb_na,"NA")

# Calcul du nombre de NA pour chaque colonnes
nom_col<-as.data.frame(colnames(data0))
nom_col<-t(nom_col) # transposition du dataframe pour avoir le nombre de lignes et colonnes souhaité

na_df<-data.frame(nom_col) # Création d'un dataframe à partir du nom de colonnes
rownames(na_df)<-"NA"
dim(na_df)
?seq
for (i in seq(from = 1, to = length(colnames(data0)), by = 1)) {
  na_df[i]<-sum(is.na(data0[,i])); # ajout valeur NA pour chaque colonne
}
na_df
colnames(na_df)<-c(nom_col[1], nom_col[2], nom_col[3], nom_col[4], nom_col[5], nom_col[6], nom_col[7])
na_df


data0bis<-na.omit(data0)
databis<-na.omit(data)


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
ACP<-PCA(scale(data0bis), scale.unit = TRUE, graph= FALSE)
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
plot(DN15~Annee, data=databis, type = "n", main = "Température selon Année")
with(subset(databis, NOM_SITE=="Eyrac"), c(lines(Annee, DN15, col = 1), points(Annee, DN15, col = 1, pch = 16)))
with(subset(databis, NOM_SITE=="Antioche"), c(lines(Annee, DN15, col = 2), points(Annee, DN15, col = 2, pch = 17)))
legend("topleft", legend=c("Eyrac", "Antioche"), col=c(1:2), pch = c(16,17), lty = 1, title = "Sites")

# DEuxième représentation posible
library(lattice)
xyplot(DN15~Annee, groups = NOM_SITE, data=databis, type = "b", main="Température~Année", 
  xlab="Années", ylab="DN15", col=c(1:2), pch=c(16:17), 
  key=list(space="right", lines=list(col=c(1:2)), text=list(c("Eyrac","Antioches")))
)

