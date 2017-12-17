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
data0<-subset(annee, NOM_SITE=="Antioche" | NOM_SITE=="Eyrac" , select = c("pH","Temperature", "Salinite", "Oxygene", "DN15", "DC13", "CN"))

data<-subset(annee, NOM_SITE=="Antioche" | NOM_SITE=="Eyrac", select = c("Annee","NOM_SITE","pH","Temperature", "Salinite", "Oxygene", "DN15", "DC13", "CN")) # utile pour la représentation graphique

nb_na<-sum(is.na(data0)) # Calculer nombre de NA présent dans le sous ensemble
c("Il y a", nb_na,"NA")

# Calcul du nombre de NA pour chaque colonnes
nom_col<-as.data.frame(colnames(data0))
nom_col<-t(nom_col) # transposition du dataframe pour avoir le nombre de lignes et colonnes souhaité

?dateRangeInput
na<-aggregate(data0, list(data$NOM_SITE), function(x) sum(is.na(x)))

date="2017-01-01"
min(annee$Annee)
if(min(annee$Annee)<"2016-01-01"){
  print("ok")
}
date=as.integer(date)
paste(1996, "01-01", sep="-")
?dateRangeInput
typeof((levels(annee$NOM_SITE)))
as.Date(as.character(min(annee$NOM_SITE)))

na
min(annee$Annee)
year<-as.character(min(annee$Annee)) 
max(annee$Annee)
sapply(annee , function(x) sum(is.na(x)))

sum(is.na(annee[which(annee$NOM_SITE=="Astan"), which(ncol(annee)=="Temperature")]))
tab<-apply(data0, 2, function(x) is.na(x))

tab<-subset(annee, annee$NOM_SITE == "Astan",select = c("NOM_SITE", "Temperature"))
apply(tab, 2, function(x) sum(is.na(x)))

#----- Calcul de NA pour chaque site
na<-NULL
for (station in data$NOM_SITE) {
  if (station=="Antioche"){
    na<-(apply(subset(data, NOM_SITE==station), 2, function(x) sum(is.na(x))))
  }
}
t(data.frame(na))
colnames(annee)

which(annee$NOM_SITE=="Astan")
which(levels(annee)=="Temperature")


annee[which(annee$NOM_SITE=="Astan"),which(ncol(annee)=="Temperature")]
apply(tab, 2, function(x) sum(x))

na_df<-sapply(data0, function(x) sum(is.na(x)), simplify = "array")
na_df
?dateRangeInput


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
ACP<-PCA(scale(data0bis), scale.unit = TRUE, graph= FALSE)
ACP
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

coord_var<-round(ACP$var$coord[,1:2],2)
#library(ade4)
#s.class(ACP$ind$coord[,c(1,2)] , fac=data$NOM_SITE, col=c(1:20) )
#s.class(ACP$ind$coord[,c(1,2)] , fac=data$Annee, col=c(1:20) )


###############
# Représentations graphiques avec plot ou xyplot
###############

# Représentation d'une variable selon le temps
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

?plot
?abline
# Représentation des coordonnées des variables
# Représentation avec barplots
barplot(coordP[,1], ylim = c(-1,1), main = "Coordonnées de l'axe 1 des différents paramètres sur \n le cercle de corrélation")
barplot(coordP[,2], ylim = c(-1,1), main = "Coordonnées de l'axe 2 des différents paramètres sur \n le cercle de corrélation")

# Représentation avec plots PB : PAS DE PRECISION DES NOM DE PARAMETRES POUR L'AXE X
plot(Dim.1~1, data=coordP, type = "p", main = "Coordonnées des variables pour le 1er axe", xlab="Paramètres", ylab = "Coordonnées du cercle de corrélation")
abline(a=0, b=0, lty=2, col="red")

plot(Dim.2~1, data=coordP,type = "p", main = "Coordonnées des variables pour le 2ème axe", xlab="Paramètres", ylab = "Coordonnées du cercle de corrélation")
abline(a=0, b=0, lty=2, col="red")

