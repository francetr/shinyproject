#######################
# Authors:
# Tristan FRANCES
# Antoine KOURMANALIEVA
# Mercia NGOMA KOMB
#######################

library(shiny)
setwd("~/Bureau/m2/s3/stat/project/script")
mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
annee<-read.csv("bdd_annee_somlit.csv", header = T,  dec = ".")

#-------- user interface part
shinyUI( navbarPage(
  # App title ----
  ("MySomlit"),  windowTitle="mySomlit", fluid =TRUE,
  
  #--- Visualisation of the data
  # tabPanel("Table des données",
  #   dataTableOutput("annee")
  # ),
  
  # First column choices of parameters, stations and geographic representaion
  tabPanel("Choix des données",
           fluidRow(
             #-------- Stations and parametres choices
             column(12,
                    h2("Choix des stations et paramètres"),
                    #----- Stations choices
                    column(4, helpText("Au moins une station doit être sélectionnée"),
                          
                           checkboxGroupInput(inputId = "stations", label = h4("Choix des stations")
                                              ,choices=levels(annee$NOM_SITE)
                                              #,select=levels(annee$NOM_SITE)
                           ), actionButton(inputId = "allStations", label = "Select all")
                           , actionButton(inputId = "noStations", label = "Deselect all")
                           , p("Vous avez sélectionné les stations : "), hr() , verbatimTextOutput("stations")
                           
                    ),
                    #----- Stations vizualisation in map
                    column(4,
                           h3("Représentation géographique des stations sélectionnées")
                    ),
                    #----- Parametres choices
                    column(4, helpText("Deux paramètres doivent être sélectionnés"),
                           checkboxGroupInput(inputId = "parametres", label = h4("Choix des paramètres"), choices = tail(colnames(annee),-1)
                                              #,select = tail(colnames(annee),-1)
                           ) # tail permet d'enlever un paramètre inutile (le nom du site)
                           , actionButton(inputId = "allParametres", label = "Select all")
                           , actionButton(inputId = "noParametres", label = "Deselect all")
                           , p("Vous avez sélectionné les paramètres suivants : "), hr(), verbatimTextOutput("parametres")
                    ),
                    #---- Data displays according the parametres selected by user
                    column(12, helpText("Calcul du nombre de NA pour les stations et paramètres sélectionnés")
                           ,dataTableOutput("nb_na") 
                           #,textOutput("nb_na")
                    )
             )
           )
  ),
  #------- Panel containing the ACP (use conditionnal panel?)
  tabPanel("ACP",
           fluidRow(
             h2("Analyse multivariée"),
             column(4, 
                    plotOutput("vfm")),
             column(4, 
                    plotOutput("ifm")),
             column(4, 
                    plotOutput("ebouli"))
             
           )
  ),
  
  
  #------- Panel containing the graphic representations
  tabPanel("Représentations",
           fluidRow(
             h2("Représentations graphiques"),
             column(12
             )
           )
  )
)
)