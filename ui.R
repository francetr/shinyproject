#######################
# Authors:
# Tristan FRANCES
# Antoine KOURMANALIEVA
# Mercia NGOMA KOMB
#######################

library(shiny)
setwd("~/Bureau/m2/s3/stat/project/script")

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
                    column(4, helpText("Au moins une station doit être sélectionnée")
                           , uiOutput("stations_checkbox")
                           , actionButton(inputId = "allStations", label = "Select all")
                           , actionButton(inputId = "noStations", label = "Deselect all")
                           , p("Vous avez sélectionné les stations : "), hr(), verbatimTextOutput("stations")
                           ),
                    #----- Stations vizualisation in map
                    column(4,
                           h3("Représentation géographique des stations sélectionnées")
                    ),
                    #----- Parametres choices
                    column(4, strong("Années disponibles pour l'étude :"), textOutput("date_range")
                           #---- TODO
                           , dateRangeInput(inputId = "date", label = "Choix de la date", startview = "year", start = "1997-01-01" )
                           , helpText("Deux paramètres doivent être sélectionnés")
                           , uiOutput("parametres_checkbox")
                           , actionButton(inputId = "allParametres", label = "Select all")
                           , actionButton(inputId = "noParametres", label = "Deselect all")
                           , p("Vous avez sélectionné les paramètres suivants : "), hr(), verbatimTextOutput("parametres")
                    ),
                    #---- Data displays according the parametres selected by user
                    column(12, h3("Calcul du nombre de NA pour les stations et paramètres sélectionnés"), helpText("Attention suivant le nombre de NA pour un pramètre donné, l'analyse peut être fortement biaisée")
                           ,dataTableOutput("nb_na")
                           )
                    )
             )
           ),
  #------- Panel containing the ACP (use conditionnal panel?)
  tabPanel("ACP",
           column(12,
                  fluidRow(
                    h2("Analyse multivariée"),
                    column(4,
                           plotOutput("vfm")),
                    column(4,
                           plotOutput("ifm")),
                    column(4,
                           plotOutput("ebouli"))
                    )
                  )
           ),
  #------- Panel containing the graphic representations
  tabPanel("Représentations",
           column(12,
                  fluidRow(
                    h2("Représentations graphiques"),
                    column(12)
                    )
                  )
           )
  )
)