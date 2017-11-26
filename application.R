library(shiny)
library(FactoMineR)
library(lattice)
setwd("~/Bureau/m2/s3/stat/project/script")
# mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
annee<-read.csv("bdd_annee_somlit.csv", header = T,  dec = ".")


#-------- user interface part
ui <- navbarPage(
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
                    column(12
                           )
                    )
                  )
           )
  )

#--------- Server part
server <- function(input, output, session){
  #--- Display raw data
  data_annee<-reactive(
    return(annee)
    )
  
  output$annee <- renderDataTable({data_annee()})
  
  #---- Definition of the checkbox group that will be displayed in UI
  output$stations_checkbox <- renderUI(checkboxGroupInput(inputId = "stations", label = "Choix des stations", choices = levels(annee$NOM_SITE)))
  output$parametres_checkbox <- renderUI(checkboxGroupInput(inputId = "parametres", label = ("Choix des paramètres"), choices = tail(colnames(annee),-2)))
  
  #---- Button to select/deselect all the stations/parametres
  #---- For stations
  observeEvent(input$allStations, {updateCheckboxGroupInput(session, "stations", label = "Choix des stations", choices=levels(annee$NOM_SITE), selected = levels(annee$NOM_SITE) )})
  observeEvent(input$noStations, {updateCheckboxGroupInput(session, "stations", label = "Choix des stations", choices=levels(annee$NOM_SITE))})
  
  #---- For parametres
  observeEvent(input$allParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-1), selected = tail(colnames(annee),-1))})
  observeEvent(input$noParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-1))})
  
  #---- Reactive objects of the stations/parametres selected by user
  
  choix_stations<-reactive(
    {return(input$stations)}
  )
  choix_parametres<-reactive(
    {return(input$parametres)}
  )
  
  #---- Reactive objects of the date selected by user
  # TODO
  output$date_range<-renderText(as.character(c(min(annee$Annee), "à", max(annee$Annee))))

  choix_date<-reactive(
    {return(input$date)}
  )
  # output$date_start<-renderText(date_start())

  
  output$stations <- renderText({choix_stations()}) # fonction pour afficher les stations sélectionnées
  output$parametres  <- renderText({choix_parametres()}) # fonction pour afficher les paramètres sélectionnés
  #---
  #---- data for the dataframe of NA
  new_data_with_station<-reactive(
    subset(data_annee(), NOM_SITE %in% choix_stations(), select = c(choix_parametres(),"NOM_SITE"))
  )
  
  #--- Calculus of the number of NA by col and creation of the dataframe
  nb_na<-reactive(
    #----- if stations and parametres are selected
    if(length(choix_parametres())>=1 && length(choix_stations())>=1){
      na<-aggregate(new_data(), list(new_data_with_station()$NOM_SITE), function(x) sum(is.na(x)))
      colnames(na)<-c("NOM_SITE", choix_parametres()) # change the name of the first column
      return(na)
      }
  )
  output$nb_na<-renderDataTable(nb_na())
  
  #--- data with parameters setted by user
  new_data<-reactive(
    subset(data_annee(), NOM_SITE %in% choix_stations(), select = choix_parametres())
  )
  output$new_data<-renderDataTable(new_data())
  
  #--- data whitout NA
  data_sans_na<-reactive(
    na.omit(subset(data_annee(), NOM_SITE %in% choix_stations(), select = choix_parametres()))
  )
  output$data_sans_na<-renderDataTable(data_sans_na())
  
  #--- ACP result
  
  acp<-reactive(
    PCA(scale(data_sans_na()), graph=FALSE))
  
  vfm<-reactive(
    plot(acp(), choix="var",axes = c(1,2))
  )
  output$vfm<-renderPlot(vfm())
  
  #--- ebouli
  
  ebouli<-reactive(
    barplot(acp()$eig[,1], main="Ebouli des valeurs propres", xlab = "Composantes", ylab="Valeurs propres")
  )
  output$ebouli<-renderPlot(ebouli())
  
  #--- individual factor map
  
  ifm<-reactive(
    plot(acp(),choix="ind",axes= c(1,2))
    
  )
  output$ifm<-renderPlot(ifm())
} 

shinyApp(ui = ui, server = server)