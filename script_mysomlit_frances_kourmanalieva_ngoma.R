library(shiny)
library(FactoMineR)
library(lattice)
library(ade4)
library(factoextra)
annee<-read.csv("/home/kourmanalieva/Bureau/shinyproject-master/base_mysomlit.csv", header = T,  dec = ".")


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
                           , uiOutput("date")
                           , h4("Dates sélectionnées"),  textOutput("date_selection")
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
                    column(7,
                    	     h4("Cercle de corrélation"),
                           plotOutput("vfm")),
                 		       h5("Un cos2 élevé indique une bonne représentation de la variable sur les axes principaux en considération. Dans ce cas, la variable est positionnée à proximité de la circonférence du cercle de corrélation. Un faible cos2 indique que la variable n’est pas parfaitement représentée par les axes principaux. Dans ce cas, la variable est proche du centre du cercle."),
                    column(5,
                    	   h4("Graphique des individus"),
                           plotOutput("ifm")),
                    column(4,
                           plotOutput("ebouli")),
                    column(4,
                           h4("Contribution des variables sur le premier axe (top 10)"),
                           plotOutput("contribution_d1")),

                  	column(4,

                  	       h4("Contribution des variables sur le second axe (top 10)"),
                           plotOutput("contribution_d2")),

                  	column(8,
                  	       h4("Biplot des individus et des variables"),
                           plotOutput("biplot"))
                
                    )

                  )
           ),
  
  #------- Panel containing the graphic representations
  tabPanel("Représentations",
           column(12,
                  fluidRow(
                    h2("Représentations graphiques"),
                    column(4, helpText("Veuillez a séléctionner au minimum 2 paramètres et 1 station")
                           , uiOutput("parametres_checkbox2")
                           , actionButton(inputId = "allParametres2", label = "Select all")
                           , actionButton(inputId = "noParametres2", label = "Deselect all")
                           , p("Représentation graphique pour les paramètres suivants : "), hr(), verbatimTextOutput("parametres2")
                           , p("Pour les stations suivants : "), hr(), verbatimTextOutput("stations2")
                            ),
                    column(8
                           # , dataTableOutput("tab_graph")
                           , plotOutput("parametres_representation")
                           )
                    )
                  )
           )
  )

#--------- Server part
server <- function(input, output, session){
  #--- Display raw data
  data_annee<-reactive({
    return (annee)
  })
  
  output$annee <- renderDataTable({data_annee()})
  
  #---- Definition of the checkbox group that will be displayed in UI
  output$stations_checkbox <- renderUI(checkboxGroupInput(inputId = "stations", label = "Choix des stations", choices = levels(annee$NOM_SITE)))
  output$parametres_checkbox <- renderUI(checkboxGroupInput(inputId = "parametres", label = ("Choix des paramètres"), choices = tail(colnames(annee),-2)))
  
  #---- Button to select/deselect all the stations/parametres
  #---- For stations
  observeEvent(input$allStations, {updateCheckboxGroupInput(session, "stations", label = "Choix des stations", choices=levels(annee$NOM_SITE), selected = levels(annee$NOM_SITE) )})
  observeEvent(input$noStations, {updateCheckboxGroupInput(session, "stations", label = "Choix des stations", choices=levels(annee$NOM_SITE))})
  
  #---- For parametres
  observeEvent(input$allParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-2), selected = tail(colnames(annee),-2))})
  observeEvent(input$noParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-2))})
  
  #---- Reactive objects of the stations/parametres selected by user
  choix_stations<-reactive(
    {return(input$stations)}
  )
  choix_parametres<-reactive(
    {return(input$parametres)}
  )
  
  output$stations<- output$stations2 <- renderText({choix_stations()}) # display the selected stations
  output$parametres  <- renderText({choix_parametres()}) # display the selected parametres
  
  #---- Reactive objects of the date selected by user
  annee_min<-reactive(
    return(paste(min(annee$Annee), "01-01", sep = "-"))
           )
  annee_max<-reactive(
    return(paste(max(annee$Annee), "12-31", sep = "-"))
  )
  
  # Display the date range of available data 
  output$date_range <- renderText(paste("De", annee_min(), "à" , annee_max(), sep = " "))
  
  output$date<-renderUI(dateRangeInput(inputId = "date", label = "Choix de la date", startview = "year", format = "dd-mm-yyyy"
                                       , start = annee_min(), min= annee_min()
                                       , end = annee_max(), max = annee_max()))
  #--- 2 type of date : 
  # - choix_date : used to compare the start and end date range
  # - annee : displayed to know the years selected
  choix_date_start<-reactive(
    return(as.character(input$date[1]))
  )
  annee_start<-reactive(
    return(as.character(input$date[1], "%Y"))
  )
  choix_date_end<-reactive(
    return(as.character(input$date[2]))
  )
  annee_end<-reactive(
    return(as.character(input$date[2], "%Y"))
  )
  
  output$date_selection<-renderText(paste("De", annee_start(), "à", annee_end(), sep = " "))
  
  #---- data used for the construction of the dataframe of NA in the range of the chosen date
  # Dates conditions are : date start <= date end, year of date start <= year of study and year of date end >= year of study
  new_data_with_station<-reactive(
    subset(data_annee(), NOM_SITE %in% choix_stations() & as.Date(as.character(choix_date_start())) <= as.Date(as.character(choix_date_end())) & as.Date(choix_date_start()) <= as.Date(as.character(paste(Annee, "12-31", sep = "-"))) & as.Date(choix_date_end()) >= as.Date(paste(Annee, "01-01", sep = "-")) , select = c(choix_parametres(), "NOM_SITE"))
  )
  
  #--- data with parameters setted by user in the date range selected
  new_data<-reactive(
    subset(data_annee(), NOM_SITE %in% choix_stations() & as.Date(as.character(choix_date_start())) <= as.Date(as.character(choix_date_end())) & as.Date(choix_date_start()) <= as.Date(as.character(paste(Annee, "12-31", sep = "-"))) & as.Date(choix_date_end()) >= as.Date(paste(Annee, "01-01", sep = "-")) , select = choix_parametres())
  )
  output$new_data<-renderDataTable(new_data())
  
  #--- Calculus of the number of NA by col and creation of the dataframe
  nb_na<-reactive(
    #----- if data selected is not null (nrow(new_data())) stations(length(choix_stations)) and parametres(length(choix_parametres)) are selected
    if(length(choix_parametres())>=1 & length(choix_stations())>=1 & nrow(new_data()) > 0){
        na<-aggregate(new_data(), list(new_data_with_station()$NOM_SITE), function(x) sum(is.na(x)))
        colnames(na)<-c("NOM_SITE", choix_parametres()) # change the name of the first column
        return(na)
      }
  )
  output$nb_na<-renderDataTable(nb_na())
  
  #--- data whitout NA for the date range selected
  data_sans_na<-reactive(
    na.omit(subset(data_annee(), NOM_SITE %in% choix_stations() & as.Date(as.character(choix_date_start())) <= as.Date(as.character(choix_date_end())) & as.Date(choix_date_start()) <= as.Date(as.character(paste(Annee, "12-31", sep = "-"))) & as.Date(choix_date_end()) >= as.Date(paste(Annee, "01-01", sep = "-")) , select = c(choix_parametres())))
  )
  output$data_sans_na<-renderDataTable(data_sans_na())
  
  data_sans_na2<-reactive(
    na.omit(subset(data_annee(), NOM_SITE %in% choix_stations() & as.Date(as.character(choix_date_start())) <= as.Date(as.character(choix_date_end())) & as.Date(choix_date_start()) <= as.Date(as.character(paste(Annee, "12-31", sep = "-"))) & as.Date(choix_date_end()) >= as.Date(paste(Annee, "01-01", sep = "-")) , select = c(choix_parametres(), "Annee")))
  )
  
  #--- ACP result
  
  acp<-reactive(
      PCA(scale(data_sans_na()), graph=FALSE)
    )
  
  vfm<-reactive(
    # plot(acp(), choix="var",axes = c(1,2))
    #fviz_pca_var(acp(), col.var = "black")
    fviz_pca_var(acp(), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
             )
  )
  output$vfm<-renderPlot(vfm())
  
  #--- ebouli
  
  ebouli<-reactive(
  	barplot(acp()$eig[,1], main="Ebouli des valeurs propres", xlab = "Composantes", ylab="Valeurs propres"))
  output$ebouli<-renderPlot(ebouli())
  
  
  
  #--- individual factor map
  
  ifm<-reactive(
   fviz_pca_ind (acp(), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE # Évite le chevauchement de texte
             )
  )
  output$ifm<-renderPlot(ifm())
  
  data_class<-reactive({
    na.omit(subset(data_annee(), NOM_SITE %in% choix_stations() & as.Date(as.character(choix_date_start())) <= as.Date(as.character(choix_date_end())) & as.Date(choix_date_start()) <= as.Date(as.character(paste(Annee, "12-31", sep = "-"))) & as.Date(choix_date_end()) >= as.Date(paste(Annee, "01-01", sep = "-")) , select = c(choix_parametres(), "Annee", "NOM_SITE")))
  })
  #-----contribution
  contribution_d1<-reactive(
    fviz_contrib(acp(), choice = "var", axes = 1, top = 10)
  )
  output$contribution_d1<-renderPlot(contribution_d1())

 contribution_d2<-reactive(
    fviz_contrib(acp(), choice = "var", axes = 2, top = 10)
  )
  output$contribution_d2<-renderPlot(contribution_d2())

  biplot<-reactive(
    fviz_pca_biplot(acp(), 
                # Individus
                geom.ind = "point",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "stations", color = "Contribution",
                                    alpha = "Contribution")
                )
  )
  output$biplot<-renderPlot(biplot())
  
  
  
  
  
  class_s<-reactive(
    #print(data_class())
    s.class(acp()$li, fac=data_class()$NOM_SITE)
  )
  output$class_s<-renderPlot(class_s())
  
  #---- Graphical representation of environnemental variables
  #---- We take the same object used for the choices of parametres for the PCA 
  output$parametres_checkbox2 <- renderUI(checkboxGroupInput(inputId = "parametres2", label = ("Choix des paramètres pour la représentation graphique"), choices = choix_parametres() , selected = choix_parametres()))

  observeEvent(input$allParametres2, {updateCheckboxGroupInput(session, "parametres2", label = "Choix des parametres pour la représentation graphique", choices=choix_parametres(), selected = choix_parametres())})
  observeEvent(input$noParametres2, {updateCheckboxGroupInput(session, "parametres2", label = "Choix des parametres pour la représentation graphique", choices=choix_parametres())})
  
  #---- Display the parametres selected for graphical representation
  choix_parametres2<-reactive(
    {return(input$parametres2)}
  )
  output$parametres2  <- renderText({choix_parametres2()}) # display the selected parametres
  
  #---- data for graph representation with Annee
  data_graph_avec_annee<- reactive(
    subset(data_sans_na(), select = c(choix_parametres2(), "Annee"))
  )
  
  #---- data for graph representation without Annee
  data_graph<- reactive(
    subset(data_sans_na(), select = choix_parametres2())
  )
  
  
  output$tab_graph<-renderDataTable(data_graph_avec_annee())
  
  output$parametres_representation<-renderPlot({
    plot(data_graph_avec_annee()~data_graph_avec_annee()$Annee)
    })
} 

shinyApp(ui = ui, server = server)