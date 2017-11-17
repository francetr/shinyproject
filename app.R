library(shiny)
library(FactoMineR)
library(lattice)
setwd("~/Bureau/m2/s3/stat/project/script")
mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
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
             column(4, helpText("Au moins une station doit être sélectionnée"),
                    checkboxGroupInput(inputId = "stations", label = h4("Choix des stations"), choices=levels(annee$NOM_SITE)
                                       #,select=levels(annee$NOM_SITE)
                                       )
                    , hr(), p("Vous avez sélectionné les stations : "), verbatimTextOutput("stations")

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
        h2("Analyse multivariee"),
        column(12
               #,PCA(scale("data"), scale.unit = TRUE, graph= FALSE)
               )
      )
  ),
  #------- Panel containing the graphic representations
  tabPanel("Représentations",
      fluidRow(
        h2("Représentations graphiques"),
        column(12,
               plotOutput("plot_parametres")
               )
        )
      )
)

#--------- Server part
server <- function(input, output){
  #--- Display raw data
  data_annee<-reactive(
    return(annee)
  )
  output$annee <- renderDataTable({data_annee()})
  
  choix_stations<-reactive(
    {return(input$stations)}
  )
  choix_parametres<-reactive(
    {return(input$parametres)}
  )

  # na<-renderTable(
  #   if(is.null(input$choix_stations)){
  #     print("Entrer au moins une station")
  #   } else if(is.null(input$choix_parametres) || input$parametres<2){
  #     print("Entrer au moins deux paramètres")
  #   }
  # )
  
  output$stations <- output$stations2 <- output$stations3 <-renderText({choix_stations()}) # fonction pour afficher les stations sélectionnées
  output$parametres <- output$parametres2 <- output$parametres3 <- renderText({choix_parametres()}) # fonction pour afficher les paramètres sélectionnés

  #--- data with parameters setted by user
  new_data<-reactive(
    subset(data_annee(), NOM_SITE %in% choix_stations(), select = choix_parametres())
  )
  output$new_data<-renderDataTable(new_data())

  #--- Calculus of the number of NA by col
  nb_na<-reactive(
    #----- if stations and parametres are selected
    if(length(choix_parametres())>=1 && length(choix_stations())>=1){
      na<-apply(new_data(),2,function(x) sum(is.na(x)))
      df<-t(data.frame(na))
    return(df)
    }

  #   #--- if no stations and no parametres selected
  #   else if(length(choix_parametres())<1 && length(choix_stations())<1){
  #     "Sélectionner un nombre de paramètres et/ou de stations cohérent"
  #   }
  #   #----- if no parameter selected
  #   else if (length(choix_parametres())<1){
  #     "Sélectionner au moins 1 paramètres"}
  #   #---- if no stations selected
  #   else{
  #     "Sélectionner au moins 1 stations"}
  )
  
  output$nb_na<- renderDataTable(nb_na())
  
  #--- data whitout NA
  data_sans_na<-reactive(
    na.omit(subset(data_annee(), NOM_SITE %in% choix_stations(), select = choix_parametres()))
  )
  output$data_sans_na<-renderDataTable(data_sans_na())
}

shinyApp(ui = ui, server = server)