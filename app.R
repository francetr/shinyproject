library(shiny)
library(FactoMineR)
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
      column(12,
             h2("Choix des stations et paramètres"),
             column(4,
                    checkboxGroupInput(inputId = "stations", label = h4("Choix des stations"), choices=levels(annee$NOM_SITE),select=levels(annee$NOM_SITE))
                    , hr(), p("Vous avez sélectionné les stations : "), verbatimTextOutput("stations")
                    
             ),
             column(4,
                    h3("Représentation géographique des stations sélectionnées")
             ),
             column(4, helpText("Deux paramètres doivent être sélectionnés"),
                    checkboxGroupInput(inputId = "parametres", label = h4("Choix des paramètres"), choices = tail(colnames(annee),-1),select = tail(colnames(annee),-1)) # tail permet d'enlever un paramètre inutile (le nom du site)
                    , p("Vous avez sélectionné les paramètres suivants : "), verbatimTextOutput("parametres")
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
        column(12
               )
        )
      )
)

#--------- Server part
server <- function(input, output){
  data_annee<-reactive(
    return(annee) # uses to display the dataframe of the data
  )
  output$annee <- renderDataTable({data_annee()})
  
  choix_stations<-reactive(
    {return(input$stations)}
  )
  choix_parametres<-reactive(
    {return(input$parametres)}
  )
  
  output$stations <- output$stations2 <- output$stations3 <-renderText({choix_stations()}) # fonction pour afficher les stations sélectionnées
  output$parametres <- output$parametres2 <- output$parametres3 <- renderText({choix_parametres()}) # fonction pour afficher les paramètres sélectionnés
  
  }
shinyApp(ui = ui, server = server)