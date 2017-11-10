library(shiny)
mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
annee<-read.csv("bdd_annee_somlit.csv", header = T,  dec = ".")

stations<-levels(annee$NOM_SITE)
parametres<-tail(colnames(annee),-1) # permet d'enlever le nom du site
parametres

#?source
#source("script_project.R")
ui <- fluidPage(
  # App title ----
  titlePanel("MySomlit"),
  # First column choices of parameters, stations and geographic representaion
  fluidRow(
    column(12,
           h2("Choix des stations et paramètres"),
           column(4,
                  checkboxGroupInput(inputId = "stations", label = h4("Choix des stations"), choices=stations)
           ),
           column(4,
                  checkboxGroupInput(inputId = "paramètres", label = h4("Choix des paramètres"), choices=parametres)
           ),
           column(4,
                  h3("Représentation géographique des stations")
           )
    )
  ),
  fluidRow(
    h2("Analyse multivariee"),
    column(12

           
           )
  ),
  fluidRow(
    h2("Représentations graphiques"),
    column(12

           )
  )
)
  
  

server <- function(input, output){
  
  
}

shinyApp(ui = ui, server = server)