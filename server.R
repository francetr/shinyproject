#######################
# Authors:
# Tristan FRANCES
# Antoine KOURMANALIEVA
# Mercia NGOMA KOMB
#######################

library(shiny)
library(FactoMineR)
library(lattice)

setwd("~/Bureau/m2/s3/stat/project/script")
# mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
annee<-read.csv("bdd_annee_somlit.csv", header = T,  dec = ".")


#--------- Server part
shinyServer ( function(input, output, session){
  #--- Display raw data
  data_annee<-reactive(
    return(annee)
  )
  
  output$annee <- renderDataTable({data_annee()})
  
  #---- Button to select/deselect all the stations/parametres
  #---- For stations
  observeEvent(input$allStations, {updateCheckboxGroupInput(session, "stations", label = "Choix des stations", choices=levels(annee$NOM_SITE), selected = levels(annee$NOM_SITE) )})
  observeEvent(input$noStations, {updateCheckboxGroupInput(session, "stations", label = "Choix des stations", choices=levels(annee$NOM_SITE))})
  
  #---- For parametres
  observeEvent(input$allParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-1), selected = tail(colnames(annee),-1))})
  observeEvent(input$noParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-1))})
  
  #---- Reactive objects of the date/stations/parametres selected by user
  # TODO
  output$date_start<-renderText(as.character(min(annee$Annee)))
  output$date_end<-renderText(as.character(max(annee$Annee)))

  
  choix_date<-reactive(
    {return(input$date)}
  )
  
  
  
  
  choix_stations<-reactive(
    {return(input$stations)}
  )
  choix_parametres<-reactive(
    {return(input$parametres)}
  )

  #---- Display the stations/parametres selected by user
  output$stations <- renderText({choix_stations()}) # fonction pour afficher les stations sélectionnées
  output$parametres  <- renderText({choix_parametres()}) # fonction pour afficher les paramètres sélectionnés
  
  #---- data for the dataframe of NA(with the name of the stations)
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
  output$nb_na<- renderDataTable(nb_na())
  
  #--- data with parameters setted by user(don't contain the stations's name)
  new_data<-reactive(
    subset(new_data_with_station(), NOM_SITE %in% choix_stations(), select = choix_parametres())
  )
  output$new_data<-renderDataTable(new_data())
  
  #--- data whitout NA
  data_sans_na<-reactive(
    na.omit(subset(data_annee(), NOM_SITE %in% choix_stations(), select = choix_parametres()))
  )
  output$data_sans_na<-renderDataTable(data_sans_na())
  
  #--- ACP result
  
  vfm<-reactive(
    PCA(scale(data_sans_na())))
  output$vfm<-renderPlot(vfm())
  
  #--- ebouli
  
  ebouli<-reactive(
    barplot(vfm()$eig[,1], main="Ebouli des valeurs propres", xlab = "Composantes", ylab="Valeurs propres")
  )
  output$ebouli<-renderPlot(ebouli())
  
  #--- individual factor map
  
  ifm<-reactive(
    plot(vfm(),choix="ind",axes= c(1,2))
    
  )
  output$ifm<-renderPlot(ifm())
  
}
)
