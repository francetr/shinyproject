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
  data_annee<-reactive({
    # annee$Annee<-as.Date(as.character(paste(annee$Annee, "01-01", sep = "-")))
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
  observeEvent(input$allParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-1), selected = tail(colnames(annee),-1))})
  observeEvent(input$noParametres, {updateCheckboxGroupInput(session, "parametres", label = "Choix des parametres", choices=tail(colnames(annee),-1))})
  
  #---- Reactive objects of the stations/parametres selected by user
  choix_stations<-choix_stations2<-reactive(
    {return(input$stations)}
  )
  choix_parametres<-choix_parametres2<-reactive(
    {return(input$parametres)}
  )
  
  output$stations <- renderText({choix_stations()}) # display the selected stations
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
  # - annee : displayed to know the years seelcted
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
  
  #--- data whitout NA n the date range selected
  data_sans_na<-reactive(
    subset(data_annee(), NOM_SITE %in% choix_stations() & as.Date(as.character(choix_date_start())) <= as.Date(as.character(choix_date_end())) & as.Date(choix_date_start()) <= as.Date(as.character(paste(Annee, "12-31", sep = "-"))) & as.Date(choix_date_end()) >= as.Date(paste(Annee, "01-01", sep = "-")) , select = choix_parametres())
  )
  output$data_sans_na<-renderDataTable(data_sans_na())
  
  #--- ACP result
  
  acp<-reactive(
    PCA(scale(data_sans_na()), graph=FALSE)
  )
  
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
)
