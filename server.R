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
mois<-read.csv("bdd_mois_somlit.csv", header = T,  dec = ".")
annee<-read.csv("bdd_annee_somlit.csv", header = T,  dec = ".")


#--------- Server part
shinyServer ( function(input, output){
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
      na<-apply(new_data(), 2, function(x) sum(is.na(x)))
      df<-t(data.frame(na))
      rownames<-(choix_stations())
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
