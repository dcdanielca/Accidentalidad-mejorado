library(shiny)
library(rgdal)
library(leaflet)
library(raster)
library(rgdal)
library(car)
library(lubridate)
library(DescTools)
library(pROC)

shinyServer(function(input, output) {
  
  ##Zona de definiciones###
  
  #Carga de base de datos recodificada y modelo desde .Rdata
  load(".RData")
  # Función para recargar Bases de datos de acuerdo al año 
  cargarBaseDeDatos <- reactive({
    switch(input$year,
           "2015" = accidentalidad.15,
           "2016" = accidentalidad.16,
           "2017" = accidentalidad.17
    )  
  })
  
  cargarBaseDeDatos2 <- reactive({
    accidentalidad2.17
  })  
  
  ### FIN Zona de definiciones###
  
  # Carga de nombres de las Zonas 
  output$zonas <- renderUI({
    
    # Recarga de base de datos
    accidentalidad <- cargarBaseDeDatos()
    
    # Se muestran los nombres de las comunas cuando se eligen en el filtro "Zona"
    if(input$filtro == 'Zona'){
      selectInput("nombreZona", "Zonas",
                  choices = c(sort(unique(accidentalidad@data$COMUNA)))
      )
    }
  })
  
  # Carga de nombres de accidentes 
  output$tipoAccidente <- renderUI({
    
    # Recarga de base de datos
    accidentalidad <- cargarBaseDeDatos()
    
    # Se muestran los nombres de los accidentes cuando se eligen en el filtro "Accidente"
    if(input$filtro == 'Tipo de Accidente'){
      selectInput("nombreAccidente", "Accidentes",
                  choices = c(as.character(sort(unique(accidentalidad@data$CLASE))))
      )
    }
  })
  
  # Carga de nombres de los clases
  output$clases <- renderUI({
    # Recarga de base de datos
    accidentalidad2 <- cargarBaseDeDatos2()
    
    selectInput("nombreClases", "Clase",
                choices = c(as.character(sort(unique(accidentalidad2@data$CLASE))))
    )
  })
  
  # Carga de nombres de los dias
  output$dias <- renderUI({
    # Recarga de base de datos
    accidentalidad2 <- cargarBaseDeDatos2()
    
    # Se muestran los nombres de los diseños cuando se eligen en el filtro2  "Diseño"
    selectInput("nombreDias", "Día",
                choices = c(as.character(sort(unique(accidentalidad2@data$DIA))))
    )
  })
  
  # Carga de nombres de diseños
  output$diseños <- renderUI({
    # Recarga de base de datos
    accidentalidad2 <- cargarBaseDeDatos2()
    
    # Se muestran los nombres de los diseños cuando se eligen en el filtro2  "Diseño"
    selectInput("nombreDiseños", "Diseño",
                choices = c(as.character(sort(unique(accidentalidad2@data$DISENO))))
    )
  })
  
  # Carga de nombres de barrios
  output$barrios<- renderUI({
    # Recarga de base de datos
    accidentalidad2 <- cargarBaseDeDatos2()
    
    selectInput("nombreBarrios", "Barrio",
                choices = c(as.character(sort(unique(accidentalidad2@data$BARRIO))))
    )
  })
  
  # Carga de nombres de clases
  output$clases<- renderUI({
    # Recarga de base de datos
    accidentalidad2 <- cargarBaseDeDatos2()
    
    selectInput("nombreClases", "Clase",
                choices = c(as.character(sort(unique(accidentalidad2@data$CLASE))))
    )
  })
  
  # Carga del mapa
  output$map <- renderLeaflet({
    
    # Recarga de base de datos
    accidentalidad <- cargarBaseDeDatos()
    
    # Se hace una busqueda en la Base de datos según los filtros escogidos
    switch(input$filtro,
           "Zona" = {if(is.null(input$nombreZona)){ #Al inicio cuando se carga la página el input es nulo
             
           }else if(input$nombreZona == "NA"){ 
             accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
           }else if(input$nombreZona == "0"){
             accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == 0)
           }else{
             accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == input$nombreZona)
           }
             select <- input$nombreZona},
           "Tipo de Accidente" = {if(is.null(input$nombreAccidente)){ #Al inicio cuando se carga la página el input es nulo
             
           }else if(input$nombreAccidente == "NA"){
             accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
           }else {
             accidentalidad <- subset(accidentalidad, accidentalidad@data$CLASE == input$nombreAccidente)
           }
             select <- input$nombreAccidente},
           "Hora" = { # Se obtienen las dos horas del slider hourRange
             Inicio <- input$hoursRange[1] 
             Fin <- input$hoursRange[2]  
             
             # Como las horas no están como numéricas, se convierten a una unidad de tiempo (Horas)
             # En la posición 9 a 10 está la hora, en la posición 12 a 13 están los minutos,
             accidentalidad@data$HORA <- as.double(substr(accidentalidad@data$HORA,9,10)) + as.double(substr(accidentalidad@data$HORA,12,13))/60
             
             # Se hace una busqueda en la Base de datos según el rango de horas escogido
             accidentalidad <- subset(accidentalidad, accidentalidad@data$HORA >= Inicio & accidentalidad@data$HORA < Fin)}
    )
    
    # Cuando no hay nada seleccionado no hay que cargar mapa aún, cuando se coge filtro Hora no se carga ningun select
    # de algún input
    if(!is.null(select)){
      
      popup<-paste(accidentalidad@data$BARRIO)
      
      m<-leaflet()
      
      m<-fitBounds(m,
                   lng1=min(accidentalidad@coords[,1]), 
                   lat1=min(accidentalidad@coords[,2]), 
                   lng2=max(accidentalidad@coords[,1]),
                   lat2=max(accidentalidad@coords[,2]))
      m<-addProviderTiles(m,provider="OpenStreetMap.Mapnik")
      if(length(accidentalidad) > 1000){
        radio = 2
      } else{
        radio= 4
      }
      m<-addCircleMarkers(m,
                          lng = accidentalidad@coords[,1],
                          lat = accidentalidad@coords[,2],
                          popup = popup, 
                          radius = radio, 
                          stroke = FALSE,
                          fillOpacity = 0.75
      )
      m <- setView(m, mean(accidentalidad@coords[,1]), mean(accidentalidad@coords[,2]), zoom=14)
      m
    }
  })
  
  
  output$plot <- renderPlot({
    
    if(!is.null(input$Hora)){
      
      hora <- {if(input$Hora == 0 || input$Hora == 24){
        paste(12,":00 AM")
      } else if(input$Hora < 12){
        paste(input$Hora, ":00 AM")
      } else if(input$Hora == 12){
        paste(input$Hora, ":00 PM")
      } else
        paste(input$Hora-12,":00 PM")
      }
      
      hora <- parse_date_time(hora, '%I:%M %p') 
      
      log.odds <- predict(mod1,data.frame(HORA=hora,BARRIO=input$nombreBarrios,DISENO=input$nombreDiseños, 
                                         CLASE=input$nombreClases))
      round(1/(exp(-log.odds)+1),4)
      
      prob <- round(1/(exp(-log.odds)+1),4)
      
      accidentalidad2 <- cargarBaseDeDatos2()
      if(input$nombreBarrios ==0){
        tamaño <- length(subset(accidentalidad2,accidentalidad2@data$BARRIO == 0))
      } else {
        tamaño <- length(subset(accidentalidad2,accidentalidad2@data$BARRIO == input$nombreBarrios))
      }
      
      plot(c(0,20),c(0,20), type = "n", axes = FALSE,
           ylab = "", xlab = "", asp = 1)
      text(3,10, paste("Descripción: \n El modelo utiliza los datos más actuales (2017) \n . Para realizar la inferencia se 
      hace uso de un modelo logit", "\n", "El número de accidentes en este barrio en el año 2017 fue de: \n" , tamaño, "\n",
                       "\n Probabilidad de lesión o muerte en accidente: \n"), cex = 1.2, col="black")
      text(3,10,paste("\n \n \n \n \n \n ",prob*100, "%"), cex = 2 , col="black")
    }
  })
})