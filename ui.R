##Incio del UI

library(shiny)
library(leaflet)
library(shinyTime)

shinyUI(
  
  # Construcción de interfaz
  navbarPage("MAPAS",
             # Estadística Descriptiva: Tab para visualizar los mapas
             tabPanel(title= "Filtrado",
                      div(class="outer",
                          tags$head(
                            
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          # Mapa
                          leafletOutput("map", width="100%", height="100%"),
                          # Controles para los diferentes filtros
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        h3("Controles", class="text-center"),
                                        # Por año
                                        selectInput(inputId="year", label="Año", choices= c("2015", "2016", "2017")),
                                        # Por filtro (Zona, Hora o Tipo de Accidente)
                                        selectInput(inputId="filtro", label="Filtro", choices= c( "Zona", "Hora", "Tipo de Accidente")),
                                        # Si se escoge Hora, cargar slider con rango de horas
                                        conditionalPanel("input.filtro == 'Hora'",
                                                         sliderInput("hoursRange", "Rango de horas ",
                                                                     min = 0, max = 24, value = c(10,11)),
                                                         hr(),
                                                         fluidRow(column(3, verbatimTextOutput("value")))
                                        ),
                                        # Si se escoge Zona, cargar selectInput con nombres de Zonas
                                        conditionalPanel("input.filtro == 'Zona'",
                                                         uiOutput("zonas")
                                        ),
                                        # Si se escoge Tipo de Accidente, cargar selectInput con nombres de Accidentes
                                        conditionalPanel("input.filtro == 'Tipo de Accidente'",
                                                         uiOutput("tipoAccidente")
                                        )
                          )
                      )
             ),
             tabPanel(title= "Modelo de inferencia",
                      div(class="outer",
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          textOutput("texto"),
                          # Controles para los diferentes filtros
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        h3("Controles", class="text-center"),
                                        # Por Hora
                                        sliderInput("Hora", "Hora de interés", min = 0 , max = 24, value = 8),
                                        # Por Clase
                                        uiOutput("clases"),
                                        # Por Diseño
                                        uiOutput("diseños"),
                                        # Por Barrio
                                        uiOutput("barrios")
                          ),
                          plotOutput("plot")
                      )
             )
  )
)