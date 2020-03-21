# I - Librerias -----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(dashboardthemes)
library(leaflet)

library(plotly)
library(tidyquant)
library(tidyverse)
library(DT)
library(openxlsx)
library(RUMBA)
library(sf)

comunas <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/comunas.csv') %>% 
    rename("comuna"=comunas)

ui <- navbarPage(
    title = 'Proyecto CABA',
    windowTitle = 'Proyecto CABA',
    inverse = FALSE,
    collapsible = TRUE,
    
    tabPanel('asd',
             column(3,
                    textInput(
                        inputId = 'direccion_input',
                        label = 'Direccion:',
                        value = ''
                    )),
             
             column(9,
                    textOutput('texto'),
                    verbatimTextOutput('texto2'),
                    dataTableOutput('tabla'),
                    leafletOutput('leaflet'))
    )
)

server <- function(input, output, session) {
    
    output$texto <- renderText(input$direccion_input)
    output$texto2 <- renderText(input$direccion_input)
    
    
    tabla_clientes <- reactive({
        USIG_geocode(input$direccion_input)
    })
    
    output$tabla <- renderDataTable({
        tabla_clientes()
    })
    
    output$leaflet <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            setView(lng = tabla_clientes()$lon, lat = tabla_clientes()$lat, zoom = 11.5) %>% 
            
    })
}

shinyApp(ui = ui, server = server)

