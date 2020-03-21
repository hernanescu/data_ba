# I - Librerias -----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(dashboardthemes)

library(plotly)
library(tidyquant)
library(tidyverse)
library(DT)
library(openxlsx)

ui <- navbarPage(
    title = 'Proyecto CABA',
    windowTitle = 'Proyecto CABA',
    inverse = FALSE,
    collapsible = TRUE,
    
    #theme = shinytheme('superhero'),
    
    tabPanel(
        'Clientes',
        shinyjs::useShinyjs(),
        
        ###Header de la página ----
        div(
            class = "container",
            id = "header",
            h1(class = "page-header", "Próxima Mejor Oferta", tags$small("por Corebi - Banco Ciudad")),
            p(class = "lead", "Shiny App desarrollada para visualizar información relativa al proyecto de Próxima Mejor Oferta.")
        ),
        
        # Display ----
        # div(
        #     class = "container hidden-sm hidden-xs",
        #     id = "favorite_container",
        #     
        #     div(
        #         class = "",
        #         column(
        #             width = 12,
        #             h4(class = "pull-left", "Resumen"),
        #             actionButton(inputId = "favorites_toggle", "Mostrar/Ocultar", class = "pull-right")
        #         )
        #     ),
        #     div(
        #         class = "row",
        #         id = "favorite_card_section",
        #         uiOutput(outputId = "favorite_cards", class = "container")
        #     )
        # ),
        # 
        ### body ----
        div(
            class = "container",
            id = "application_ui",
            column(
                width = 3, 
                wellPanel(
                    div(id = 'filtros_panel',
                        h3(strong('Filtros'))
                    ),
                    hr(),
                    div(
                        id = "input_main",
                        div(
                            id = 'probabilidad_titulo',
                            actionButton(inputId = 'probabilidad_btn', 
                                         label = 'Probabilidad', 
                                         icon = icon('cog', class = 'fa-1x'),
                                         class = 'btn-primary btn-medium')
                        ),
                        hr(),
                        div(id = 'direccion',
                            textInput(
                                inputId = 'direccion_input',
                                label = 'Direccion:',
                                value = 'Dirección valida en CABA'
                            )
                        )# %>% hidden()
                        
                        
                    ),
                    div(
                        id = "input_buttons",
                        actionButton(inputId = "map_plot", label = "Plotear", icon = icon("angle-double-right"), class = 'btn-info')
                    )
                )
            ),
            column(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        'Mapa CABA',
                        br(),
                        div(
                            class = "panel", 
                            div(
                                class = "panel-body",
                                leafletOutput(
                                    outputId = 'mapaBA',
                                    height = 800
                                )
                            )
                        )
                    )
                   
                )
                
            )
            
            
        )
    )
)

server <- function(input, output, session) {
    
    direccion_user <- eventReactive(input$map_plot, {
        verbatimTextOutput('direccion_input')
    })
    
    tabla_direcciones <- observeEvent(input$map_plot, {
        USIG_geocode(direccion_user())
    })
    
    output$mapaBA <- renderLeaflet({
        
        leaflet() %>% 
            addTiles() %>% 
            setView(lng = tabla_direcciones()$lon, lat = tabla_direcciones()$lat, zoom = 11.5)
        
    })
    
}

shinyApp(ui = ui, server = server)
