#
library(shiny)
library(shinydashboard)
library(shinythemes)


setwd("C:/Users/52722/Desktop/BEDU/II. PROGRAMACION Y ESTADISTICA CON R")
match.data <- read.csv("match.data.csv")

ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "PostWork 8"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Gráfica de barras", tabName = "grafica", icon = icon("area-chart")),
                    menuItem("Gráficas PW3", tabName = "pw3", icon = icon("file-picture-o")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                    menuItem("Factores de ganancia", tabName = "fact", icon = icon("file-picture-o"))
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Gráfica de barras
                    tabItem(tabName = "grafica",
                            
                            fluidRow(
                                titlePanel("Gráfica de goles de la Liga de Fútbl Española"), 
                                selectInput("x", "Seleccione el valor de X",
                                            choices = c("home.score", "away.score")),
                                box(plotOutput("plot1", height = 500)),
                                box(
                                    title = "Controls",
                                    sliderInput("bins", "Número de observaciones:", 1, 8, 8)
                                )
                                
                                
                            )
                            
                    ),
                    
                    # Gráficas PW3
                    tabItem(tabName = "pw3", 
                            fluidRow(
                                titlePanel(h3("Gráficas de Probabilidades")),
                                img( src = "pmel.png", 
                                     height = 350, width = 350),
                                img( src = "pmev.png", 
                                     height = 350, width = 350),
                                img( src = "pcon.png", 
                                     height = 350, width = 350)
                                
                            )
                    ),
                    
                    #Data Table
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table de Match.data")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    #Gráficas de factores de ganancia
                    tabItem(tabName = "fact",
                            fluidRow(
                                titlePanel(h3("Gráficas de factores de ganacia")),
                                img( src = "mM.png", 
                                     height = 350, width = 350),
                                img( src = "mP.png", 
                                     height = 350, width = 350)
                            )
                    )
                    
                )
            )
        )
    )


server <- function(input, output) {
    library(ggplot2)

    
    # Gráfica de barras
    output$plot1 <- renderPlot({
        
        x <- match.data[,input$x]
        bin <- seq(min(x), max(x), length.out = input$bins + 1)
        
        ggplot(match.data, aes(x)) + 
            geom_histogram( breaks = bin) +
            labs( xlim = c(0, max(x))) + 
            theme_light() + 
            xlab(input$x) + ylab("Frecuencia") + 
            facet_wrap(~away.team)
        
    })
    
    #Data Table
    output$data_table <- renderDataTable( {match.data}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )
    
}


shinyApp(ui, server)