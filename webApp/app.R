library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinythemes)


goles <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-08/Postwork/match.data.csv", header=T)

ui <- fluidPage(
  
  dashboardPage(
    
    dashboardHeader(title = "Basic dashboard"),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Gráficas", tabName="Dashboard", icon=icon("chart-column")),
        menuItem("Imágenes", tabName="img", icon=icon("file-picture-o")),
        menuItem("Data table", tabName="data_table", icon=icon("table")),
        menuItem("Factores de ganancia", tabName="graph", icon=icon("chart-line")),
        menuItem("Prueba de hipótesis", tabName='hypothesis', icon=icon("magnifying-glass")),
        menuItem("Regresión lineal", tabName="regression", icon=icon("chart-line")),
        menuItem("Serie de tiempo", tabName="time_series", icon=icon("clock"))
      )
      
    ),
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "Dashboard", 
                fluidRow(
                  titlePanel("Gráficas de goles por equipo"),
                  selectInput("x", "Selecciona el valor de las X",
                              choices =  c("home.score", "away.score")),
                  plotOutput("output_plot") 
                )),
        
        tabItem(tabName = "img",
                fluidRow(
                  h3("Probabilidades para la cantidad de goles en casa"),
                  img(src ="frel-equipolocal.png", width = 550, height = 350),
                  h3("Probabilidades para la cantidad de goles fuera"),
                  img(src ="frel-equipovisitante.png", width = 550, height = 350),
                  h3("Heatmap de probabilidades conjuntas"),
                  img(src ="probconjestimadas.png", width = 550, height = 350),
                )),
        
        tabItem(tabName = "data_table", 
                fluidRow(
                  dataTableOutput("datatable")
                )),
        
        tabItem(tabName = "graph",
                fluidRow(
                  h3("Factores de ganancia máximos"),
                  img(src ="escenarioMomiosMax.png", width = 450, height = 450),
                  h3("Factores de ganancia promedio"),
                  img(src ="escenarioMomiosPro.png", width = 450, height = 450)
                )),
        
        tabItem(tabName="time_series",
                tabsetPanel(
                  tabPanel("Serie de tiempo", 
                           fluidRow(
                             selectInput("team", "Selecciona el equipo",
                                         choices =  unique(goles$home.team)),
                             h3("Serie de tiempo"),
                             plotOutput("timeseries"),
                             h3("Gráfica de autocorrelación"),
                             plotOutput("acf"),
                             h3("Gráfica de autocorrelación parcial"),
                             plotOutput("pacf")
                           )
                  ),
                  tabPanel("Prediccion", 
                           fluidRow(
                             h3("Prediccion mediante un modelo ARIMA estacional"),
                             selectInput("periods", "Selecciona el número de periodos a predecir",
                                         choices = c(5, 10, 15)),
                             numericInput("ar", "Orden del término autorregresivo: ", 1, min=0, max=5),
                             numericInput("i", "Integrada de orden: ", 0, min=0, max=5),
                             numericInput("ma", "Orden del término de media móvil:", 1, min=0, max=5),
                             numericInput("sar", "Orden estacional autorregresivo: ", 0, min=0, max=5),
                             numericInput("si", "Orden estacional integrado: ", 0, min=0, max=5),
                             numericInput("sma", "Orden estacional de media móvil: ", 0, min=0, max=5),
                             plotOutput("predict.time.series")
                           )
                           
                  )
                )
        )
        
      )
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$output_plot <- renderPlot({ #plots
    
    goles <- mutate(goles, ganador=ifelse(home.score > away.score, "Home", ifelse(home.score < away.score, "Away", "Empate")))
    x <- goles[, input$x]
    
    ggplot(goles, aes(x, fill=ganador)) +
      geom_bar() +
      facet_wrap("away.team") +
      labs(x=input$x, y="Cantidad de goles")
  })
  
  
  output$datatable <- renderDataTable( {goles},       #Data table
                                       options = list(aLengthMenu = c(10,20,50), iDisplayLength = 10) )
  
  
  output$timeseries <- renderPlot({
    goles <- mutate(goles, date = as.Date(date, "%Y-%m-%d"))
    
    team_home <- goles %>% 
      select(date, home.team, home.score) %>% 
      filter(home.team == input$team) %>% 
      rename(team = home.team, score = home.score)
    
    team_away <- goles %>% 
      select(date, away.team, away.score) %>% 
      filter(away.team == input$team) %>% 
      rename(team = away.team, score = away.score)
    
    team <- rbind(team_home, team_away)
    
    team <- mutate(team, date=format(date, "%Y-%m"))
    prom_team <- team %>% group_by(date) %>% summarize(goles=mean(score))
    
    serie_team <<- ts(prom_team$goles, start=c(2017, 8), end=c(2020, 7), frequency=12)
    ts.plot(serie_team, main="", xlab="", ylab="")
    title(main=paste("Goles promedio de ", input$team),
          xlab="Tiempo",
          ylab="Goles promedio")
  })
  
  output$acf <- renderPlot({
    acf(as.numeric(serie_team))
  })
  
  output$pacf <- renderPlot({
    pacf(as.numeric(serie_team))
  })
  
  output$predict.time.series <- renderPlot({
    model <- arima(serie_team, order=c(input$ar, input$i, input$ma), seasonal=list(order=c(input$sar, input$i, input$ma)))
    p <- predict(model, input$periods)$pred
    ts.plot(cbind(serie_team, p), col=c("blue", "red"), main="", xlab="", ylab="")
    title(main=paste("Goles promedio de ", input$team),
          xlab="Tiempo",
          ylab="Goles promedio")
    
  })
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)





