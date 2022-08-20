library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinythemes)



goles <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-08/Postwork/match.data.csv", header=T)
goles.expandido <- read.csv("match.expanded.data.csv")

ui <- fluidPage(
  
  dashboardPage(
    
    dashboardHeader(title = "Proyecto final BEDU"),
    
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
                  p("La primera gráfica de barras muestra la probabilidad (marginal) de que el equipo que juega en casa anote “x” goles (x=0,1,2,). En este caso, la probabilidad más alta es que el equipo local anote 1 gol."),
                  h3("Probabilidades para la cantidad de goles fuera"),
                  img(src ="frel-equipovisitante.png", width = 550, height = 350),
                  p("La segunda gráfica de barras muestra la probabilidad (marginal) de que el equipo que juega como visitante anote “y” goles (y=0,1,2,). En este caso, la probabilidad más alta es que el equipo visitante no anote."),
                  h3("Heatmap de probabilidades conjuntas"),
                  img(src ="probconjestimadas.png", width = 550, height = 350),
                  p("La tercera gráfica es un HeatMap, que muestra la probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,). En este último caso, lo más probable es que en un partido el equipo local anote 1 gol y el equipo visitante no anote.")
                )),
        
        tabItem(tabName = "data_table", 
                fluidRow(
                  dataTableOutput("datatable")
                )),
        
        tabItem(tabName = "graph",
                fluidRow(
                  h3("Factores de ganancia máximos"),
                  img(src ="escenarioMomiosMax.jpeg", width = 450, height = 450),
                  h3("Factores de ganancia promedio"),
                  img(src ="escenarioMomiosPro.jpeg", width = 450, height = 450),
                  h4("Predicción de ganancias y pérdidas de capital apostando a los momios promedio y máximos durante 600 juegos"),
                  p("Comparando ambas gráficas, se puede ver que con un capital inicial de $50000.00 y seleccionado los “momios promedio”, hay pérdidas durante los primeros 250 juegos"),
                  p("En cambio seleccionando los “momios máximos”, aunque también hay pérdidas durante los primeros 250 juegos, alrededor de los 400 juegos se logra recuperar y aumentar en un 10% el capital inicial, aún más, un poco antes de los 600 juegos se alcanza un máximo de ganancias del alrededor del 20%")
                )),
        
        tabItem(tabName="hypothesis",
                fluidRow(
                  p("En promedio los equipos anotan más goles cuando juegan local que cuando juegan de visitante. Es decir, el promedio de los goles de local es mayor al promedio de los goles de visitante"),
                  h5("H0: home_goals <= away_goals --> home_goals - away_goals <= 0"),
                  h5("Ha: home_goals > away_goals --> home_goals - away_goals > 0"),
                  selectInput("confianza", "Selecciona el nivel de confianza", choices=c(90, 95, 99)),
                  plotOutput("distribucion"),
                  h3("Conclusiones"),
                  textOutput("prueba")
                  
                )
        ),
        
        tabItem(tabName = "regression",
                fluidRow(
                  tabsetPanel(
                    
                    tabPanel("Correlación", 
                             selectInput("teamr", "Selecciona un equipo", choices=unique(goles$home.team)),
                             h3("Correlación entre las variables"),
                             plotOutput("correlation")
                    ),
                    
                    tabPanel("Modelo", 
                             h4("Selecciona las variables independientes del modelo"),
                             checkboxInput("away.score", "Away score", value=TRUE),
                             checkboxInput("home.halftime.score", "Home halftime score"),
                             checkboxInput("away.halftime.score", "Away halftime score"),
                             checkboxInput("halftime.result", "Hafltime result"),
                             h3("Resumen del modelo de regresión"),
                             verbatimTextOutput("Regmodel")
                    ),
                    tabPanel("Pruebas de supuestos",
                             h3("Ajuste de la regresión"),
                             plotOutput("ajust"),
                             h3("Matriz de dispersión de las variables"),
                             plotOutput("an.res"),
                             h3("Normalidad de los residuos"),
                             plotOutput("normal.res")
                    )
                  )
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
                  tabPanel("Predicción", 
                           fluidRow(
                             h3("Predicción mediante un modelo ARIMA estacional"),
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
  
  output$prueba <- renderText({
    home.mean <- mean(goles$home.score)
    away.mean <- mean(goles$away.score)
    
    home.var <- var(goles$home.score)
    away.var <- var(goles$away.score)
    
    home.n <- length(goles$home.score)
    away.n <- length(goles$away.score)
    
    z0 <<- (home.mean - away.mean) / sqrt((home.var/home.n) + (away.var/away.n))
    
    conf <- as.numeric(input$confianza)/100
    z.a <- qnorm(1-conf, lower.tail = F)
    
    paste("El estadístico de prueba z0 ", 
          ifelse(z0 > z.a, 
                 "cae en la región de rechazo, se rechaza H0 a favor de Ha. El promedio de los goles de local es mayor que el promedio de los goles de visitante.", 
                 "no cae en la región de rechazo, se falla en rechazar H0. El promedio de los goles de local es menor o igual que el promedio de los goles de visitante."))
  })
  
  output$distribucion <- renderPlot({
    x <- seq(-4, 16, 0.01)
    
    y <- dnorm(x)
    
    conf <- as.numeric(input$confianza)/100
    z.a <- qnorm(1-conf, lower.tail = F)
    
    plot(x, y, type = "l", xlab="", ylab="")
    title(main = "Densidad normal estándar")
    polygon(c(z.a, x[x>=z.a], max(x)), c(0, y[x>=z.a], 0), col="red")
    axis(side = 1, at = z.a, font = 2, padj = 1, lwd = 2)
    text(2.5, 0.1, labels = expression(alpha == 1-conf), col = "red")
    points(z0, 0, col = "red", pch = 19)
    text(z0 - 0.5, 0.1, labels = "Estadístico Z", col = "red")
    
  })
  
  output$correlation <- renderPlot({
    team_home <- goles.expandido %>% 
      filter(home.team == input$teamr) 
    
    team_away <- goles.expandido %>% 
      filter(away.team == input$teamr) 
    
    team <- rbind(team_home, team_away)
    team_expanded <<- mutate(team, halftime.result = factor(halftime.result))
    
    pairs(~ home.score + away.score + home.halftime.score + away.halftime.score + halftime.result, data = team_expanded, gap = 0.4, cex.labels = 1.5)
    
  }
  )
  
  output$Regmodel <- renderPrint({
    
    attach(team_expanded)
    input$teamr
    
    model <- lm(home.score ~ away.score)
    
    if(!input$away.score){
      model <- update(model, .~.-away.score)
    }
    
    if(input$home.halftime.score){
      model <- update(model, .~. + home.halftime.score)
    }
    
    if(input$away.halftime.score){
      model <- update(model, .~. + away.halftime.score)
    }
    
    if(input$halftime.result){
      model <- update(model, .~. + halftime.result)
    }
    
    regression.model <<- model
    
    summary(model)
    
  })
  
  output$ajust <- renderPlot({
    attach(team_expanded)
    input$teamr
    input$away.score
    input$home.halftime.score
    input$away.halftime.score
    input$halftime.result
    plot(regression.model$fitted.values, home.score, xlab = "Valores ajustados", ylab = "home.score")
    abline(lsfit(regression.model$fitted.values, home.score))
  })
  
  output$an.res <- renderPlot({
    attach(team_expanded)
    input$teamr
    input$away.score
    input$home.halftime.score
    input$away.halftime.score
    input$halftime.result
    StanRes <- rstandard(regression.model)
    par(mfrow = c(2, 2))
    plot(away.score, StanRes, ylab = "Residuales Estandarizados")
    abline(lsfit(away.score, StanRes))
    plot(home.halftime.score, StanRes, ylab = "Residuales Estandarizados")
    abline(lsfit(home.halftime.score, StanRes))
    plot(away.halftime.score, StanRes, ylab = "Residuales Estandarizados")
    abline(lsfit(away.halftime.score, StanRes))
    plot(halftime.result, StanRes, xlab="halftime.result", ylab = "Residuales Estandarizados")
    #dev.off()
  })
  
  output$normal.res <- renderPlot({
    attach(team_expanded)
    input$teamr
    input$away.score
    input$home.halftime.score
    input$away.halftime.score
    input$halftime.result
    StanRes <- rstandard(regression.model)
    qqnorm(StanRes)
    qqline(StanRes)
  })
  
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
    input$team
    acf(as.numeric(serie_team))
  })
  
  output$pacf <- renderPlot({
    input$team
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




