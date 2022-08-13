library(shiny)

ui <- pageWithSidebar(
  
  headerPanel("Proyecto final módulo de R"),
  
  sidebarPanel(p("Vamos a crear plots con el dataset de 'iris'"),
               selectInput("x", "Selecciona el eje de las X",
                           choices = names(iris) )
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Gráficas", 
               h3(textOutput("output_text")), 
               plotOutput("output_plot") 
      ),
      
      tabPanel("Imágenes",
               img(src ="cor_iris.png", width = 450, height = 450)     
      ), 
      
      tabPanel("Data Table", dataTableOutput("datatable")),   # salida del data table
      
      tabPanel("Factores",
               img(src ="cor_iris.png", width = 450, height = 450)     
      )
      
      #tabPanel("Summary", verbatimTextOutput("summary")),    # salida del Summary
      #tabPanel("Table", tableOutput("table")),               # salida de la tabla
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$output_text <- renderText( {paste("Gráfico de Sepal.Length ~", input$x)}) #Titulo del main Panel
  
  output$output_plot <- renderPlot( {plot(as.formula(paste("Sepal.Length ~", input$x)),  #Gráficos de salida
                                          data = iris) })
  
  output$summary <- renderPrint( {summary(iris)} )   # Summary
  
  output$table <- renderTable({ data.frame(iris)})   # Data Frame
  
  output$datatable <- renderDataTable( {iris},       #Data table
                                       options = list(aLengthMenu = c(10,20,50), iDisplayLength = 10) ) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)





