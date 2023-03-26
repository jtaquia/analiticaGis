#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
library(shiny)
library(shinythemes)
library(TSP)
library(leaflet)
library(dplyr)
library(ggplot2)
library(magrittr)
library(readr)

### aqui se puede leer un archivo excel con la data detallada de los puntos
### a analizar, el valor de "n" se obtendría del excell


####################################
# User interface                   #
####################################

# Define UI for application that draws a histogram
ui <- fluidPage( 
  
  shinythemes::themeSelector(),
  # Application title
  #titlePanel("Old Faithful Geyser Data"),
  #tags$head("Old Faithful Geyser Data" ),
  tags$h2("Analítica Gis"),
  theme = "cerulean", 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(position = "left",
                
                sidebarPanel(
                  sliderInput("bins","Número de Repartos:",min = 4,max = 20, value = 6),
                  hr(),
                  sliderInput("C_over","C_Over <> Margen:",min = 0.25,max = 1, value = 0.5)
                  
                  ,hr(),
                  sliderInput("C_under","C_under + C operativo:",min = 0.25,max = 1, value = 0.8)
                  
                  ,hr(),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary"),
                  
                  numericInput("obs2", "Observations:", 4, min = 1, max = 100), 
                  
                  sliderInput("integer", "Cantidad de puntos de entrega:",
                              min = 4, max = 20,
                              value = 6),
                  
                  # Built with Shiny by RStudio
                  br(), br(),
                  h5("Built with",
                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                     "by",
                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                     "."),
                  h5("Elaborado por José Antonio Taquía Gutiérrez")
                  
                ), #este signo cierra el sidebar Panel
                
                mainPanel(
                  
                  tabsetPanel(type = "tabs",
                              id = "tabsetpanel",
                              
                              tabPanel(title = "Plot", br(), br(),
                                       textOutput("selected_var"),
                                       plotOutput(outputId = "loess"),
                                       leafletOutput("newmap")
                              ),
                              tabPanel(title = "Data", 
                                       br(),br(),
                                       textOutput("selected_var2"),
                                       tableOutput("values2"),
                                       DT::dataTableOutput(outputId = "moviestable"))
                  )
                  
                )
                
                # Show a plot of the generated distribution
                
  )
)


####################################
# Server                           #
####################################

server <- function(input, output, session) { 
  
  output$value <- renderText({input$obs2 })
  
  output$selected_var2<-renderText({ 
    n<-4:20
    n<-data.frame(n)
    #lengths(n)
    R<-input$bins
    margen<-input$C_over
    margen_c<-input$C_under
    beneficio<- matrix(ncol=1, nrow=lengths(n))
    beneficio <- data.frame(beneficio)
    for(i in 1:nrow(n)){
      if (n[i,1]<=R) { 
        #print(n[i,1]*margen-max(R-n[i,1],0)*margen_c)
        beneficio[i,]<-n[i,1]*margen-max(R-n[i,1],0)*margen_c
      } else {
        #print(R*margen-(n[i,1]-R)*margen)
        beneficio[i,]<-R*margen-(n[i,1]-R)*margen
      }
      
    }
    beneficio <- data.frame(beneficio)
    paste("El beneficio máximo es : ", max(beneficio$beneficio, na.rm = TRUE))
  })
  output$selected_var <- renderText({ 
    n<-4:20
    n<-data.frame(n)
    #lengths(n)
    R<-input$bins
    margen<-input$C_over
    margen_c<-input$C_under
    beneficio<- matrix(ncol=1, nrow=lengths(n))
    beneficio <- data.frame(beneficio)
    for(i in 1:nrow(n)){
      if (n[i,1]<=R) { 
        print(n[i,1]*margen-max(R-n[i,1],0)*margen_c)
        beneficio[i,]<-n[i,1]*margen-max(R-n[i,1],0)*margen_c
      } else {
        #print(R*margen-(n[i,1]-R)*margen)
        beneficio[i,]<-R*margen-(n[i,1]-R)*margen
      }
      
    }
    beneficio <- data.frame(beneficio)
    paste("El beneficio máximo es : ", max(beneficio$beneficio, na.rm = TRUE))
  })
  
  output$loess <- renderPlot({n<-4:20
  n<-data.frame(n)
  #lengths(n)
  R<-input$bins
  margen<-input$C_over
  margen_c<-input$C_under
  beneficio<- matrix(ncol=1, nrow=lengths(n))
  beneficio <- data.frame(beneficio)
  for(i in 1:nrow(n)){
    if (n[i,1]<=R) { 
      #print(n[i,1]*margen-max(R-n[i,1],0)*margen_c)
      beneficio[i,]<-n[i,1]*margen-max(R-n[i,1],0)*margen_c
    } else {
      #print(R*margen-(n[i,1]-R)*margen)
      beneficio[i,]<-R*margen-(n[i,1]-R)*margen
    }
    
  }
  beneficio <- data.frame(beneficio)
  #print(beneficio)
  beneficio$Cantidad_de_repartos <- 4:20 ## el valor es el que se muestra
  ## en el eje X, es decir tiene que ser consistente con el valor del slider
  ## NO debe ser entre 1 y 21
  ## 
  
  beneficio$yvar <- 4:20
  
  ggplot(beneficio, aes(x=Cantidad_de_repartos, y=beneficio))+ geom_point() + geom_smooth(method =  "loess" )
  })
  
  datasetInput <- reactive({  
    
    
    # render newmap con tsp
    output$newmap <- renderLeaflet({
      
      n<-input$integer+1
      # Tibble containing the geographic locations for our TSP problem
      data3 <- tibble(
        id = 1:n,
        lng = rnorm(n, mean = -77.0055, sd = 0.005),
        lat = rnorm(n, mean = -12.102685, sd = 0.005)
      )
      
      write.table(data3,"jat5.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      tbl_colnames <- c("Ubicacion","lng",  "lat")
      
      data3 <- read.csv(paste("jat5", ".csv", sep=""), header = TRUE, col.names = tbl_colnames)
      
      # Distance matrix 
      dist_mat <- dist(data3%>%select(lng,lat),
                       method = 'euclidean' 
      )
      # Initialize the TSP object
      tsp_prob <- TSP(dist_mat)
      
      # We add a dummy to the TSP, in this way we remove the constraint of 
      # having a path that ends at the starting point
      tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')
      #??solve_TSP
      
      # TSP solver
      tour <- solve_TSP(
        tsp_prob,
        method = 'two_opt',
        control = list(rep = 16)
      )
      path <- names(cut_tour(tour, 'dummy'))
      
      
      #str(tour)
      # Prepare the data for plotting
      data3 %<>% 
        mutate(
          id_order = order(as.integer(path))
        )
      data3 %>% 
        arrange(id_order) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(
          ~lng,
          ~lat,
          fillColor = 'red',
          fillOpacity = 0.5,
          stroke = FALSE
        ) %>% addPolylines(~lng, ~lat)
      
      })
  })
  
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$integer)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  sliderValues2 <- reactive({
    cantidad_repartos<-4:21
    n<-data.frame(cantidad_repartos)
    #lengths(n)
    R<-input$bins
    margen<-input$C_over
    margen_c<-input$C_under
    beneficio<- matrix(ncol=1, nrow=lengths(n))
    beneficio <- data.frame(beneficio)
    for(i in 1:nrow(n)){
      if (n[i,1]<=R) { 
        #print(n[i,1]*margen-max(R-n[i,1],0)*margen_c)
        beneficio[i,]<-n[i,1]*margen-max(R-n[i,1],0)*margen_c
      } else {
        #print(R*margen-(n[i,1]-R)*margen)
        beneficio[i,]<-R*margen-(n[i,1]-R)*margen
      }
      
    }
    data.frame(cantidad_repartos,beneficio)
    
  })
  
  # Show the values in an HTML table ----
  output$values2 <- renderTable({
    sliderValues2()
  })
  
  
  
  output$distPlot2 <- renderPlot({
    hist(rnorm(input$bins))
  })
  
  #data3 <- read.csv(paste("jat3", ".csv", sep=""), header = TRUE, col.names = tbl_colnames)
  
  # Ejecuta el datasetInput que genera el comportamiento
  output$newmap <- renderLeaflet({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } else {
      
      n<-input$bins +1
      # Tibble containing the geographic locations for our TSP problem
      data3 <- tibble(
        id = 1:n,
        lng = rnorm(n, mean = -77.0055, sd = 0.005),
        lat = rnorm(n, mean = -12.102685, sd = 0.005)
      )
      
      write.table(data3,"jat54.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      tbl_colnames <- c("Ubicacion","lng",  "lat")
      
      data3 <- read.csv(paste("jat54", ".csv", sep=""), header = TRUE, col.names = tbl_colnames)
      
      # Distance matrix 
      dist_mat <- dist(data3%>%select(lng,lat),
                       method = 'euclidean' 
      )
      # Initialize the TSP object
      tsp_prob <- TSP(dist_mat)
      
      # We add a dummy to the TSP, in this way we remove the constraint of 
      # having a path that ends at the starting point
      tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')
      #??solve_TSP
      
      # TSP solver
      tour <- solve_TSP(
        tsp_prob,
        method = 'two_opt',
        control = list(rep = 16)
      )
      path <- names(cut_tour(tour, 'dummy'))
      
      #str(tour)
      # Prepare the data for plotting
      data3 %<>% 
        mutate(
          id_order = order(as.integer(path))
        )
      
      data3 %>% 
        arrange(id_order) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(
          ~lng,
          ~lat,
          fillColor = 'red',
          fillOpacity = 0.5,
          stroke = FALSE
        ) %>% addPolylines(~lng, ~lat)
    }
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
