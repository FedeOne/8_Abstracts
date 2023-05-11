
library(shiny)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Federico/Desktop/8_Abstracts/data_manage_code/prevalence")

exporttoTryApp <-  readRDS("exporttoTryApp.rds") 


ui <- fluidPage(
  
  # App title ----
  titlePanel("Prevalence Plot Abstract mine"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput(inputId = "prevExpo",
                  label = "Frequency of ?"
                  )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "freqPlot"),
      # DT::dataTableOutput("table"),
      # DT::dataTableOutput("list"),
      
    )
  )
)


server <- function(input, output) {
  
  
  
  # pop1 <- reactive({
  #   pop1 <-  rnorm(input$nSubs, input$mean1, input$sd) %>% as.data.frame()
  #   pop1
  # }
  # )
  # 
  # pop2 <- reactive({
  #   pop2 <- rnorm(input$nSubs,input$mean2,input$sd)%>% as.data.frame()
  #   pop2
  # }
  # )
  # 
  # df <- reactive({
  #   df <- cbind(pop1(),pop2()) %>% as.data.frame()
  #   df
  # })
  # 
  
  output$freqPlot <- renderPlot({
    
    # req(df())
    
    p1 <-  exporttoTryApp %>% 
      filter(prevExposure1 == input$prevExpo) %>% 
      ggplot( mapping = aes(x = PMID, y = freqFin ,  fill = prevPop1)) + 
      geom_bar(stat='identity') + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))  +
      labs(fill = "Population")
    
    p1
    
  })
  
  
  # output$table<- DT::renderDataTable({
  #   
  #   DT::datatable(df() )
  # } )
  # 
  # output$list<- DT::renderDataTable({
  #   
  #   DT::datatable(pop1() )
  # } )
  
} # end server


shinyApp(ui = ui, server = server)

