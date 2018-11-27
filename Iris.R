library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(datasets)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Iris data"),
    
    dashboardSidebar(
      selectInput(inputId = "y",
              label = "Y axis:",
              choices = c("Petal length" = "iris$Petal.Length",
                          "Petal width" = "iris$Petal.Width",
                          "Sepal length" = "iris$Sepal.Length",
                          "Sepal width" = "iris$Sepal.Width"),
              selected = "iris$Petal.Length"),
  
      selectInput(inputId = "x",
              label = "X axis:",
              choices = c("Petal length" = "iris$Petal.Length",
                          "Petal width" = "iris$Petal.Width",
                          "Sepal length" = "iris$Sepal.Length",
                          "Sepal width" = "iris$Sepal.Width"),
              selected = "iris$Petal.Width"),
      
      checkboxInput(inputId = "summary",
                    label = "Show summary statistics",
                    value = FALSE),
      
      checkboxInput(inputId = "data",
                    label = "Show data table",
                    value = FALSE)
      
    ), 
  
  dashboardBody(
    
      plotlyOutput(outputId = "scatterplot"),
      
      verbatimTextOutput(outputId = "stats"),
      
      DT::dataTableOutput(outputId = "table")
     
    )
  )

server <- function(input, output){
  output$scatterplot <- renderPlotly({ 
    plot <- ggplot(data = iris, 
                   aes_string(x = input$x, 
                              y = input$y, 
                              color = iris$Species)) +
            geom_point(size = 1, alpha = 0.6) +
            stat_ellipse(lwd = 0.5) +
            theme_bw() +
            theme(legend.title = element_blank(), title = element_blank())
    
  topmargin <- list(t = 80)
    
  ggplotly(plot, tooltip = c("x", "y")) %>%
    
    add_annotations(text = "Select one or <br> more species:", 
                    xref = "paper", yref = "paper",
                    x = 1.02, xanchor = "left",
                    y = 0.8, yanchor = "bottom",
                    legendtitle = TRUE, 
                    showarrow = FALSE) %>%
    
    add_annotations(text = "<b>Petal and sepal sizes in three iris species</b>",
                    xref = "paper", yref = "paper",
                    x = 0,
                    y = 1.1,
                    showarrow = FALSE) %>%
    
    layout(legend = list(y=0.8, yanchor="top"), margin = topmargin,
           xaxis = list(title = "X axis"),
           yaxis = list(title = "Y axis"))
    
    })
  
  output$stats <- renderPrint({
    if(input$summary){
      summary(iris)
    }
  })
  
  output$table <- DT::renderDataTable({
    if(input$data){
      iris
    }
  })
}

shinyApp(ui = ui, server = server)