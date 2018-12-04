# r shiny app to explore data on kickstarter projects #

# load packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(fBasics)
library(shinycssloaders)

# assign csv file to an object and select relevant columns
kickstarter <- read.csv(url("https://raw.githubusercontent.com/jessicatowne/RShiny/master/Kickstarter1.csv"))
kickstarter <- select(kickstarter, Name, Category, Outcome, Goal, Backers, Pledged, Duration)

#-------------------------------------------------------------------------------------------------------------------------------

# create user interface object and make dashboard page with header
ui <- dashboardPage(
 dashboardHeader(title = "Kickstarter"),
 
# put three tabs in the sidebar
 dashboardSidebar(
   sidebarMenu(
      menuItem("Project list", tabName = "project", icon = icon("table"), selected = TRUE),
      menuItem("Linear regression", tabName = "lr", icon = icon("line-chart")),
      menuItem("Source code", icon = icon("file-code-o"),
               href = "https://github.com/jessicatowne/RShiny/blob/master/kickstarter3.R")
 )),

# make main body of the dashboard and link to the first tab
dashboardBody(
 tabItems(
  tabItem(
    tabName = "project",
    fluidRow(
 
# make a box containing a data table  
    box(
      title = "Project list",
      width = 12,
      status = "warning",
      solidHeader = TRUE,
      DT::dataTableOutput("table")))),
 
# link second tab to main dashboard body   
  tabItem(
    tabName = "lr",
    fluidRow(
     
# make a box containing user inputs - dropdown boxes and checkboxes     
      box(
       title = "Inputs",
       width = 3,
       status = "primary",
       solidHeader = TRUE,
       
       selectInput("outcome", "X axis",
                   choices = list("Pledged" = "Pledged",
                                  "Backers" = "Backers",
                                  "Goal" = "Goal",
                                  "Duration" = "Duration"),
                   selected = "USDPledged"),

       selectInput("indepvar", "Y axis",
                   choices = list("Pledged" = "Pledged",
                                  "Backers" = "Backers",
                                  "Goal" = "Goal",
                                  "Duration" = "Duration"),
                   selected = "Backers"),

       awesomeCheckbox("stats",
                       label = "Show summary statistics",
                       value = FALSE),   
     
       awesomeCheckbox("regression",
                       label = "Show regression summary",
                       value = FALSE)),
    
# make a box containing a scatterplot       
    box(
      title = "Linear regression",
      width = 9,
      status = "danger",
      solidHeader = TRUE,
      plotlyOutput("scatterplot") %>%
        withSpinner(color="#1f8ed2")),
     
# make a box containing summary statistics
    box(
      title = "Summary statistics",
      width = 6,
      status = "warning",
      solidHeader = TRUE,
      tableOutput("stats_summary")),    
  
# make a box containing a summary of the regression, with a white background  
    box(
      title = "Regression summary",
      width = 6,
      status = "success",
      solidHeader = TRUE,
      verbatimTextOutput("r_summary"),
      tags$style(type = 'text/css', 
                        '#r_summary{
                        background-color: rgba(255,255,255,0.40); 
                        border-style: none;}'))
    
  ))))
)

#-------------------------------------------------------------------------------------------------------------------------------

# create server function
server <- function(input, output) {
    
# produce a data table, including column filters  
  output$table <- DT::renderDataTable({
    DT::datatable(kickstarter, 
                  filter = list(position = "top"), 
                  options = list(scrollX = TRUE))
  })
 
# produce a scatterplot of a linear regression based on user inputs
  output$scatterplot <- renderPlotly({
    plot <- ggplot(kickstarter, aes_string(x = input$outcome,
                                           y = input$indepvar,
                                           color = kickstarter$Outcome)) +
                   geom_point(alpha = 0.6) +
                   geom_smooth(method = lm) +
                   theme_bw() +
                   theme(legend.title = element_blank())
    
    ggplotly(plot)
  })
   
# summarise the basic statistics of the dataset in a table if checkbox is checked   
  output$stats_summary <- renderTable({
    stats_columns <- select(kickstarter, Goal, Backers, Pledged, Duration)
    stats_table <- basicStats(stats_columns)[c("Minimum", "Maximum", "Mean", "Median"),]
    
    if(input$stats){
      stats_table}},   
       rownames = TRUE,
       digits = 0,
       width = "100%")
 
# print a summary of the linear regression if checkbox is checked   
  output$r_summary <- renderPrint({
    fit <- lm(kickstarter[,input$outcome] ~ kickstarter[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    
    if(input$regression){
      summary(fit)}
  })

}

shinyApp(ui, server)