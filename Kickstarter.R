library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(fBasics)
library(shinycssloaders)

setwd("C:/Users/Bun/Documents/R Shiny")
kickstarter <- read.csv("Kickstarter1.csv", header = TRUE)
kickstarter <- select(kickstarter, Name, Category, Outcome, Goal, Backers, Pledged, Duration)

ui <- dashboardPage(
 dashboardHeader(title = "Kickstarter"),
 
 dashboardSidebar(
   sidebarMenu(
      menuItem("Project list", tabName = "project", icon = icon("table"), selected = TRUE),
      menuItem("Linear regression", tabName = "lr", icon = icon("line-chart")),
      menuItem("Source code", tabName = "code", icon = icon("file-code-o"))
 )),

dashboardBody(
 tabItems(
  tabItem(
    tabName = "project",
    fluidRow(
   
    box(
      title = "Project list",
      width = 12,
      status = "warning",
      solidHeader = TRUE,
      DT::dataTableOutput("table")))),
    
  tabItem(
    tabName = "lr",
    fluidRow(
     
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
     
    box(
      title = "Linear regression",
      width = 9,
      status = "danger",
      solidHeader = TRUE,
      plotlyOutput("scatterplot") %>%
        withSpinner(color="#1f8ed2")),
     
    box(
      title = "Summary statistics",
      width = 6,
      status = "warning",
      solidHeader = TRUE,
      tableOutput("stats_summary")),    
    
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

server <- function(input, output) {
    
  output$table <- DT::renderDataTable({
    DT::datatable(kickstarter, 
                  filter = list(position = "top"), 
                  options = list(scrollX = TRUE))
  })
  
  output$scatterplot <- renderPlotly({
    plot <- ggplot(kickstarter, aes_string(x = input$outcome,
                                           y = input$indepvar,
                                           color = kickstarter$Outcome)) +
                   geom_point() +
                   geom_smooth(method = lm) +
                   theme_bw() +
                   theme(legend.title = element_blank())
    
    ggplotly(plot)
  })
    
  output$stats_summary <- renderTable({
    stats_columns <- select(kickstarter, Goal, Backers, Pledged, Duration)
    stats_table <- basicStats(stats_columns)[c("Minimum", "Maximum", "Mean", "Median"),]
    
    if(input$stats){
      stats_table}},   
       rownames = TRUE,
       digits = 0,
       width = "100%")
  
  output$r_summary <- renderPrint({
    fit <- lm(kickstarter[,input$outcome] ~ kickstarter[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    
    if(input$regression){
      summary(fit)}
  })

}

shinyApp(ui, server)