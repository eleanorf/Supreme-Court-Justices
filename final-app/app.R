#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(readr)

d <- read_csv("SCDB_2020_01_justiceCentered_Citation.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project",
    tabPanel("Home", 
             titlePanel("Home"),
             h3("Welcome to My Page"),
             h3("About Me"),
             h4("This is Smaller Text"),
             a("Google", href = "https://google.com")),
    tabPanel("Model",
             titlePanel("Model"),
             p("Here is my graphic for Milestone #6. I am still working on data 
               scraping to find the data that I will use for my final project,
               but I am using this data for now for the purpose of milestones."),
             fluidPage(
                 selectInput("x", "X variable", choices = names(d)),
                 selectInput("y", "Y variable", choices = names(d)),
                 selectInput("geom", "geom", c("point", "column", "jitter", "line")),
                 plotOutput("plot")
             )
    ),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello! I am planning on doing my final project using data 
             scraped from the internet that tests the sentiments of transcripts 
             regarding Supreme Court nominees. I am planning on comparing 
             different nominees and different news sources."),
             h3("About Me"),
             h4("Hello!"),
             p("My name is Eleanor Fitzgibbons and I study Government. You can 
             reach me at efitzgibbons@college.harvard.edu. This is a link to 
               my", a("repo.", 
                      href = "https://github.com/eleanorf/final-project")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               column = geom_col(),
               #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter(),
               line = geom_line()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
