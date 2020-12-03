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

d <- read_csv("Citation_Data.csv")


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
             p("Here is a graph of..."),
             fluidPage(
                 selectInput("filter", "Choose a Justice", choices = names(d$justiceName)),
                 selectInput("x", "X variable", choices = names(Citation_Data)),
                 selectInput("y", "Y variable", choices = names(Citation_Data)),
                 selectInput("geom", "geom", c("point", "column", "jitter", "line")),
                 plotOutput("justice_direction")
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
    
    output$justice_direction <- renderPlot({
        filter(justiceName == .data[[input$filter]]) %>% 
        ggplot(d, aes(x = direction)) +
            geom_bar()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
