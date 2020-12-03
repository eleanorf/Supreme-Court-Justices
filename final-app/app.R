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
             h3("Welcome to My Final Project"),
             h3("About Me"),
             h4("Here is a graph that counts how frequently each justice cast a 
                liberal or conservative vote as a Supreme Court Justice."),
             fluidPage(
                 selectInput("filter", "Choose a Justice", 
                             choices = d$justiceName,
                             selected = "RBGinsburg"),
                 plotOutput("justice_direction")
             ),
    tabPanel("Model",
             titlePanel("Model"),
             p("Here is a graph of...")
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
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$justice_direction <- renderPlot({
        d %>% 
            filter(justiceName == input$filter) %>% 
            drop_na(direction) %>% 
            ggplot(aes(x = direction)) +
            geom_bar() +
            theme_bw() +
            labs(title = "Ideological Direction Count",
                 x = "Direction",
                 y = "Count") +
            scale_x_continuous(breaks = c(1, 2),
                               label = c("Conservative", "Liberal"))
    }, res = 96)
    }


# Run the application 
shinyApp(ui = ui, server = server)
