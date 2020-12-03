library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(readr)
library(shinythemes)

d <- read_csv("Citation_Data.csv")


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project",
    tabPanel("Home", 
             tabsetPanel(
                         tabPanel("Introduction",
                                  titlePanel("Introduction"),
                                  h3("Welcome to My Final Project!"),
                                  p("In this project, I look at data regarding 
                                    Supreme Court Justices from 1946-2020. In
                                    this first graph, I have shown the average
                                    ideological leanings of each justice during
                                    this time period. A direction equal to 1 
                                    represents a conservative vote, and a 
                                    direction equal to 2 represents a liberal 
                                    vote."),
                                  fluidPage(
                                      plotOutput("alljustices_plot")),
                         ),
                         tabPanel("Individual Justices",
                                  titlePanel("Ideological Leanings of Each 
                                             Justice"),
                                  p("In this graph, I count how frequently 
                                  each justice cast a liberal or conservative 
                                    vote as a Supreme Court Justice."),
                                  fluidPage(
                                      selectInput("filter_justice", "Choose a Justice", 
                                                  choices = d$justiceName,
                                                  selected = "RBGinsburg"),
                                      plotOutput("justice_direction"))
                         ),
                         tabPanel("By President",
                                  titlePanel("Ideological Leanings of Each Justice,
                                             Grouped by President"),
                                  p("I was curious to see the voting patterns of 
                                  justices taking into account the President 
                                  who appointed them to their office. Pick a 
                                  President below to see the voting patterns of 
                                    the justices whom they appointed!"),
                                  fluidPage(
                                      selectInput("filter_president", 
                                                  "Choose a President", 
                                                  choices = d$president,
                                                  selected = "Clinton"),
                                      plotOutput("president_direction"))
                         )
                         )
             ),
    tabPanel("Model",
             titlePanel("Model"),
             p("This is where I will put my model. I am going to use stan_glm to 
               predict how a justice might vote depending on different variables.
               I have already created a model in my gather.Rmd, but I only used
               glm instead of stan_glm because I am having computer troubles. 
               My dataset is so large that it takes a while for my computer to 
               run each function.")
    ),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I have always been fascinated by the Supreme Court and curious 
               to learn more about each justice’s voting patterns. This fall, 
               Justice Ruth Bader Ginsburg passed away and President Trump 
               nominated Amy Coney Barret who was quickly confirmed by the 
               senate. It felt as though the entire world was abuzz with 
               questions, asking what this change in the Court would mean for 
               the US. Personally, I wanted to know how much the ideological 
               makeup of the court really mattered. Supreme Court Justices 
               claim to be above politics, and one would hope that their life 
               terms remove them from the political pressures of reelection. 
               In this project, I look at the ideological direction of each 
               justice’s votes from multiple angles. Then, I create a model 
               that attempts to predict how a justice might vote given different 
               explanatory variables.
               In order to answer these questions, I look at data regarding 
               Supreme Court justices from 1946 to 2020. The data I used came 
               from", a("Washington University Law’s Supreme Court Database.",
                        href = "http://supremecourtdatabase.org/documentation.php")
             ),
             h3("About Me"),
             p("My name is Eleanor Fitzgibbons and I study Government. You can 
             reach me at efitzgibbons@college.harvard.edu. This is a link to 
               my", a("repo.", 
                      href = "https://github.com/eleanorf/gov50-final-project")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$justice_direction <- renderPlot({
        d %>% 
            filter(justiceName == input$filter_justice) %>% 
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
    output$president_direction <- renderPlot({
        d %>% 
            filter(president == input$filter_president) %>% 
            drop_na(direction) %>% 
            ggplot(aes(x = justiceName, y = direction_mean)) +
            geom_point() +
            theme_bw() +
            labs(title = "Average Ideological Direction",
                 subtitle = "Of the Justices Appointed by the Selected President",
                 x = "Justice",
                 y = "Ideological Direction") +
            ylim(c(1, 2))
    }, res = 96)
    output$alljustices_plot <- renderPlot({
        d %>% 
            ggplot(aes(x = fct_reorder(justiceName, direction_mean), 
                       y = direction_mean)) +
            geom_point(color = "lightblue") +
            labs(title = "Ideological Direction of Supreme Court Justices",
                 subtitle = "Average of Ideological Leanings in Cases from 1946-2020",
                 x = "Justices",
                 y = "Ideological Direction") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90))
    }, res = 96)
    }


# Run the application 
shinyApp(ui = ui, 
         server = server)
