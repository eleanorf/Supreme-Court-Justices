library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(readr)
library(shinythemes)
library(shinystan)
library(gtsummary)
library(gt)
library(broom.mixed)
library(rstanarm)

# Here I load the data which I downloaded from WashU Law's Supreme Court
# Database.

d <- read_csv("Citation_Data.csv")

# I saved my model as an object in gather.Rmd so that I would not have to load
# the stan_glm each time I run my app.

justiceissue_model <- readRDS("justiceissue_model.rds")

ui <- navbarPage(
    "Supreme Court Justices Over the Years",
    theme = shinythemes::shinytheme("superhero"),
    tabPanel("Home", 
             tabsetPanel(
                 
                 # My home panel has tabs inside of it that show all of the
                 # graphs that I made for this project. I could have put them
                 # all on one page, but I preferred the cleaner look of
                 # different tabs for each.
                 
                         tabPanel("Introduction",
                                  titlePanel("Average Ideological Leaning of Each 
                                     Justice from 1946-2020"),
                                  br(),
                                  p("In this project, I look at data regarding 
                                    Supreme Court Justices from 1946-2020. In
                                    this first graph, I have shown the average
                                    ideological leanings of each justice during
                                    this time period. A direction equal to 1 
                                    represents a conservative vote, and a 
                                    direction equal to 2 represents a liberal 
                                    vote."),
                                  br(),
                                  fluidPage(
                                      plotOutput("alljustices_plot"))
                         ),
                         tabPanel("Individual Justices",
                                  titlePanel("Ideological Leanings of Each 
                                             Justice"),
                                  p("In this graph, I count how frequently 
                                  each justice cast a liberal or conservative 
                                    vote as a Supreme Court Justice."),
                                  fluidPage(
                                      selectInput("filter_justice", 
                                                  "Choose a Justice", 
                                                  
                                                  # Sometimes my lines go over
                                                  # 80 characters because this
                                                  # part of the app has been
                                                  # indented so far that it is
                                                  # unavoidable.
                                                  
                                                  choices = unique(d$justice_fullnames),
                                                  
                                                  # I selected a choice for each
                                                  # of the graphs that I made in
                                                  # this project. For most I
                                                  # used RBG as the default,
                                                  # because she was a large part
                                                  # of the motivation for this
                                                  # project.
                                                  
                                                  selected = "Ruth Bader Ginsburg"),
                                      plotOutput("justice_direction"))
                         ),
                         tabPanel("Justices Over Time",
                                  titlePanel("Ideological Leanings of Each 
                                             Justice Over Time"),
                                  p("Below is the average leaning of each justice
                                    by year."),
                                  fluidPage(
                                      selectInput("filter_justicetime", 
                                                  "Choose a Justice", 
                                                  choices = unique(d$justice_fullnames),
                                                  selected = "Ruth Bader Ginsburg"),
                                      plotOutput("justice_overtime"))
                         ),
                         tabPanel("Presidents",
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
                                                  choices = unique(d$president),
                                                  selected = "Roosevelt"),
                                      plotOutput("president_direction"))
                         ),
                         tabPanel("Chief Justices",
                                  titlePanel("Is the Chief Justice Typically in
                                             the Majority?"),
                                  p("In this graph, I look at the frequency with
                                    which each justice is in the majory, 
                                    highlighting the chief justices."),
                                  fluidPage(
                                      plotOutput("chief_majority_plot"))
                                  )
             )),
    tabPanel("Model",
             titlePanel("Expected Votes"),
             sidebarLayout(
                 sidebarPanel(p("I used stan_glm to create a model that predicts 
                 how each Justice might vote. I set my outcome variable to be 
                 the ideological direction of each vote, and I included each 
                 justice and each issue area as explanatory variables. In the 
                 table below, you can see the results of my model. The results 
                 are slightly hard to interpret. Because I ran a logistic 
                 regression, you would have to put the coefficients into a logit 
                 link function in order for them to have a 1:1 relationship with
                 the y variable. However, the relationship between the 
                 coefficients and therefore between justices and issue is much
                 simpler. I made the outcome variable to be 1 if the vote is 
                 conservative or 0 if the vote is liberal. As a result, higher 
                 values are more conservative and lower values are more liberal. 
                 Scroll through the table to see all of the variables.")
                              ),
                 mainPanel(
                     gt_output("justiceissue_table")
                 )
             ),
             br(),
             h3("Posterior Probability Distribution Plot"),
             p("Next, I forecast how my model expects each justice to vote in a 
               given issue area. To do this, I created posterior distributions 
               to generate expected values. Select two justices below to see 
               their posterior probability distributions for a case under the 
               civil rights category."),
             fluidPage(
                 selectInput("filter_justice_1",
                             "Choose a Justice",
                             choices = unique(d$justice_fullnames),
                             selected = "Ruth Bader Ginsburg"),
                 selectInput("filter_justice_2",
                             "Choose a Justice",
                             choices = unique(d$justice_fullnames),
                             selected = "Neil Gorsuch"),
                 plotOutput("justiceissue_plot")
             )
    ),
    tabPanel("About", 
             h3("Project Background and Motivations"),
             p("I have always been fascinated by the Supreme Court and curious 
               to learn more about each justice’s voting patterns. This fall, 
               Justice Ruth Bader Ginsburg passed away and President Trump 
               nominated Amy Coney Barret who was quickly confirmed by the 
               senate. Since then, the entire country has been wondering what 
               this change in the Court will mean for the US. Personally, I 
               wanted to better understand how much the ideological makeup of 
               the court really mattered. Supreme Court Justices claim to be 
               above politics, and one would hope that their life terms remove 
               them from the political pressures of reelection. However, I 
               wanted to look at the patterns myself."),
             br(),
             
             # Here I linked the codebook of my data, so anyone can see where
             # exactly it came from.
             
             p("In order to answer these questions, I looked at data regarding 
               Supreme Court justices from 1946 to 2020. The data I used came 
               from", a("Washington University Law’s Supreme Court Database.",
                        href = "http://supremecourtdatabase.org/documentation.php")),
             br(),
             p("In this project, I looked at the ideological direction of each 
               justice’s votes from multiple angles, comparing their votes 
               with those of other justices and with their own votes over time. 
               Then, I ran a logistic regression and created a model that 
               attempts to predict how each justice might vote given different 
               explanatory variables."),
             h3("About Me"),
             
             # I also linked my repo. This way, anyone looking at the project
             # can easily access my code.
             
             p("My name is Eleanor Fitzgibbons and I study Government at Harvard. 
             You can reach me at efitzgibbons@college.harvard.edu. If you would 
             like to take a look at my code, this is a link to my", 
               a("Github repository.",
                 href = "https://github.com/eleanorf/gov50-final-project")))
)

server <- function(input, output, session) {
    output$alljustices_plot <- renderPlot({
        d %>% 
            
            # I used fct_reorder in the x argument because the plot looked messy
            # in the original order. I also think it is interesting to see the
            # justices in order of ideological direction. Especially because
            # this data goes back to 1946, it is engaging to be able to compare
            # justices from different times.
            
            ggplot(aes(x = fct_reorder(justice_fullnames, direction_mean), 
                       y = direction_mean)) +
            geom_point(color = "lightblue") +
            labs(title = "Ideological Direction of Supreme Court Justices",
                 subtitle = "Average of Ideological Leanings in Cases from 1946-2020",
                 x = "Justices",
                 y = "Ideological Direction") +
            theme_bw() +
            
            # I had to rotate the x axis labels in order to see each justice's
            # name.
            
            theme(axis.text.x = element_text(angle = 90))
    }, res = 96)
    
    # This graph is quite simple, but I think it is still important. Personally,
    # I enjoyed seeing which justices tended to vote more frequently liberal or
    # conservative. Although Supreme Court Justices claim to be above politics
    # and say that they vote with the best interests of the country in mind,
    # they  clearly lean one way or the other.
    
    output$justice_direction <- renderPlot({
        d %>% 
            filter(justice_fullnames == input$filter_justice) %>% 
            drop_na(direction) %>% 
            ggplot(aes(x = direction)) +
            geom_bar(fill = "lightblue") +
            theme_bw() +
            labs(title = "Ideological Direction Count",
                 x = "Direction",
                 y = "Count") +
            scale_x_continuous(breaks = c(1, 2),
                               label = c("Conservative", "Liberal"))
    }, res = 96)
    
    # This plot was fun to make and answered a lot of questions that I had going
    # into this project. As a whole, the trend lines are relatively flat. This
    # shows that justices don't really change their views much over their tenure
    # on the Supreme Court.
    
    output$justice_overtime <- renderPlot({
        d %>% 
            
            # I didn't originally have this line in my code. When I created the
            # date variable, it was automatically saved as a character vector
            # instead of numeric. Then, when I tried to use geom_smooth and make
            # a trend line, it wouldn't show up on my graph. I realized then
            # that it was because the date column was not numeric, so I added
            # this line.
            
            mutate(date = as.numeric(date)) %>% 
            group_by(justice_fullnames, date) %>% 
            mutate(avg_direction_year = mean(direction, na.rm = TRUE)) %>%
            filter(justice_fullnames == input$filter_justicetime) %>% 
            ggplot(aes(x = date, y = avg_direction_year)) +
            geom_point(color = "darkblue") +
            
            # Adding this geom_smooth line with method = "lm" created a linear
            # trend line for my data.
            
            geom_smooth(method = "lm", formula = y ~ x) +
            labs(title = "Average Ideological Direction Over Time",
                 x = "Year",
                 y = "Average Direction") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90)) +
            ylim(c(1, 2))
    }, res = 96)
    
    # This graph also answered a couple of questions that I had going into the
    # project. I knew that liberal presidents nominated liberal justices and the
    # opposite was true for conservatives, but it is striking to see all of them
    # in a graph like this. I was surprised by my results, but it is fun to see
    # it like this.
    
    output$president_direction <- renderPlot({
        d %>% 
            filter(president == input$filter_president) %>% 
            drop_na(direction) %>% 
            ggplot(aes(x = justice_fullnames, y = direction_mean)) +
            geom_point(color = "navyblue") +
            theme_bw() +
            labs(title = "Average Ideological Direction",
                 subtitle = "Of the Justices Appointed by the Selected President",
                 x = "Justice",
                 y = "Ideological Direction") +
            ylim(c(1, 2)) +
            
            # When I first made this graph, I didn't change the angle of the x
            # axis labels. Then, I realized that when the names change, some are
            # longer than others and overlap, so I had to slightly rotate the
            # names.
            
            theme(axis.text.x = element_text(angle = 20, vjust = 0.5))
    }, res = 96)
    
    # I thought this was an interesting relationship to look at. I have always
    # wondered how much the chief's vote impacts the votes of the other
    # justices. Obviously this graph doesn't show causation or give any
    # explanation, but the pattern is thought-provoking.
    
    output$chief_majority_plot <- renderPlot({
        d %>% 
            group_by(justice_fullnames) %>% 
            mutate(n_maj = sum(majority == 2, na.rm = TRUE),
                   pct_maj = n_maj/sum(majority, na.rm = TRUE),
                   .groups = "drop") %>% 
            
            # I created a variable that says whether or not the justice was a
            # chief justice or not to show that the chiefs are more frequently
            # in the majority than most other justices.
            
            ggplot(aes(x = justice_fullnames, y = pct_maj, color = chiefYES)) +
            geom_point() +
            labs(title = "Are Chief Justices More Frequently in the Majority?",
                 x = "Justice",
                 y = "Percent of Time in Majority") +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_color_manual(labels = c("No", "Yes"),
                               values = c("green", "blue"),
                               name = "Chief Justice?")
    }, res = 96)
    
    output$justiceissue_table <- render_gt({
        
        # I saved my model as an object, so it was easy to quickly load the
        # object into my shiny app instead of waiting for the stan_glm to run
        # each time.
        
        tbl_regression(justiceissue_model, intercept = TRUE) %>% 
            as_gt() %>% 
            tab_header(title = "Regression of Ideological Leaning by Justice and Issue Area")
        
        # I set the height argument to be similar to that of the explanatory
        # text in the panel next to is. I'm trying to figure out how to
        # automatically make them equal instead of manually doing it...
        
    }, height = 480)
    
    output$justiceissue_plot <- renderPlot({
        new_obs <- tibble(justice_fullnames = c(input$filter_justice_1, input$filter_justice_2),
                          
                          # In order to avoid complications with issue areas
                          # that have very little data, and in some cases, only
                          # data for a handful of justices, I decided to set the
                          # issue area as a constant in this graph. You can
                          # still pick the two justices to compare, but you will
                          # be comparing the expected votes in a case regarding
                          # civil rights.
                          
                          issueArea_name = "Civil Rights")
        pe <- posterior_epred(justiceissue_model, 
                              newdata = new_obs) %>% 
            as_tibble() %>%
            pivot_longer(cols = 1:2, 
                         names_to = "Parameter",
                         values_to = "Direction")
        
        pe %>% 
            ggplot(aes(Direction, fill = Parameter)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 1000,
                           position = "identity") +
            labs(title = "Posterior Probability Distribution",
                 subtitle = "For the Selected Justices Regarding Civil Rights Issues",
                 x = "Expected Direction of Vote",
                 y = "Probability") + 
            scale_x_continuous(labels = scales::number_format()) +
            scale_y_continuous(labels = scales::percent_format()) +
            theme_classic() +
            scale_fill_manual(values = c("lightblue", "darkblue"),
                              
                              # I put the inputs for justices as the labels too
                              # so that the graph is easy to understand!
                              
                              labels = c(input$filter_justice_1, 
                                         input$filter_justice_2),
                              name = "Justices")
    }, res = 96)
    }

# Run the application 
shinyApp(ui = ui, 
         server = server)
