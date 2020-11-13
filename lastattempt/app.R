library(shiny)
library(tidyverse)

library(readr)
performances <- read_csv("data/tidy/performances.csv")

ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("scores")))
             ), 
             p("This week, I worked on cleaning my data in a different file. 
               That's why I've submitted this milestone late because I totally 
               forgot I needed to submit a link instead of just my data cleaning
               file. I really worked on getting the judges names to appear and 
               trying to create a tibble that matches manually has matched
               judge to competition names")),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("This week I really had to reteach myself how to do read csv and clear up pathways as it was destroying all of my shiny attempts.
               I also worked on cleaning up my data in the judges_aspects category. I've also worked on brainstorming various tests and methods of evaluating judging biases including regressions and mean comparisons of judges tagged with logicals that say whether they are from the nation of the skater they are judging. 
               This will allow me to see if they are higher or lower than the mean of the other judges of the panel. ")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project is a project about international judging biases on subjective olympic sports. 
               For this project, I will be looking at 3 main sports: figure skating, gymnastics, and diving. The 
               purposes of this is to investigate whether judges have national biases that are reflected in the 
               scores and whether these biases have a statistically significant impact on the competitive outcomes."),
             p("I did this project because it allows me to get to explore one of my favorite topics: olympic sports. 
             My love for olympic sports, especially the subjective ones, is really exciting. This is the perfect 
             opportuntiy for me to research these in an academic setting."),
             h3("About Me"),
             p("My name is Julia Blank, a first year in Government 50 data. 
             I inted to study Government on the Data Science track.  
             You can reach me at juliablank@college.harvard.edu.")))

server <- function(input, output) {
    output$scores <- renderPlot({
        performances %>%
            filter(competition == "Grand Prix Final 2017 Senior and Junior") %>%
            group_by(competition, name, nation) %>%
            summarize(total_comp_score = sum(total_segment_score), .groups = "drop") %>%
            ggplot(aes(x = name, y = total_comp_score, color = nation)) +
            geom_point() + 
            labs(title = "Grand Prix Final 2017 Total Competition Score", 
                 x = "competitors", 
                 y = "Total Competition Score") +
            theme(axis.text.x = element_text(angle = 90))
    })
}

shinyApp(ui = ui, server = server)
