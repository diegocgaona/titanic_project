## https://diegogaona.shinyapps.io/titanic_project
library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme = shinytheme("journal"),
      titlePanel(h1("Titanic Survival Probabilities")),
      h2("Who is most likely to survive?"),
  
      sidebarPanel(              
      
      selectInput(inputId = "Title",
                  label = "Select the Title of the person:",
                  choices = c("Mr", "Mrs","Miss", "Ms", "Master", "Sir", "Lady", "Col", "Dr", "Rev", "Mlle"),
                  selected = "Mr"),
      
      radioButtons(inputId = "Sex",
                    label = strong("Select the Gender:"),
                    choices = c("Male" = "male", "Female" = "female")),
      
      sliderInput(inputId = "family_size",
                   label = strong("Number of family people aboard, including the passenger"),
                   value = 1,
                   min = 1,
                   max = 8,
                   step = 1,
                   animate = TRUE,
                   round = TRUE
                   ),
      
      selectInput(inputId = "Pclass",
                  label = "Select the Passenger Class:",
                  choices = c("1st"="1", "2nd"="2", "3rd"="3"),
                  selected = "1"),
      
      
      sliderInput(inputId = "Fare",
                  label = strong("Ticket Fare"),
                  value = 100,
                  min = 0,
                  max = 513,
                  step = 1,
                  animate = TRUE,
                  round = TRUE),
      
      
      sliderInput(inputId = "Age",
                  label = strong("Select the Age of the passenger"),
                  value = 20,
                  min = 1,
                  max = 80,
                  step = 1,
                  animate = TRUE,
                  round = TRUE)
),
      
      mainPanel( 
            
            wellPanel(
                  p("How likely to survive the sinking of ", strong("RMS Titanic"), "?", br(),
                    "I used some Machine Learning techniques to show how likely one passenger could survive."),
                  p("Use the sidebar to input the passenger data and see which variables are most important to survive."),
            
                  p("This App was based on:", br(), helpText(a("Kaggle Titanic: Machine Learning from Disaster competition", href="https://www.kaggle.com/c/titanic/", target="_blank"), 
                    helpText(a("The Kaggle R Tutorial on Machine Learning from DataCamp", href="https://www.datacamp.com/courses/kaggle-tutorial-on-machine-learing-the-sinking-of-the-titanic", target="_blank")),
                  p("And is my course project for", helpText(a("Developing Data Products from Coursera", href="https://www.coursera.org/course/devdataprod/", target="_blank")
                    
                  
            )),

            
            h3("Inputed Data:"),
            h5(tableOutput("inputdata")),
            h2("Estimated Survival Chance:"),
            h2(textOutput("text1"))
            
      
))))))