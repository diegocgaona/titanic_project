shinyUI(bootstrapPage(
      titlePanel("Titanic Survival Probabilities"),
      
      sidebarLayout(position = "left",
                    sidebarPanel( "Inputs"),
                    mainPanel("Outcome"),
      
      selectInput(inputId = "title",
                  label = "Select the Title of the person (Mr, Mrs...):",
                  choices = c("Mr", "Mrs","Miss", "Ms", "Master", "Sir", "Lady", "Col", "Dr", "Rev", "Mlle"),
                  selected = "Mr"),
      
      radioButtons(inputId = "Sex",
                    label = strong("Select the Gender"),
                    choices = c("male", "female")),
      
      sliderInput(inputId = "SibSp",
                   label = strong("Number of Siblings/Spouses Aboard"),
                   value = 0,
                   min = 0,
                   max = 8,
                   step = 1,
                   animate = TRUE,
                   round = TRUE
                   ),
      
      sliderInput(inputId = "Parch",
                   label = strong("Number of Parents/Children Aboard"),
                   value = 0,
                   min = 0,
                   max = 9,
                   step = 1,
                   animate = TRUE,
                   round = TRUE
      ),
      
      selectInput(inputId = "Pclass",
                  label = "Passenger Class",
                  choices = c("1st", "2nd","3rd"),
                  selected = "3rd"),
      
      sliderInput(inputId = "Fare",
                  label = strong("Passenger Fare"),
                  value = 0,
                  min = 0,
                  max = 513,
                  step = 1,
                  animate = TRUE,
                  round = TRUE
      ),
      
      selectInput(inputId = "Embrarked",
                  label = "Port of Embarkation",
                  choices = c("Cherbourg", "Queenstown","Southampton"),
                  selected = "Cherbourg"),
      
   
      
      mainPanel(
            textOutput("prob1")
      )),
      
      # Display this only if the density is shown
      conditionalPanel(condition = "input.density == true",
                       sliderInput(inputId = "bw_adjust",
                                   label = "Bandwidth adjustment:",
                                   min = 0.2, max = 2, value = 1, step = 0.2)
      )
      
))