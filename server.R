library(shiny)
require(randomForest)
require(rpart)

## Read model
my_model <- readRDS("my_model.rds")
## Read data structure
datamodel <- readRDS("datamodel.RDS")

## BEGIN
shinyServer(function(input, output) {
      
      inputdata <- reactive({
            data.frame(
                  "Age" = as.numeric(input$Age),
                  "Sex" = factor(input$Sex, levels = levels(datamodel$Sex)),
                  "Pclass" = factor(input$Pclass, levels = levels(datamodel$Pclass)),
                  "family_size" = factor(input$family_size, levels = levels(datamodel$family_size)),
                  "Title" = factor(input$Title, levels = levels(datamodel$Title)),
                  "Fare" = as.numeric(input$Fare)
                  )
      })
      
      # Return the formula text for printing as a caption
      output$inputdata <- renderTable({
            inputdata()
      })
      
      output$text1 <- renderText({
         predictions <- predict(my_model, type = "prob", inputdata())
         paste(round(predictions[,2]*100, digits = 2), "%")
      })
      
     
     
      
})