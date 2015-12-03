library(shiny)
require(randomForest)

## Read all_data
all_data <- read.csv("complete_data.csv", sep = ";")

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
all_data$Embarked[c(62, 830)] <- "S"

# Factorize embarkment codes.
all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method = "anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])


## Creating a Random Forest model
set.seed(155)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex * Age + SibSp + Parch + Fare + Embarked + Title, 
                          data = all_data, importance = TRUE, ntree = 15000)

## BEGIN
shinyServer(function(input, output) {
      
      predictions <- predict(my_forest, data.frame(c(Pclass = input$Pclass, Sex = input$Sex, Age = input$Age, SibSp = input$SibSp, 
                                                     Parch = input$Parch, Fare = input$Fare, Embarked = input$Embarked, Title = input$Title)))
      
      output$prob1 <- renderText({
            
            
            
      })
})

textOutput()