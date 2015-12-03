require(randomForest)
require(caret)
require(rpart)

## Read all_data
all_data <- read.csv("complete_data.csv", sep = ";")
all_data$Pclass <- factor(all_data$Pclass)

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

## Modifying data types
all_data$Survived <- as.factor(all_data$Survived)
all_data$Pclass <- as.factor(all_data$Pclass)
all_data$family_size <- as.factor(all_data$family_size)

## Creating a Random Forest model
set.seed(155)
my_model <- randomForest(Survived ~ Pclass + Sex + Title + Age + family_size + Fare, 
                         data = all_data, localImp = TRUE, ntree = 15000, 
                         proximity = TRUE, oob.times = 60, nodesize = .3)

varImpPlot(my_model)

saveRDS(my_model, "my_model.rds")

predictions <- predict(my_model, newdata = teste, type = "prob")


preddf <- data.frame(
      Pclass = input$Pclass,
      Sex = input$Sex,
      Age = input$Age,
      SibSp = input$SibSp,
      Parch = input$Parch,
      Fare = input$Fare,
      Embarked = input$Embarked,
      Title = input$Title
)
levels(preddf$Sex) <- levels(all_data$Sex)
levels(preddf$Embarked) <- levels(all_data$Embarked)
levels(preddf$Title) <- levels(all_data$Title)
preddf$Pclass <- as.factor(preddf$Pclass)
preddf$SibSp <- as.integer(preddf$SibSp)
preddf$Parch <- as.integer(preddf$Parch)
levels(preddf$Pclass) <- levels(all_data$Pclass)


teste <- data.frame(
      Pclass = 1, 
      Sex = "male", 
      Age = 29, 
      Fare = 180, 
      Title = "Mr",
      family_size = "1")
levels(teste$Sex) <- levels(all_data$Sex)
levels(teste$family_size) <- levels(all_data$family_size)
teste$family_size <- as.factor(teste$family_size)
levels(teste$Title) <- levels(all_data$Title)
teste$Pclass <- as.factor(teste$Pclass)
levels(teste$Pclass) <- levels(all_data$Pclass)
saveRDS(teste[1,], "datamodel.RDS")
str(readRDS("datamodel.RDS"))



      
      shinyapps::deployApp()

