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

## Creating a Random Forest model
set.seed(155)
my_model <- randomForest(as.factor(Survived) ~ as.numeric(all_data$Pclass) + as.factor(all_data$Sex) * as.numeric(all_data$Age) + as.integer(all_data$SibSp) 
                          + as.integer(all_data$Parch) + as.numeric(all_data$Fare) + as.factor(all_data$Embarked) + as.factor(all_data$Title), 
                          data = all_data, importance = TRUE, ntree = 15000)
my_prediction <- predict(my_model, newdata = all_data)
my_solution <- data.frame(PassengerId = all_data$PassengerId, Survived = my_prediction)
# Confusion Matrix
modelconfusion <- confusionMatrix(all_data$Survived, my_solution$Survived)$overall
modelconfusion

my_model <- randomForest(Survived ~ Pclass + Sex * Age + SibSp 
                         + Parch + Fare + Embarked + Title, 
                         data = all_data, importance = TRUE, ntree = 15000)

predictions <- predict(my_model, newdata = test_new[3,c(3:14)], type = "prob")
predictions <- predict(my_model, newdata = teste, type = "prob")


predictions <- predict(my_model, data.frame(c(Pclass = input$Pclass, Sex = input$Sex, Age = input$Age, SibSp = input$SibSp, 
                                               Parch = input$Parch, Fare = input$Fare, Embarked = input$Embarked, Title = input$Title)))

teste <- all_data[1,]
teste$Pclass <- factor("3")


teste <- data.frame(
      Pclass = 3, 
      Sex = "male", 
      Age = 30, 
      SibSp = 5, 
      Parch = 8, 
      Fare = 1500, 
      Embarked = "C", 
      Title = "Master")
levels(teste$Sex) <- levels(all_data$Sex)
levels(teste$Embarked) <- levels(all_data$Embarked)
levels(teste$Title) <- levels(all_data$Title)
teste$Pclass <- as.factor(teste$Pclass)
teste$SibSp <- as.integer(teste$SibSp)
teste$Parch <- as.integer(teste$Parch)
levels(teste$Pclass) <- levels(all_data$Pclass)


my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                      data = all_data, 
                      method = "anova" )
predictions <- predict(my_tree_five, newdata = teste)
