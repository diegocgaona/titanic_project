require(randomForest)
require(caret)
require(rpart)
require(rattle)
require(rpart.plot)
require(RColorBrewer)

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

# Split the data back into a train set and a test set
train_new <- all_data[1:891,]
test_new <- all_data[892:1309,]

## Creating a Random Forest model
set.seed(155)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex * Age + SibSp + Parch + Fare + Embarked + Title, 
                          data = train_new, importance = TRUE, ntree = 15000)
my_predictionforest <- predict(my_forest, newdata = test_new)
my_solution_forest <- data.frame(PassengerId = test_new$PassengerId, Survived = my_predictionforest)
# write.csv(x = my_solution_forest, file = "my_solution_forest.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)
# Confusion Matrix
forestconfusion <- confusionMatrix(test_new$Survived, my_solution_forest$Survived)$overall
forestconfusion

## New Model, using Title (from DataCamp, I don't have this new datasets)
set.seed(155)
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                      data = train_new, 
                      method ="class")
my_prediction <- predict(my_tree_five, newdata = test_new, type = "class")
my_solution_tree <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
# write.csv(x = my_solution_tree, file = "my_solution.csv", row.names = FALSE)
treeconfusion <- confusionMatrix(test_new$Survived, my_solution_tree$Survived)$overall
treeconfusion

## Combined prediction model
preddf <- data.frame(my_predictionforest, my_prediction, Survived = test_new$Survived)
combmodel <- train(Survived ~ ., method = "gbm", data = preddf)
combpred <- predict(combmodel, predcomb)
my_solution_comb <- data.frame(PassengerId = test_new$PassengerId, Survived = combpred)
combconfusion <- confusionMatrix(test_new$Survived, my_solution_comb$Survived)$overall
treeconfusion