require(rpart)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

# Print train and test to the console
# print(c(train,test))
str(train)
str(test)

# Passengers that survived vs passengers that passed away
# absolute numbers # sum(train$Survived) get the number of survivor too
table(train$Survived) 

# percentages
prop.table(table(train$Survived))

# Gender vs Survived
prop.table(table(train$Sex, train$Survived), margin = 1)

# Test of p-values: 
# fitlm <- lm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, na.action = na.exclude)
# best_step_model <- step(fitlm, direction = "backward")
# Test of NearZeroVariance
# require(caret)
# nzv <- nearZeroVar(train, saveMetrics = TRUE)

# Create the column child, and indicate whether child or no child
# train$Child[train$Age == NA] <- NA
# train$Child[train$Age < 18 ] <- 1
# train$Child[train$Age >= 18 ] <- 0

# Two-way comparison
# prop.table(table(train$Child, train$Survived), margin = 1)

# Create a copy of test: test_one
# test_one <- test
# Initialize a Survived column to 0
# test_one$Survived <- 0
# Set Survived to 1 if Sex equals "female"
# test_one$Survived[test_one$Sex == "female"] <- 1

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                 data = train, 
                 method ="class")
# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)

# Make your prediction using the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
# Check that your data frame has 418 entries
nrow(my_solution)
# Write your solution to a csv file with the name my_solution.csv
write.csv(x = my_solution, file = "my_solution.csv", row.names = FALSE)

# Create a new decision tree my_tree_three
control <-  rpart.control(minsplit = 50, cp = 0)
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                       data = train, 
                       method ="class", control = control)
my_prediction2 <- predict(my_tree_three, newdata = test, type = "class")
my_solution2 <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction2)
write.csv(x = my_solution2, file = "my_solution2.csv", row.names = FALSE)

fancyRpartPlot(my_tree_three)

## Create a new train dataset with family size
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1
# Create the model
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, 
                     data = train_two, 
                     method ="class")
# Time to plot your fancy tree
fancyRpartPlot(my_tree_four)

## New Model, using Title (from DataCamp, I don't have this new datasets)
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                     data = train_new, 
                     method ="class")
my_prediction <- predict(my_tree_five, newdata = test_new, type = "class")
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(x = my_solution, file = "my_solution.csv", row.names = FALSE)


## Creating a Random Forest model
set.seed(111)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                          data = train, importance = TRUE, ntree = 1000)
my_prediction <- predict(my_forest, newdata = test)
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(x = my_solution, file = "my_solution.csv", row.names = FALSE)

#################################################################
# MY TESTS
require(rpart)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
require(randomForest)
# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

## Read all_data
all_data <- read.csv("all_data.csv")

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
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                          data = train_new, importance = TRUE, ntree = 10000)
my_prediction <- predict(my_forest, newdata = test_new)
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(x = my_solution, file = "my_solution.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)

###########################
#CREATING TEST DATA
inTrain <- createDataPartition(y = train_new$Survived, p=0.7, list=FALSE)
training <- train_new[inTrain,]
mytesting <- train_new[-inTrain,]
# TEST 1
set.seed(155)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                          data = training, importance = TRUE, ntree = 10000)
my_prediction <- predict(my_forest, newdata = mytesting)
my_solution_new <- data.frame(PassengerId = mytesting$PassengerId, Survived = my_prediction)
write.csv(x = my_solution_new, file = "my_solution_new.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)
# Confusion Matrix
test1confusion <- confusionMatrix(mytesting$Survived, my_solution_new$Survived)$overall



## Creating my model
## Model test 1 Forest
set.seed(155)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + family_size + Fare + Title, 
                          data = training, importance = TRUE, ntree = 10000)
my_predictionforest <- predict(my_forest, newdata = mytesting)
my_solution_forest <- data.frame(PassengerId = mytesting$PassengerId, Survived = my_predictionforest)
write.csv(x = my_solution_forest, file = "my_solution_forest.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)
forestconfusion1 <- confusionMatrix(mytesting$Survived, my_solution_forest$Survived)$overall

## MODEL test 1 GBM
set.seed(155)
my_gbm <- train(as.factor(Survived) ~ Pclass + Sex + Age + family_size + Fare + Title, 
                          data = training, method = "gbm")
my_predictiongbm <- predict(my_gbm, newdata = mytesting)
my_solution_gbm <- data.frame(PassengerId = mytesting$PassengerId, Survived = my_predictiongbm)
write.csv(x = my_solution_gbm, file = "my_solution_gbm.csv", row.names = FALSE)
gbmconfusion <- confusionMatrix(mytesting$Survived, my_solution_gbm$Survived)$overall

#############
# FULL MODELS (trained with all data)
## Model FULL 1 Forest
set.seed(155)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + family_size + Fare + Title, 
                          data = train_new, importance = TRUE, ntree = 10000)
my_predictionforest <- predict(my_forest, newdata = test_new)
my_solution_forest <- data.frame(PassengerId = test_new$PassengerId, Survived = my_predictionforest)
write.csv(x = my_solution_forest, file = "my_solution_forest.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)


## MODEL FULL 1 GBM
set.seed(155)
my_gbm <- train(as.factor(Survived) ~ Pclass + Sex + Age + family_size + Fare + Title, 
                data = train_new, method = "gbm")
my_predictiongbm <- predict(my_gbm, newdata = test_new)
my_solution_gbm <- data.frame(PassengerId = test_new$PassengerId, Survived = my_predictiongbm)
write.csv(x = my_solution_gbm, file = "my_solution_gbm.csv", row.names = FALSE)


###############
# Tests phase 2
## Model test 2 Forest
set.seed(155)
my_forest <- randomForest(as.factor(Survived) ~ Pclass * Fare + Sex * Age + family_size * Title + SibSp, 
                          data = training, importance = TRUE, ntree = 10000)
my_predictionforest <- predict(my_forest, newdata = mytesting)
my_solution_forest <- data.frame(PassengerId = mytesting$PassengerId, Survived = my_predictionforest)
write.csv(x = my_solution_forest, file = "my_solution_forest.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)
forestconfusion1 <- confusionMatrix(mytesting$Survived, my_solution_forest$Survived)$overall

## MODEL test 2 GBM
set.seed(155)
my_gbm <- train(as.factor(Survived) ~ Pclass + Sex + Age + family_size + SibSp + Fare + Title, data = training, 
                method = "gbm", trControl = trainControl(method="repeatedcv", number=15, repeats=5, savePred=T))
my_predictiongbm <- predict(my_gbm, newdata = mytesting)
my_solution_gbm <- data.frame(PassengerId = mytesting$PassengerId, Survived = my_predictiongbm)
write.csv(x = my_solution_gbm, file = "my_solution_gbm.csv", row.names = FALSE)
gbmconfusion <- confusionMatrix(mytesting$Survived, my_solution_gbm$Survived)$overall


###############
# FULL MODELS phase 2
## FULL MODELS 2 Forest
set.seed(155)
my_forest <- randomForest(as.factor(Survived) ~ Pclass * Fare + Sex * Age + family_size * Title + SibSp, 
                          data = train_new, importance = TRUE, ntree = 10000)
my_predictionforest <- predict(my_forest, newdata = test_new)
my_solution_forest <- data.frame(PassengerId = test_new$PassengerId, Survived = my_predictionforest)
write.csv(x = my_solution_forest, file = "my_solution_forest.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)


## FULL MODELS 2 GBM
set.seed(155)
my_gbm <- train(as.factor(Survived) ~ Pclass + Sex + Age + family_size + SibSp + Fare + Title, data = train_new, 
                method = "gbm", trControl = trainControl(method="repeatedcv", number = 16, repeats=8, savePred=T))
my_predictiongbm <- predict(my_gbm, newdata = test_new)
my_solution_gbm <- data.frame(PassengerId = test_new$PassengerId, Survived = my_predictiongbm)
write.csv(x = my_solution_gbm, file = "my_solution_gbm.csv", row.names = FALSE)

## PHASE 3 - Using similar model from mytree_five
## FULL MODELS 3 Forest
set.seed(111)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                          data = train_new, importance = TRUE, ntree = 15000)
my_predictionforest <- predict(my_forest, newdata = test_new)
my_solution_forest <- data.frame(PassengerId = test_new$PassengerId, Survived = my_predictionforest)
write.csv(x = my_solution_forest, file = "my_solution_forest.csv", row.names = FALSE)
# Var importance
varImpPlot(my_forest)





# Build a better decision tree (BAD, AND NOT RAMDOM FOREST)
tree2 <- rpart(Survived ~ Sex + Age + Pclass + SibSp, 
               data = train, 
               method ="class")
fancyRpartPlot(tree2)
pred_tree2 <- predict(tree2, newdata = test, type = "class")
my_solution_test <- data.frame(PassengerId = test$PassengerId, Survived = pred_tree2)
write.csv(x = my_solution_test, file = "my_solution_test.csv", row.names = FALSE)
