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

# Build a better decision tree
tree2 <- rpart(Survived ~ Sex + Age + Pclass + SibSp, 
               data = train, 
               method ="class")
fancyRpartPlot(tree2)
pred_tree2 <- predict(tree2, newdata = test, type = "class")
my_solution_test <- data.frame(PassengerId = test$PassengerId, Survived = pred_tree2)
write.csv(x = my_solution_test, file = "my_solution_test.csv", row.names = FALSE)
