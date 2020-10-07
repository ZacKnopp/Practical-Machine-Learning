library(caret)
library(randomForest)
library(rpart)

#---------------Create data sets -----------------------------

#load testing and trainin data sets 
training <- read.csv("M:/Analytics Team/Machine Learning/practical Machine Learning/data/pml-training.csv")[, -1]
testing <- read.csv("M:/Analytics Team/Machine Learning/practical Machine Learning/data/pml-testing.csv")[, -1]


# Remove columns with NA or is empty
subTrain <- training[, names(training)[sapply(training, function (x)! (any(is.na(x) | x == "")))]]

subTrain <- subTrain[, names(subTrain)[!(nzv(subTrain, saveMetrics = T)[, 4])]]

#remove columns we dont need 
# x just looks like an ID column
# subTrain$x <- NULL


#read a rumor that a bunch of caret methods run better when target variables are a factor
subTrain$classe <- as.factor(subTrain$classe)

#separate training (again) into training and validation
#Now we will have a validation data set made out of training data. 
#This means we can test against training-validation and testing-testing
set.seed(42)
inTrain <- createDataPartition(subTrain$classe, p = 0.65, list = FALSE)

subTraining <- subTrain[inTrain,]
subValidation <- subTrain[-inTrain,]

#----------------Create MODEL-----------------------------------------

#I'm not sure what method will give the best results so will try a few and take an average  
fitRF <- train(classe ~. , data = subTraining, method  = "rf")
fitGBM <- train(classe ~. , data = subTraining, method  = "gbm", verbose=FALSE)
fitLDA <- train(classe ~. , data = subTraining, method  = "lda")

prRF <- predict(fitRF, subValidation)
prGBM <- predict(fitGBM, subValidation)
prLDA <- predict(fitLDA, subValidation)

#Combine methods together (Also known as stacking)
combo <- data.frame(prRF, prGBM, prLDA, classe = subValidation$classe)

#Take the combine data frame and run a random forest to see if we can boost accuracy 
fitCombo <- train(classe ~. , data = combo, method="rf")


#----------------Validation--------------------------------------------

#use re trained fit
prCombo <- predict(fitCombo, combo)

#test fit 
confusionMatrix(prCombo, subValidation$classe)

#Is this better or worse than just using one method?
print(paste0("RF accuracy = ", confusionMatrix(prRF, subValidation$classe)$overall['Accuracy']))
print(paste0("GBM accuracy = ", confusionMatrix(prGBM, subValidation$classe)$overall['Accuracy']))
print(paste0("LDA accuracy = ", confusionMatrix(prLDA, subValidation$classe)$overall['Accuracy']))
print(paste0("Stacked accuracy = ", confusionMatrix(prCombo, subValidation$classe)$overall['Accuracy']))

varImp(fitCombo)

fitCombo$finalModel

# -----------------------Apply 3 models to testing data-----------------
subTest <- testing[, names(testing)[sapply(testing, function (x)! (any(is.na(x) | x == "")))]]

subTest <- subTest[, names(subTest)[!(nzv(subTest, saveMetrics = T)[, 4])]]

names(subTest)

subTest$x <- NULL

prRF <- predict(fitRF, subTest[, -59])
prGBM <- predict(fitGBM, subTest[, -59])
prLDA <- predict(fitLDA, subTest[, -59])

combo_test <- data.frame(prRF, prGBM, prLDA)


#-----------------Final Apply--------------------------------------

finalPrediction <- predict(fitCombo, combo_test)
finalPrediction
