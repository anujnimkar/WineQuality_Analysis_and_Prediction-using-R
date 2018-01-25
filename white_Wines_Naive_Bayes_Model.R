
# load the libraries
library(caret)
library(klaR)
  
# load the dataset
setwd('F:/Anuj/Study & Work/Sem 2 Courses/INST 737/Final Project')
wineWhites <- read.csv('wineQualityWhites.csv',sep=',') 

# define an 80%/20% train/test split of the dataset
trainIndex <- createDataPartition(wineWhites$quality, p=0.80, list=FALSE)
data_train <- wineWhites[ trainIndex,]
data_test <- wineWhites[-trainIndex,]

# train a naive bayes model
model <- NaiveBayes(factor(quality)~., data=data_train)

# make predictions
predictions <- predict(model, data_test[,1:13])

# summarize results
confusionMatrix(predictions$class, data_test$quality) 



