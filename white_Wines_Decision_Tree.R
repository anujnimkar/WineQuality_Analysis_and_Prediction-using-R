---
  title: "White_Wines"
author: "Anuj Nimkar"
date: "Monday, April 27, 2015"
output: html_document
---
### Setting the work directory and loading in the train data
  
  ```{r}
setwd('F:/Anuj/Study & Work/Sem 2 Courses/INST 737/Final Project') 
trainData <- read.csv('wineWhites_trainData.csv', sep = ',')
testData <- read.csv('wineWhites_testData.csv', sep = ',')

```
 
### Cleaning the data and ensuring data consistency

```{r}
trainData <- na.omit(trainData)

trainData <- subset(trainData, 
                    (trainData$fixed.acidity >= 0.0 & trainData$fixed.acidity <= 10.0) &   
                      (trainData$volatile.acidity >= 0.0 & trainData$volatile.acidity <= 1.0) &
                      (trainData$citric.acid >= 0.0 & trainData$citric.acid <=1.0) &
                      (trainData$residual.sugar >= 0.0 & trainData$residual.sugar <= 100.0) &
                      (trainData$chlorides >= 0.0 & trainData$chlorides <= 1.0) &
                      (trainData$free.sulfur.dioxide >= 0.0 & trainData$free.sulfur.dioxide <= 100.0  ))

```


### Decision tree

```{r}
install.packages('caret')
library(tree)
library(rpart)
library(caret) 

rt <- rpart(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,  minsplit=10, data=trainData, method="anova")

testData <- testData[complete.cases(testData),]

testData$quality <- predict(rt,testData)

write.csv(testData,file = "white_Wines_test_Data_Predictions.csv", row.names = FALSE)

confusionMatrix(quality.pred,testData$quality) 


```






