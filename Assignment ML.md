---
title: "Assignment Practice Machine Learning"
author: "KELGIN"
date: "May 21, 2017"
output: html_document
---

##Introduction
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

##Data
Training: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

Testing: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

**Dependency**
```r
library(caret)
library(randomForest)
library(rpart)
library(gbm)
```

##Import Data

```r
Train <- read.csv("pml-training.csv", na.strings=c("NA",""), strip.white=T)

Valid <- read.csv("pml-testing.csv", na.strings=c("NA",""), strip.white=T)

Partition <- createDataPartition(y=Train$classe,
                               p=0.7, list=FALSE)
Training <- Train[Partition,]
Testing <- Train[-Partition,]
dim(Training); dim(Testing)
```

##Data Cleansing

Remove variables that contained mostly NA

```{r message=FALSE, warning=FALSE}
NA_Value    <- sapply(Training, function(x) sum(is.na(x))) > 0.95
Training <- Training[, NA_Value==FALSE]
Testing  <- Testing[, NA_Value==FALSE]
dim(Training); dim(Testing)
```

Remove identity variables from first seven columns
```{r message=FALSE, warning=FALSE}
Training <- Training[, -(1:7)]
Testing  <- Testing[, -(1:7)]
dim(Training) ; dim(Testing)
```

##Model Building
Three models will be run to compare the accuracy.

**Random Forest**
```{r message=FALSE, warning=FALSE}
set.seed(100)
cv <- trainControl(method="cv", number=3)
model_rf <- train(classe~ . , data = Training, method="rf", trControl=cv, verbose=F)
predict_rf <- predict(model_rf, Testing)
confusionMatrix(predict_rf,Testing$classe)
```

**Decision Tree**
```{r message=FALSE, warning=FALSE}
set.seed(100)
model_tree <- train(classe~ . , data = Training, method="rpart")
predict_tree <- predict(model_tree, Testing)
confusionMatrix(predict_tree,Testing$classe)
```

**Gradient Boosting Model**
```{r message=FALSE, warning=FALSE}
set.seed(100)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
model_gbm <- train(classe~ . , data = Training, method="gbm",
                    trControl = controlGBM, verbose = FALSE)
```
```{r message=FALSE, warning=FALSE}
predict_gbm <- predict(model_gbm, Testing)
confusionMatrix(predict_gbm,Testing$classe)
```

Comparison of models accuracy
```{r message=FALSE, warning=FALSE}
confusionMatrix(predict_rf, Testing$classe)$overall[1]
confusionMatrix(predict_tree, Testing$classe)$overall[1]
confusionMatrix(predict_gbm, Testing$classe)$overall[1]
```

**Conclusion**

Random forest shows the highest accuracy, 0.99, while decision Tree shows the lowest accuracy, 0.495. Random forest is the most preferable model with highest accuracy. Random forest and gradient boosting model use the longest time to run, as such number variables can be fine tune to improve the performance. 

Below are the 20 most importants to predict classe.
```{r message=FALSE, warning=FALSE}
varImp(model_rf)
```
