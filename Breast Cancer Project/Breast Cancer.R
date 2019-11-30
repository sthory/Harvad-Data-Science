# BreastCancer.R
# Author: Eduardo Sthory
# Breast Cancer Wisconsin (Diagnostic) Data Set
# HarvardX - PH125.9x Data Science - Choose your own project

if(!require(tidyverse)) 
  install.packages("tidyverse", 
                   repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", 
                   repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) 
  install.packages("ggplot2", 
                   repos = "http://cran.us.r-project.org")
if(!require(funModeling)) 
  install.packages("funModeling", 
                   repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(ggplot2)
library(funModeling)
library(corrplot)

# Load data
# https://www.kaggle.com/uciml/breast-cancer-wisconsin-data/version/2

# The dataset is in my github in that address: 
# https://github.com/sthory/Harvad-Data-Science/tree/master/Breast%20Cancer%20Project
datacancer = read.csv("data2.csv")  # read csv file 

# Check for missing values
map_int(datacancer, function(.x) sum(is.na(.x)))

# Validation Dataset
# Split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training

# Remove the Id column and 33 number columns, after convert the data to numeric
datacancer <- datacancer[,-1]
datacancer <- datacancer[,-ncol(datacancer)]

# Create dataset of training and test validation
set.seed(1)
validationIndex <- createDataPartition(datacancer$diagnosis, 
                                       p=0.80, 
                                       list=FALSE)

# select 20% of the data for validation
validation <- datacancer[-validationIndex,]

# use the remaining 80% of data to training and testing the models
dataset <- datacancer[validationIndex,]

# Analyze Data
# The objective of this step in the process is to better understand the problem.

## Descriptive Statistics
## Let's start off by confirming the dimensions of the dataset.

# Explore dataset

dim(dataset)

head(dataset)

sapply(dataset, class)

head(dataset)

sapply(dataset, class)

# convert input values to numeric
for(i in 2:ncol(dataset)) {
  dataset[,i] <- as.numeric(as.character(dataset[,i]))
}

# summary
summary(dataset)

# diagnosis distribution
cbind(freq=table(dataset$diagnosis), 
      percentage=prop.table(table(dataset$diagnosis))*100)

# We see the correlation between the attributes.
# summarize correlations between input variables
cor(dataset[,2:ncol(dataset)])

# Correlation plot
correlationMatrix <- cor(dataset[,2:ncol(dataset)])

corrplot(correlationMatrix, 
         method = "color",
         order = "FPC", 
         tl.cex = 0.8) 
         
         #addrect = 8)

# Unimodal Data Visualizations
# This is the distribution of individual attributes with histograms in the data set.

# Graphics values
lineas <- 5
columnas <- 6

# histograms each attribute
par(mfrow=c(lineas,columnas))

for(i in 2:ncol(dataset)) {
  hist(dataset[,i], main=names(dataset)[i])
}
plot_num(dataset %>% select(-diagnosis), bins=10)

# Graphics parameter
lineas <- 1
columnas <- 3

# density plot for each attribute
par(mfrow=c(lineas,columnas))
for(i in 2:ncol(dataset)) {
  plot(density(dataset[,i]), 
       main=names(dataset)[i])
}

"Now we see the distributions using box and whisker plots"

# boxplots for each attribute
par(mfrow=c(lineas,columnas))
for(i in 2:ncol(dataset)) {
  boxplot(dataset[,i], 
          main=names(dataset)[i])
}

# Multimodal Data Visualizations
"Next we will see the interactions between the attributes.
First with a scatterplot matrix of the attributes colored 
by the 'diagnosis' values."

# scatterplot matrix

# mean
pairs(dataset[,2:11], 
      names(dataset[,2:11]), 
      col=dataset$diagnosis)

# Standard error
pairs(dataset[,12:21], 
      names(dataset[,12:21]), 
      col=dataset$diagnosis)

# Worst
pairs(dataset[,22:ncol(dataset)], 
      names(dataset[,22:ncol(dataset)]), 
      col=dataset$diagnosis)

# Evaluating models

"As we do not know which algorithms will work well in this data, 
we will try several models and check which ones are the best."

"We will define how we will do the tests, we will use cross 
validation 10 times with 3 repetitions. Since it is a binary 
diagnosisification problem, we will use 'Accuracy' and 'Kappa'
metrics."

# We define this function as follows:
# 10-fold cross validation with 3 repeats

trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3)
metric <- "Accuracy"

"Now we will create the models that we are going to evaluate, 
we will use the default parameters and then we will consider 
the respective settings.
In order for each algorithm to be evaluated in exactly the same 
data divisions, we need to reset the seed of random numbers 
before training the models."

# Generalized Linear Model
set.seed(1)
fit.glm <- dataset %>%
  train(diagnosis~., 
        data=., 
        method="glm", 
        metric=metric, 
        trControl=trainControl)

# SVM - Support Vector Machines
set.seed(1)
fit.svm <- dataset %>%  
  train(diagnosis~., 
        data=., 
        method="svmRadial", 
        metric=metric,
        trControl=trainControl)

# Linear Discriminant Analysis
set.seed(1)
fit.lda <- dataset %>% 
  train(diagnosis~., 
        data=., 
        method="lda", 
        metric=metric, 
        trControl=trainControl)

# Naive Bayes
set.seed(1)
fit.nb <- dataset %>%  
  train(diagnosis~., 
        data=., 
        method="nb", 
        metric=metric, 
        trControl=trainControl)

# K-nearest Neighbors
set.seed(1)
fit.knn <- dataset %>% 
  train(diagnosis~., 
        data=., 
        method="knn", 
        metric=metric, 
        trControl=trainControl)

# CART - diagnosisification and Regression Trees
set.seed(1)
fit.cart <- dataset %>%  
  train(diagnosis~., 
        data=., 
        method="rpart", 
        metric=metric,
        trControl=trainControl)

# Comparing Models
results <- resamples(list(LG=fit.glm, 
                          SVM=fit.svm,
                          LDA=fit.lda, 
                          NB=fit.nb, 
                          KNN=fit.knn,
                          CART=fit.cart))
summary(results)
dotplot(results)

"It is very possible that we have some skewed distributions, 
we will use the Box-Cox method to adjust and normalize these 
distributions, the data will be transformed using a Box-Cox 
power transformation to flatten the distributions."

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3)
metric <- "Accuracy"

# LG
set.seed(1)
fit.glm <- dataset %>% 
  train(diagnosis~., 
        data = ., 
        method = "glm", 
        metric = metric, 
        preProc = c("BoxCox"),
        trControl = trainControl)

# SVM
set.seed(1)
fit.svm <- dataset %>% 
  train(diagnosis~., 
        data=., 
        method="svmRadial", 
        metric=metric,
        preProc=c("BoxCox"), trControl=trainControl)

# LDA
set.seed(1)
fit.lda <- dataset %>% 
  train(diagnosis~., 
        data=., 
        method="lda", 
        metric=metric, 
        preProc=c("BoxCox"),
        trControl=trainControl)

# Naive Bayes
set.seed(1)
fit.nb <- dataset %>% 
  train(diagnosis~., 
        data=., 
        method="nb", 
        metric=metric, 
        preProc=c("BoxCox"),
        trControl=trainControl)

# KNN
set.seed(1)
fit.knn <- dataset %>%  
  train(diagnosis~., 
        data=., 
        method="knn", 
        metric=metric, 
        preProc=c("BoxCox"),
        trControl=trainControl)

# CART
set.seed(1)
fit.cart <- dataset %>% 
  train(diagnosis~., 
        data=., 
        method="rpart", 
        metric=metric,
        preProc=c("BoxCox"), trControl=trainControl)

# Compare algorithms
transformResults <- resamples(list(LG=fit.glm, 
                                   SVM=fit.svm,
                                   LDA=fit.lda, 
                                   NB=fit.nb,
                                   KNN=fit.knn,
                                   CART=fit.cart))
summary(transformResults)
dotplot(transformResults)

# Model Tuning

"Now, knowing that SVM is the best results 
in our tests, we will make some adjustments to try to 
improve accuracy.
Note: LDA has no parameters to adjust"

# Tuning SVM

# We see which parameters adjust

modelLookup("svmRadial")

"We can adjust 'sigma' and 'C'"

"The SVM model in the 'caret' package has two parameters 
that can be adjusted, these are: 1: sigma which is a 
smoothing term and 2: C which is a cost constraint. 
We will test for C a range of values between 1 and 15. 
For sigma small values, approximately 0.1."

# 10-fold cross validation with 3 repeats

trainControl <- trainControl(method = "repeatedcv", 
                             number=10, 
                             repeats=3)
metric <- "Accuracy"

set.seed(1)

grid <- expand.grid(.sigma = c(0.025, 0.05, 0.1, 0.15),
                    .C = seq(1, 15, by=1))

fit.svm <- dataset %>%
  train(diagnosis~., 
        data = ., 
        method = "svmRadial", 
        metric = metric, 
        tuneGrid = grid,
        preProc = c("BoxCox"), 
        trControl = trainControl)

print(fit.svm)

plot(fit.svm)

fit.svm$bestTune

"ROC curve variable importance"
ggplot(varImp(fit.svm))
varImp(fit.svm)

# Ensemble Methods ----------------------------------------

"A boosting and bagging ensemble algorithm will be applied.
Since the decision tree methods underlie these models, it is possible 
to infer that CART (BAG) and 'Random Forest (RF)' like Bagging and 
'Stochastic Gradient Boosting (GBM)' and C5.0 (C50) like Boosting can 
do an excellent job let's try them and see. 
We will use the same test data as before, including Box-Cox that flattens
the distributions"

# 10-fold cross validation with 3 repeats

trainControl <- 
  trainControl(method = "repeatedcv", 
               number = 10, 
               repeats = 3)


metric <- "Accuracy"

# Bagged CART
set.seed(1)
fit.treebag <- dataset %>% 
  train(diagnosis ~ ., 
        data = ., 
        method = "treebag", 
        metric = metric,
        trControl = trainControl)

# Random Forest
set.seed(1)
fit.rf <- dataset %>% 
  train(diagnosis ~ ., 
        data = ., 
        method = "rf", 
        metric = metric, 
        preProc = c("BoxCox"),
        trControl = trainControl)

# Stochastic Gradient Boosting
set.seed(1)
fit.gbm <- dataset %>% 
  train(diagnosis ~ ., 
        data = ., 
        method = "gbm", 
        metric = metric, 
        preProc = c("BoxCox"),
        trControl = trainControl, 
        verbose = FALSE)

# C5.0
set.seed(1)
fit.c50 <- dataset %>% 
  train(diagnosis ~ ., 
        data = ., 
        method="C5.0", 
        metric = metric, 
        preProc = c("BoxCox"),
        trControl = trainControl)

# Compare results
ensembleResults <- resamples(list(BAG=fit.treebag, 
                                  RF=fit.rf, 
                                  GBM=fit.gbm, 
                                  C50=fit.c50))

"ROC curve variable importance"
varImp(fit.c50)
ggplot(varImp(fit.c50))

summary(ensembleResults)
dotplot(ensembleResults)

# -------------------------------- Final Model -----------------------

"We will finish making the final model, our model with better accuracy 
and Kappa score was the SVM, that's why it will be the final model.
However, we will also test the C5.0 Ensemble Methods (It also performed 
well) to compare its execution with the validation set with the other 
chosen model (SVM).

We are going to capture the parameters of the Box-Cox transformation and
prepare the data, previously we eliminate the unused attributes, and we 
convert all the entries to a numerical format but in the training set, 
now we have to do the same with the validation portion and remove missing 
values (Na)."

# Preparing parameters for data transform

set.seed(1)

preprocessParams <- preProcess(dataset, 
                               method = c("BoxCox"))

x <- predict(preprocessParams, dataset)

"Now we will proceed to prepare the validation data set, for this we will 
eliminate the attributes that we will not use, convert all the input 
attributes to numerical and finally we will apply the Box-Cox 
transformation to the input attributes using the parameters prepared in
training data set. Also remove Na values."

# prepare parameters for data transform
set.seed(1)
datasetNoMissing <- dataset[complete.cases(dataset),]
x <- datasetNoMissing[,2:11]
preprocessParams <- preProcess(x, method=c("BoxCox"))
x <- predict(preprocessParams, x)

# Preparing the validation dataset --------------------------------
set.seed(1)

# remove missing values (not allowed in this implementation of knn)
validation <- validation[complete.cases(validation),]

# convert to numeric
for(i in 2:ncol(validation)) {
  validation[,i] <- as.numeric(as.character(validation[,i]))
}

# transform the validation dataset
validationX <- predict(preprocessParams, 
                       validation[,2:ncol(validation)])


# Run final models

# First, we test with the C5.0 model (was the best in train)
library(C50)
set.seed(1)

# Tuning some parameters that previously did not tuning.
C5.0.Grid <-expand.grid(interaction.depth = c(1,5,9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minosinnode =20)

# Train model
c5model <- C5.0(x = dataset[,-1], 
                  y = dataset$diagnosis,
                  preProc = c("BoxCox"),
                  Grid =C5.0.Grid,
                  tuneLength=30,
                  trControl = trainControl)

# Model C5.0 predictions with the validation set
predictions <- predict.C5.0(c5model, validationX)

# Confusion Matrix for C5.0 model
confusionMatrix(as.factor(predictions), 
                validation$diagnosis)


# Model SVM predictions with validation set
# This is previously tuning
set.seed(1)
predictions <- predict(fit.svm, validation[,-1])

# Confusion Matrix for SVM model
confusionMatrix(as.factor(predictions), 
                validation$diagnosis)
